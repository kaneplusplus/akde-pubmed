library(rCharts)
library(httr)
library(RJSONIO)
library(ldatools)
library(tm)
library(xts)
library(lubridate)
library(mallet)

#source("dc.r")

# TODO: start here
topic_view <- function(docs, num_topics) {
  lda_fit <- run_lda(docs, num_topics)
  
}

topic_viz <- function(docs, num_topics) {
  fit <- run_lda(docs, num_topics)
  doc.id <- fit@wordassignments$i
  token.id <- fit@wordassignments$j
  topic.id <- fit@wordassignments$v

  #vocab <- fit@terms
  assign("vocab", fit@terms, .GlobalEnv)

  # Get the phi matrix using LDAviz
  dat <- getProbs(token.id, doc.id, topic.id, vocab, K = max(topic.id), 
                  sort.topics = "byTerms")
  # phi <- t(dat$phi.hat)
  assign("phi", t(dat$phi.hat), .GlobalEnv)

  # term.frequency <- as.numeric(table(token.id))
  assign("term.frequency", as.numeric(table(token.id)), .GlobalEnv)
  topic.id <- dat$topic.id

  #topic.proportion <- as.numeric(table(topic.id)/length(topic.id))
  assign("topic.proportion", as.numeric(table(topic.id)/length(topic.id)),
         .GlobalEnv)

  # Run the visualization locally using LDAvis
  z <- check.inputs(K=max(topic.id), W=max(token.id), phi, term.frequency, 
                    vocab, topic.proportion)
  runVis()
}

clean_up_date_string <- function(ds) {
  ret <- gsub("NULL", "", ds)
  gsub("[ ]+", " ", ret)
}

clean_up_entry <- function(entry, max_lines=3, width=30) {
  entry_vector <- strwrap(entry, width=width)
  if (length(entry_vector) > max_lines) {
    title_vector <- entry_vector[1:max_lines]
    entry_vector[max_lines] <- paste(entry_vector[max_lines], "...", sep="")
  }
  paste(entry_vector, collapse="<br>")
}

clean_up_entries <- function(entries, max_lines=3, width=30) {
  foreach (entry=entries, .combine=c) %do% {
    clean_up_entry(entry, max_lines, width)
  }
}

create_html_caption <- function(doc_title, author, date, journal) {
  str <- paste("<b>%s</b><table>",
    "<tr><td align='left'><b>Author:</b></td><td>%s</td></tr>",
    "<td align='left'><b>Date:</b></td><td>%s</td></tr>",
    "<td align='left'><b>Journal:</b></td><td>%s</td></tr></table>")
  sprintf(str, 
    clean_up_entries(doc_title, width=40), 
    clean_up_entries(author), 
    clean_up_date_string(date), 
    clean_up_entries(journal))
}

highcharts_viz <- function(data, x_name="x", y_name="y", by="", xlab="", 
                           ylab="", title="", 
                           legend_title="", subtitle="") {
  names(data)[match(c(x_name, y_name), names(data))] <- c("x", "y")

  data_list <- lapply(split(data, data[,by]), function(x) {
    res <- lapply(split(x, rownames(x)), as.list)
    names(res) <- NULL
    res
  })

  viz <- rCharts::Highcharts$new()
  invisible(sapply(data_list, function(x) {
      apply_cat <- eval(parse(text=paste("x[[1]]$", by, sep="")))
      viz$series(data=x, type="scatter", name=apply_cat) 
    }))

  viz$plotOptions(
    scatter = list(
      cursor = "pointer", 
      point = list(
        events = list(
          click = "#! function() { window.open(this.options.url); } !#")), 
      marker = list(
        symbol = "circle", 
        radius = 5
      )
    )
  )

  viz$xAxis(title = list(text=xlab), 
    labels = list(format="{value} "))
  viz$yAxis(title = list(text=ylab), 
    labels = list(format="{value} "))
  viz$tooltip(useHTML=TRUE,
    formatter="#! function() { return this.point.name; } !#")
 
  viz$legend(
    align = 'right', 
    verticalAlign = 'middle', 
    layout = 'vertical', 
    title = list(text=legend_title)
  )
  viz$title(text=title)
  viz$subtitle(text=subtitle) 
  viz$chart( zoomType = 'xy' )
  viz
}

get_session_id <- function() {
  if (exists("session"))
    as.character(session$clientData$url_hostname)
  else
    "127.0.0.1"
}

get_docs_and_procs <- function(query, max_ids=Inf, verbose=FALSE) {
  session_id <- get_session_id()
  doc_key <- paste(session_id, query, "docs")
  doc_proc_key <- paste(session_id, query, "doc_proc")
  max_id_key <- paste(session_id, query, "max_id_key")
  if (verbose) {
    cat("Session id is", session_id, "\n", 
        "doc_key is", doc_key, "\n",
        "doc_proc_key is", doc_proc_key, "\n",
        "max_id_key is", max_id_key, "\n")
  }
  if (redisExists(doc_key) && redisExists(doc_proc_key) && 
      redisGet(max_id_key) == max_ids) {
    if (verbose)
      cat("Docs and doc procs found in Redis.\n")
    docs <- redisGet(doc_key)
    doc_proc <- redisGet(doc_proc_key)
  } else {
    if (verbose)
      cat("Docs and doc procs not found in Redis.\n")
    docs <- pm_query(query, max_ids, verbose=verbose)
    doc_proc <- preprocess(data=docs$title_and_abstract, 
      stopwords=stopwords(), stem=TRUE)
    if (any(doc_proc$category != 0)) {
      docs <- docs[doc_proc$category == 0,]
      doc_proc <- preprocess(data=docs$title_and_abstract, 
        stopwords=stopwords(), stem=TRUE)
    }
    redisSet(max_id_key, max_ids)
    redisSet(doc_key, docs)
    redisSet(doc_proc_key, doc_proc)
  }
  ret <- list("docs"=docs, "doc_proc"=doc_proc)
  redisExpireAt(doc_key, 
    as.integer(as.POSIXct(Sys.time())+options()$expire_time))
  redisExpireAt(doc_proc_key, 
    as.integer(as.POSIXct(Sys.time())+options()$expire_time))
  redisExpireAt(max_id_key, 
    as.integer(as.POSIXct(Sys.time())+options()$expire_time))
  ret
}

# This could be done slighly better by caching the entire document basis.
get_proj_docs <- function(doc_proc, query, max_ids, components, verbose=FALSE) {
  proj_doc_key <- paste(get_session_id(), query, max_ids, 
                       paste(components, collapse= " "))
  if (verbose)
    cat("proj_doc_key is", proj_doc_key, "\n")
  if (redisExists(proj_doc_key)) {
    proj_docs <- redisGet(proj_doc_key)
  } else {
    tdm <- create_tdm(doc_proc)
    document_basis <- irlb_lsa(tdm, dims=max(components))$tk

    components <- 1:2
    proj_docs <- t(tdm) %*% document_basis[,components]

    if (sum(proj_docs[,1] > 0) < nrow(proj_docs))
      proj_docs <- -1 * proj_docs

    redisSet(proj_doc_key, proj_docs)
  }
  redisExpireAt(proj_doc_key,
    as.integer(as.POSIXct(Sys.time())+options()$expire_time))
  proj_docs
}

memoize <- function(expr, key=NULL, expire_time=Inf, verbose=FALSE, 
                    envir=parent.frame()) {
  if (is.null(key)) {
    key <- paste(substitute(expr), collapse="")
  }
  if (redisExists(key)) {
    ret <- redisGet(key)
  } else {
    ret <- eval(substitute(expr), envir=envir)
    redisSet(key, ret)
  }
  if (expire_time < Inf) {
    redisExpireAt(proj_doc_key,
      as.integer(as.POSIXct(Sys.time())+expire_time))
  }
  ret
}

create_viz <- function(query, max_ids, num_clusters, cluster_algo="lsa", 
  components=1:2, verbose=FALSE, ...) {

  cat("getting docs and procs\n")
  l <- get_docs_and_procs(query, max_ids, verbose=verbose)
  doc_proc <- l$doc_proc
  docs <- l$docs

  if (cluster_algo == "lsa") {
    cluster_obj <- memoize(
      run_lsa(doc_proc, num_topics=num_clusters, ...),
      paste(get_session_id(), query, max_ids, "lsa", num_clusters))
  } else if (cluster_algo == "lda") {
    cluster_obj <- memoize(
      run_lda(doc_proc, num_topics=num_clusters, ...),
      paste(get_session_id(), query, max_ids, "lda", num_clusters))
  } else {
    stop("Unsupported clustering algorithm")
  }

  proj_docs <- get_proj_docs(doc_proc, query, max_ids, components, 
                             verbose=verbose)
  
  num_docs <- length(docs$url)
  if (verbose)
    cat("Number of documents is", num_docs, "\n")
  data <- data.frame(list(x=proj_docs[1:num_docs,1], y=proj_docs[1:num_docs,2], 
    cluster=cluster_obj$doc_cluster$cluster[1:num_docs], url=docs$url), 
    stringsAsFactors=FALSE)

  data$name <- create_html_caption(docs$title, docs$author, docs$date, 
    docs$journal)

  highcharts_viz(data, by="cluster")
}


get_article_counts <- function(docs, resolution="months") {
  date_matrix <- matrix( unlist(strsplit(docs$date, " ")), ncol=3, byrow=TRUE )
  if (resolution == "months") {
    date_matrix[,2] <- "1"
  } else if (resolution == "years") {
    date_matrix[,1] <- "Jan"
  } else if (resolution != "days") {
    stop("Unsupported time resolution")
  }
  date_strings <- apply(date_matrix, 1, paste, collapse=" ")
  d <- strptime(date_strings, format="%b %d %Y")

  if (resolution == "years")
    d <- d + years(1) - days(1)
  if (resolution == "months")
    d <- d + months(1) - days(1)

  if (any(is.na(d))) {
    warnings(paste(length(d) - length(d[!is.na(d)]), "dates could not be",
                   "constructed out of", length(d)))
  }
  d <- d[!is.na(d)]
  ret <- d
  if (length(d) > 0) {
    article_dates <- xts(rep(1, length(d)), order.by=d, unique=FALSE)
    tz(index(article_dates)) <- "GMT"
    vol <- period.apply(article_dates, endpoints(article_dates, resolution), 
                        length)
    ret <- vol
    names(ret) <- "count"
    #ret <- data.frame(date=as.character(index(vol)), count=as.vector(vol),
    #                  stringsAsFactors=FALSE)
    #ret$title <- ret$date
  #  ret$annotation <- ret$date
  } else {
    warning("Dates could not be resolved at specified resolution")
  }
  ret
}

# A corpus-context summary shows the publication frequency and the associated
# stemmed words.
# TODO: change this to use dimple
plot_article_activity <- function(query, max_ids, resolution="years", 
                                verbose=TRUE) {
  if (resolution != "years")
    stop("Resolutions other than years are not supported yet")
  docs <- pm_query(query, max_ids, verbose=verbose)
  article_counts <- get_article_counts(docs, resolution=resolution)
  # This is not right. The dates are being turned into integers. However,
  # I don't have an internet connection now to look up how to fix it.
  acdf <- as.data.frame(article_counts)

  acdf$date <- year(time(article_counts))
  nPlot(count ~ date, data=acdf, type="lineChart")
}

# A journal context summary shows the histogram frequency 
make_journal_table_data_frame <- function(docs, max_num_journals, format=TRUE,
                                          truncate=FALSE) {
  journal_table <- table(docs$journal)
  journal_table <- journal_table[order(journal_table, decreasing=TRUE)]
  if (max_num_journals < length(journal_table)) {
    journal_table_tail <- sum(
      journal_table[max_num_journals:length(journal_table)])
    journal_table <- journal_table[1:max_num_journals]
    journal_table[max_num_journals] <- journal_table_tail
    names(journal_table)[max_num_journals] <- "Other journals"
  }
  if (format) {
    names(journal_table) <- foreach(n=names(journal_table), .combine=c) %do% {
      paste(strwrap(n, width=width), collapse="\n")
    }
  }
  d <- data.frame(list(y=as.vector(journal_table), x=names(journal_table)))
  names(d) <- c("count", "journal")
  if (truncate) d <- d[-nrow(d),]
  d
}

# TODO: Waiting on a fix in rCharts so that journal names can be more than
# one line of text.
journal_hist <- function(query, max_ids, max_num_journals=10, verbose=FALSE, 
                         width=20, truncate=FALSE) {
  docs <- pm_query(query, max_ids, verbose=verbose)
  d <- make_journal_table_data_frame(docs, max_num_journals, truncate=truncate)
  d$journal <- substr(d$journal, 1, 11)
  nPlot(count ~ journal, data=d, type="discreteBarChart")
}

journal_article_activity_ts <- function(query, max_ids, max_num_journals, 
                                        verbose=FALSE, truncate=FALSE) {
  docs <- pm_query(query, max_ids, verbose=verbose)
  journal_table <- make_journal_table_data_frame(docs, max_num_journals, 
                                                 truncate=truncate, 
                                                 format=FALSE)
  docs$journal[!(docs$journal %in% journal_table$journal)] <- "Other journals"
  # TODO: Again, this needs to be fixed.
  tsl <- foreach(journal=unique(docs$journal)) %do% {
    df <- get_article_counts(docs[docs$journal==journal,])
    list(df, journal)
  }
  # Get the tsl elements that ave at least one journal entry.
  these <- which(unlist(Map(function(x) length(x[[1]]) > 0, tsl)))
  journal_names <- unique(docs$journal)[these]
  journal_counts <- foreach(jc=tsl[these], .combine=merge) %do% {
    jc[[1]]
  }
  names(journal_counts) <- journal_names
  journal_counts[is.na(journal_counts)] <- 0
  ret <- as.data.frame(journal_counts)
  ret$date <- rownames(ret)
  ret <- melt(ret, variable.name="date")
  ret$date <- strptime(ret$date, "%Y-%m-%d")
  names(ret) <- c("date", "journal", "count")
  nPlot(count ~ date, data=ret, group="journal", type="lineChart")
}

