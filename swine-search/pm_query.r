library(rentrez)
library(XML)
library(lubridate)
library(foreach)

get_author_list <- function(element) {
  lasts <- xmlSApply(getNodeSet(element, ".//AuthorList//Author//Lastname"),
    xmlValue)
  initials <- xmlSApply(getNodeSet(element, ".//AuthorList//Author//Initials"),
    xmlValue)
  firsts <- xmlSApply(getNodeSet(element, ".//AuthorList//Author/ForeName"),
    xmlValue)
  names <- paste(firsts, initials, lasts)
  if (length(names) > 3) {
    names <- paste(names[1], "et al.")
  } else {
    names <- paste(names, collapse=" and ")
  }
  names
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

make_date_time = function(date_strings, resolution="months") {
  date_matrix <- matrix(unlist(strsplit(date_strings, " ")), ncol=3, 
                        byrow=TRUE )
  if (resolution == "months") {
    date_matrix[,2] <- "1"
  } else if (resolution == "years") {
    date_matrix[,1] <- "Jan"
    date_matrix[,2] = "1"
  } #else if (resolution != "days") {
  #  stop("Unsupported time resolution")
  #}
  date_strings <- apply(date_matrix, 1, paste, collapse=" ")
  d <- strptime(date_strings, format="%b %d %Y", tz="GMT")

#  if (resolution == "years")
#    d <- d + years(1) - days(1)
#  if (resolution == "months")
#    d <- d + months(1) - days(1)
  d
}

get_publication_type <- function(element) {
  paste(xmlSApply(getNodeSet(element, ".//PublicationType"), xmlValue),
        collapse=",")
}

get_journal <- function(element) {
  xmlSApply(getNodeSet(element, ".//Journal//Title"), xmlValue)[1]
}

get_date <- function(element) {
  year <- xmlSApply(getNodeSet(element,
    ".//Journal//JournalIssue//PubDate//Year"), xmlValue)[1]
  month <- xmlSApply(getNodeSet(element,
    ".//Journal//JournalIssue//PubDate//Month"), xmlValue)[1]
  day <- xmlSApply(getNodeSet(element,
    ".//Journal//JournalIssue//PubDate//Day"), xmlValue)[1]
  paste(month, day, year)
}

data_frame_from_entrez <- function(term, max_docs=Inf, verbose=FALSE, 
    chunk_size=500) {
  term_search <- entrez_search(db="pubmed", term=term, retmax=max_docs)
  term_search$count = as.numeric(term_search$count)
  if (verbose) {
    cat("Query: ", term, "\n", sep="")
    cat("Total documents returned by query: ", term_search$count, "\n",
        sep="")
    cat("Number of documents to fetch: ", min(term_search$count, max_docs),
        "\n", sep="")
  }
  ret = foreach (it=isplitIndices(min(term_search$count, max_docs), 
      chunkSize=chunk_size), .combine=rbind) %dopar% {
    if (verbose) {
      cat(paste("Fetching documents: ", it[1], ":", it[length(it)], "\n", 
                sep=""))
    }
    term_fetch <- entrez_fetch(db="pubmed", id=term_search$ids, rettype="XML")
    doc <- xmlParse(term_fetch)
    foreach(element=getNodeSet(doc, "//PubmedArticle"), id=term_search$ids,
            .combine=rbind) %do% {
      url <- paste("http://www.ncbi.nlm.nih.gov/pubmed/?term=",
        id, "%5Buid%5D", sep="")
      abstract <- xmlSApply(getNodeSet(element, ".//AbstractText"), xmlValue)
      if (length(abstract) > 1)
        abstract <- paste(abstract, collapse=" ")
      if (length(abstract) < 1)
        abstract=""
      title <- xmlSApply(getNodeSet(element, ".//ArticleTitle"), xmlValue)
      if (length(title) > 1)
        title <- paste(title, collapse=" ")
      # We'll only take articles with titles and abstracts.
      title_and_abstract <- ""
      if (length(abstract) > 0 && length(title) > 0)
        title_and_abstract <- paste(title, abstract)
      author <- get_author_list(element)
      if (length(author) == 0)
        author=""
      publication_type <- get_publication_type(element)
      if (length(publication_type) == 0)
        publication_type = "Unknown"
      journal <- get_journal(element)
      if (length(journal) == 0)
        journal="Unknown"
      date_string <- get_date(element)
      if (length(date_string) == 0)
        date_string = "NULL NULL NULL"
      data.frame(list(id=id, title=title, author=author, 
        date_string=date_string, abstract=abstract, journal=journal, 
        publication_type=publication_type, url=url, 
        title_and_abstract=title_and_abstract), stringsAsFactors=FALSE)
    }
  }
  if (verbose)
    cat("\n")
  ret
}

create_pm_query_df = function(queries, label_name, labels=NULL, verbose=TRUE,
  max_docs_per_query=100) {
  ret = NULL
  if (length(queries) > 1) {
    if (is.null(labels))
      labels = queries
    if (length(labels) != length(queries))
      stop("Queries and query labels must be the same length")
    ret = foreach(i=1:length(queries)) %do% { 
      df = data_frame_from_entrez(queries[i], max_docs=max_docs_per_query,
        verbose=verbose)
      eval(parse(text=paste("df$", label_name, " = '", labels[i], "'", sep="")))
      df
    }
    these = unlist(lapply(ret, function(x) !is.null(nrow(x))))
    ret = Reduce(rbind, ret[these])
  } else {
    ret = data_frame_from_entrez(queries, max_docs=max_docs_per_query,
      verbose=verbose)
  }
  if(!is.null(ret) > 0) {
    ret$html_caption = create_html_caption(ret$title, ret$author, 
      clean_up_date_string(ret$date_string), ret$journal)
  }
  ret
}

multiple_inds = function(x) {
  tb = table(x)
  dup_label = names(tb[tb > 1])
  which(x %in% dup_label)
}


