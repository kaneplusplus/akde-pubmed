library(foreach)
library(tm)
library(Matrix)
library(irlba)

source("pm_query.r")

text_to_tdm = function(doc_vec, sparse=TRUE) {
  corpus = Corpus(VectorSource(doc_vec))
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, stripWhitespace)
  corpus = corpus[!unlist(lapply(corpus, function(x) x$content == ""))]
  tdm = TermDocumentMatrix(corpus,
                     control=list(stemming = TRUE, stopwords = TRUE,
                     minWordLength = 3, removeNumbers = TRUE,
                     removePunctuation = TRUE))
  col_totals = apply(tdm , 2, sum)
  tdm = tdm[,col_totals > 0]
  x = Matrix(tdm, nrow=nrow(tdm), ncol=ncol(tdm))
  rownames(x) = rownames(tdm)
  colnames(x) = colnames(tdm)
  x
}

tfidf <- function(x, log.scale) {
  UseMethod("tfidf", x)
}

tfidf.default <- function(x, log.scale=TRUE) {
  df <- as.vector(apply(x, 1, function(x) sum(x > 0)))
  if (log.scale)
   log(x+1) * (log2(ncol(x)/df) + 1)
  else
   x * ncol(x)/df
}

tfidf.sparseMatrix <- function(x, log.scale=TRUE) {
  df <- as.vector(apply(x, 1, function(x) sum(x > 0)))
  if (log.scale) {
    x@x <- log(x@x+1)
    x * (log2(ncol(x)/df) + 1)
  }
  else
    x * ncol(x)/df
}

project_into_document_space = function(x, components=1:2, jitter=FALSE) {
  UseMethod("project_into_document_space", x)
}

project_into_document_space.default <- function(x, components=1:2, 
  jitter=FALSE) {
  pcp = irlba(x, nv=max(components), tol=1e-5, dU=rep(1,nrow(x)), 
              ds=1, dV=colMeans(x))
  ret = t(x) %*% pcp$u
  ret = ret[,components]
  if (jitter) {
    for (j in 1:ncol(x))
      x[,j] = jitter(x[,j])
  }
  rownames(ret) = colnames(x)
  ret
}

project_into_document_space.character <- function(x, components=1:2,
  jitter=FALSE) {
  project_into_document_space.default(tfidf(text_to_tdm(x)), 
    jitter=jitter)
}


library(rCharts)

scatter_plot_with_url <- function(form, x, xlab, ylab, color=NULL) {
  char_form = as.character(form)
  tmp <- x[,c(char_form[-1], "title_short", "author", "date_string_clean", 
    "journal_short", color, "url")]

  # polycharts is dumb and bounds the data points exactly
  # which masks parts of some of the plotting points
  # we need some whitespace around the edges
  getLims <- function(x, frac = 0.07) {
    xRange <- range(x, na.rm = TRUE)
    xRange + c(-1, 1) * diff(xRange) * frac
  }

  xlim <- getLims(x[,char_form[3]])
  ylim <- getLims(x[,char_form[2]])
  tooltip_string = paste("#!function(item){ return item.title_short",
                         "'Author: ' + item.author +",
                         "\n'Date: ' + item.date_string_clean + ",
                         "'Journal: ' + item.journal_short }!#", sep=" + ")

  if (is.null(color)) {
    p <- rPlot(form, data = tmp, type = 'point', size = list(const=3),      
      tooltip = tooltip_string)
  } else {
    p <- rPlot(form, data = tmp, type = 'point', size = list(const=3),      
      color='species', tooltip = tooltip_string)
  }
  p$guides(
    x = list(title = xlab, min = xlim[1], max = xlim[2]),
    y = list(title = ylab, min = ylim[1], max = ylim[2]))
  p$set(dom = 'chart1')
  p$setTemplate(afterScript = "
    <script>
    graph_chart1.addHandler(function(type, e){
       var data = e.evtData
       if (type === 'click') {
          var url = data.url.in[0]
          window.open(url, '_blank')
       }
    })
    </script>    
  ")
  p
}

library(rentrez)
library(XML)
library(lubridate)

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


make_date_time = function(date_strings, resolution="months") {
  date_matrix <- matrix(unlist(strsplit(date_strings, " ")), ncol=3, 
                        byrow=TRUE )
  if (resolution == "months") {
    date_matrix[,2] <- "1"
  } else if (resolution == "years") {
    date_matrix[,1] <- "Jan"
  } else if (resolution != "days") {
    stop("Unsupported time resolution")
  }
  date_strings <- apply(date_matrix, 1, paste, collapse=" ")
  d <- strptime(date_strings, format="%b %d %Y", tz="GMT")

  if (resolution == "years")
    d <- d + years(1) - days(1)
  if (resolution == "months")
    d <- d + months(1) - days(1)
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


data_frame_from_entrez <- function(term) {
  term_search <- entrez_search(db="pubmed", term=term)
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
    publication_type <- get_publication_type(element)
    journal <- get_journal(element)
    date_string <- get_date(element)
    data.frame(list(id=id, title=title, author=author, date_string=date_string, 
      abstract=abstract, journal=journal, publication_type=publication_type, 
      url=url, title_and_abstract=title_and_abstract), stringsAsFactors=FALSE)
  }
}

create_pm_query_df = function(queries, label_name, labels=NULL) {
  if (is.null(labels))
    labels = queries
  if (length(labels) != length(queries))
    stop("Queries and query labels must be the same length")
  foreach(i=1:length(queries), .combine=rbind) %do% { 
    df = data_frame_from_entrez(queries[i])
    df[[label_name]] = labels[i]
    df
  }
}

multiple_inds = function(x) {
  tb = table(x)
  dup_label = names(tb[tb > 1])
  which(x %in% dup_label)
}


library(datadr)
library(trelliscope)

source("pm_query.r")
source("doc_project.r")
source("panels.r")

queries =c("ebola AND monkey", 
           "ebola AND bats", 
           "ebola AND porcupine", 
           "ebola AND rodent", 
           "ebola AND dog", 
           "ebola AND pig", 
           "ebola AND human", 
           "ebola AND gorilla",
           "ebola AND chimpanzee")
label_name = "species"
labels = c("monkey", "bat", "porcupine", "rodent", "dog", "pig", 
      "human", "gorilla", "chimpanzee")

cat("Querying PubMed.\n")
df = create_pm_query_df(queries, label_name, labels)
df$species[multiple_inds(df$id)] = "multiple"
df = df[!duplicated(df$id),]
df$species = as.factor(df$species)
df$day = as.factor(1) #is.na(make_date_time(df$date_string, "days")))
df$month = as.factor(1) #is.na(make_date_time(df$date_string, "months")))
df$year = as.factor(1) #is.na(make_date_time(df$date_string, "years")))

df$title_short = clean_up_entries(df$title)
df$journal_short = clean_up_entries(df$journal)
df$date_string_clean = clean_up_date_string(df$date_string)

vdbConn("ebola_zoonosis", name="Ebola Zoonosis", autoYes=TRUE)

df$all = as.factor(1)
df_by_all = divide(df, by="all", update=TRUE)
df_by_species = divide(df, by="species", update=TRUE)
df_by_journal = divide(df, by="journal", update=TRUE)
df_by_publication_type = divide(df, by="publication_type", update=TRUE)

proj_doc_panel_gen = function(color=NULL) {
  color=color
  function(x) { 
    ret = NULL
    if (nrow(x) > 1) {
      pd = project_into_document_space(x$title_and_abstract, components, 
        jitter=TRUE)
      components=1:2
      x = cbind(x[rownames(pd),], as.matrix(pd))
      names(x)[tail(1:ncol(x), length(components))] = 
        paste("pc", components, sep="")
      #plot(x$pc1, x$pc2, col=as.factor(x$species))
      ret = scatter_plot_with_url(pc2 ~ pc1, x, xlab="pc1", ylab="pc2", 
        color=color)
    }
    force(ret)
  }
}

proj_doc_cog_fun = function(x) {
  list(num_documents=cog(nrow(x[,]), desc="Number of documents"))
}

makeDisplay(df_by_all,
            name="pcp_all",
            group="All",
            width = 300, height = 300,
            desc = "Documents by Species in PCP Space",
            panelFn = proj_doc_panel_gen(color="species"),
            cogFn=proj_doc_cog_fun)

makeDisplay(df_by_species,
            name="pcp_species",
            group="Species",
            width = 300, height = 300,
            desc = "Documents by Species in PCP Space",
            panelFn = proj_doc_panel_gen(),
            cogFn=proj_doc_cog_fun)

view()
