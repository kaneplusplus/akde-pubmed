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


