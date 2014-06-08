require(XML)
require(itertools)
require(foreach)

query_ids <- function(query) {
  esearch=paste('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmode=xml&retmax=10000000&term=', query, sep="")
  html_doc <- htmlParse(esearch)
  ids <- xmlSApply(getNodeSet(html_doc, "//idlist//id"), xmlValue)
}

get_author_list <- function(element) {
  lasts <- xmlSApply(getNodeSet(element, ".//authorlist//author//lastname"),
    xmlValue)
  initials <- xmlSApply(getNodeSet(element, ".//authorlist//author//initials"),
    xmlValue)
  firsts <- xmlSApply(getNodeSet(element, ".//authorlist//author/forename"),
    xmlValue)
  names <- paste(firsts, initials, lasts)
  if (length(names) > 3) {
    names <- paste(names[1], "et al.")
  } else {
    names <- paste(names, collapse=" and ")
  }
  names
}

get_publication_type <- function(element) {
  paste(xmlSApply(getNodeSet(element, ".//publicationtype"), xmlValue),
        collapse=",")
}

get_journal <- function(element) {
  xmlSApply(getNodeSet(element, ".//journal//title"), xmlValue)[1]
}

get_date <- function(element) {
  year <- xmlSApply(getNodeSet(element, 
    ".//journal//journalissue//pubdate//year"), xmlValue)[1]
  month <- xmlSApply(getNodeSet(element, 
    ".//journal//journalissue//pubdate//month"), xmlValue)[1]
  day <- xmlSApply(getNodeSet(element, 
    ".//journal//journalissue//pubdate//day"), xmlValue)[1]
  paste(month, day, year)
}

pm_doc_info <- function(query, max_ids=Inf, verbose=FALSE, chunkSize=200) {
  ids <- query_ids(query)
  if (verbose)
    cat(length(ids), "ID's found\n")
  foreach(it=isplitIndices(min(length(ids), max_ids), chunkSize=chunkSize), 
          .combine=rbind) %do% {
    if (verbose)
      cat(it[1], ":", it[length(it)], "\n", sep="")
    id_string <- paste(ids[it], sep="", collapse=",")
    abstract_query <- paste('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?&db=pubmed&retmode=xml&id=', id_string, sep="")
    doc <- htmlParse(abstract_query)
    foreach(element=getNodeSet(doc, "//medlinecitation/article"), pmid=ids[it],
            .combine=rbind) %do% {
      url <- paste("http://www.ncbi.nlm.nih.gov/pubmed/?term=",
        pmid, "%5Buid%5D", sep="")
      abstract <- xmlSApply(getNodeSet(element, ".//abstracttext"), xmlValue)
      if (length(abstract) > 1) 
        abstract <- paste(abstract, collapse=" ")
      title <- xmlSApply(getNodeSet(element, ".//articletitle"), xmlValue)
      if (length(title) > 1) 
        title <- paste(title, collapse=" ")
      # We'll only take articles with titles and abstracts.
      title_and_abstract <- ""
      if (length(abstract) > 0 && length(title) > 0)
        title_and_abstract <- paste(title, abstract)
      author <- get_author_list(element)
      publication_type <- get_publication_type(element)
      journal <- get_journal(element)
      date <- get_date(element)
      data.frame(list(title=title, author=author, date=date, journal=journal,
        publication_type=publication_type, url=url,
        title_and_abstract=title_and_abstract), stringsAsFactors=FALSE)
    }
  }
}

pm_title_abstracts <- function(query, max_ids=Inf, verbose=FALSE,
                               chunkSize=200) {
  pm_doc_info(query, max_ids, verbose, chunkSize)$title_and_abstract
}

init_pm_query_cache <- function(host="localhost", port=6379, 
                                password=NULL, returnRef=FALSE, 
                                nodelay=FALSE, timeout=2678399L,
                                expire_time=3600) {
  require(rredis)
  redisConnect(host=host, port=port, password=password, returnRef=returnRef,
               nodelay=nodelay, timeout=timeout)
  options(expire_time=expire_time)
}

pm_query <- function(query, max_ids=Inf, verbose=FALSE, chunkSize=200) {
  ret <- NULL
  cache_exists <- try(
    # Does the query exist in Redis?
    if (redisExists(query)) {
      if (verbose)
        cat("Query found in Redis\n")
      # Are there at least max_ids in the query?
      cached_obj <- redisGet(query)
      if (cached_obj$max_ids >= max_ids) {
        # The cached object is sufficient.
        ret <- cached_obj$query_result[1:min(max_ids, 
                                       nrow(cached_obj$query_result)),]
      } else {
        if (verbose)
          cat("Redis did not have enough articles\n")
      }
    }, silent=TRUE)
  have_redis <- !inherits(cache_exists, "try-error")
  if (is.null(ret)) {
    # We need to hit Pubmed.
    ret <- pm_doc_info(query=query, max_ids=max_ids, verbose=verbose,
                       chunkSize=chunkSize)
    if (have_redis) {
      if (verbose)
        cat("Query not found in Redis. Going to Pubmed...\n")
      redisSet(query, list(max_ids=max_ids, query_result=ret))
    }
  }
  if (have_redis) {
    redisExpireAt(query, 
                  as.integer(as.POSIXct(Sys.time())+options()$expire_time))
  }
  ret
}
