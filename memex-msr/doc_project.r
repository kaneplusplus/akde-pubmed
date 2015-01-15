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

project_into_document_space = function(x, components, jitter=FALSE) {
  UseMethod("project_into_document_space", x)
}

project_into_document_space.default <- function(x, components=1:2, 
  jitter=FALSE) {
  cat("components are", components, "\n")
  pcp = irlba(x, nv=max(components), tol=1e-5, dU=rep(1,nrow(x)), 
              ds=1, dV=colMeans(x))
  ret = t(x) %*% pcp$u
  ret[,components[1]] = ret[,components[1]] - mean(ret[,components[1]])
  ret[,components[2]] = ret[,components[2]] - mean(ret[,components[2]])
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
    components=components, jitter=jitter)
}


