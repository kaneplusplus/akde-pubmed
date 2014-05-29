library(tm)
library(ldatools)
library(LDAviz)
library(Matrix)
library(irlba)
library(foreach)

run_lda <- function(doc_proc, num_topics, num_iter=1000, ...) {
  fit <- LDAviz::fitLDA(word.id=doc_proc$token.id, doc.id=doc_proc$doc.id, 
                k=num_topics, n.iter=num_iter)
  doc_prob <- LDAviz::getProbs(word.id=doc_proc$token.id, 
                               doc.id=doc_proc$doc.id, topic.id=fit$topics, 
                               vocab=doc_proc$vocab, sort.topics="byDocs", 
                               K=num_topics)
  doc_cluster <- data.frame(list(doc_id=unique(doc_proc$doc.id),
                                 cluster=doc_prob$main.topic))
  ret <- list("doc_cluster"=doc_cluster, "doc_proc"=doc_proc)
  class(ret) <- "lda_cluster"
  ret
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

irlb_lsa <- function(x, dims=round(min(dim(x))*.1)) {
  space <- irlba(x, nv=dims, nu=dims)
  names(space)[c(2, 3, 1)] <- c("tk", "dk", "sk")
  rownames(space$tk) = rownames(x)
  rownames(space$dk) = colnames(x)
  class(space) = c(class(space), "LSAspace")
  space
}

project_into_document_space <- function(x, components=1:2) {
  ret <- t(x) %*% irlb_lsa(x, dims=max(components))$tk
  ret[,components]
}

project_into_word_space <- function(x, components=1:2) {
  space <- irlba(x, nv=components, nu=components)
  ret <- t(x) %*% irlb_lsa(x, dims=max(components))$dk
  ret[,components]
}

create_tdm <- function(doc_proc, sparse=TRUE) {
  if (sparse) {
    tdm <- Matrix(0, nrow=max(doc_proc$token.id), ncol=max(doc_proc$doc.id))
  } else {
    tdm <- matrix(0, nrow=max(doc_proc$token.id), ncol=max(doc_proc$doc.id))
  }
  rownames(tdm) <- doc_proc$vocab
  colnames(tdm) <- 1:max(doc_proc$doc.id)
  # This should be done row-wise
  tvs <- foreach(doc_id=unique(doc_proc$doc.id)) %dopar% {
    t <- table(doc_proc$token.id[doc_proc$doc.id==doc_id])
    rv <- sparseVector(x=as.vector(t), i=as.integer(names(t)), length=nrow(tdm))
  }
  for (j in 1:ncol(tdm)) {
    tdm[,j] <- tvs[[j]]
  }
  tdm
}

run_lsa <- function(doc_proc, num_topics, sparse.tdm=TRUE, use.irlb=TRUE, ...) {
  tdm <- tfidf(create_tdm(doc_proc, sparse=sparse.tdm), log.scale=TRUE)
  if (use.irlb)
    lsa_space <- irlb_lsa(tdm, ...)
  else
    lsa_space <- lsa(tdm)
  projected_documents <- t(tdm) %*% lsa_space$tk
  km <- kmeans(projected_documents, centers=num_topics)
  doc_cluster <- data.frame(list(doc_id=unique(doc_proc$doc.id),
                                  cluster=km$cluster))
  ret <- list("doc_cluster"=doc_cluster, "doc_proc"=doc_proc, "space"=lsa_space)
  class(ret) <- "lsa_cluster"
  ret
}

comparison_matrix <- function(cluster1, cluster2) {
  num_clusters <- length(unique(cluster1))
  if (num_clusters != length(unique(cluster2)))
    stop("Cluster sizes mismatch")

  cmat <- matrix(NA, nrow=num_clusters, ncol=num_clusters)
  for (i in 1:num_clusters) {
    for (j in 1:num_clusters) {
      cmat[i, j] <- sum( cluster1 == i & cluster2 == j)
    }
  }
  cmat
}


bench <- function(query="rift+valley+fever", num_topics=6, verbose=TRUE,
                  use.irlb=FALSE, doc_cutoff=5000, ...) {
  if (verbose)
    cat("Retrieving documents from PubMed\n")
  docs <- pm_title_abstracts(query, verbose=verbose, max_ids=doc_cutoff)
  if (verbose)
    cat("Grabbed", length(docs), "documents\n")
  doc_proc <- ldatools::preprocess(data=docs, stopwords=stopwords(), stem=TRUE)
  if (any(doc_proc$category != 0)) {
    docs <- docs[doc_proc$category == 0]
    doc_proc <- ldatools::preprocess(data=docs, stopwords=stopwords(), 
                                     stem=TRUE)
  }
  if (verbose)
    cat(length(docs), "documents left after preprocessing\n")

  lda_timing <- system.time({lda_cluster <- 
    run_lda(doc_proc, num_topics=num_topics)})
  lsa_timing <- system.time({lsa_cluster <- 
    run_lsa(doc_proc, num_topics=num_topics, use.irlb=TRUE, ...)})
  if (verbose) {
    cat("lda timing\n")
    print(lda_timing)
    cat("lsa timing\n")
    print(lsa_timing)
  }

  lda_clusters <- lda_cluster[[1]]$cluster
  lsa_clusters <- lsa_cluster[[1]]$cluster

  # Rows will be the python lda topics and columns will the R topics.
  cmat <- comparison_matrix(lda_clusters, lsa_clusters)
  list("lda_table"=table(lda_clusters), 
       "lsa_table"=table(lsa_clusters),
       "lda_vs_lsa"=cmat)
}

# check this way and the transpose of this way.
register_clusters <- function(cluster1, cluster2) {
  cmat <- comparison_matrix(cluster1, cluster2)
  new_cluster2 <- cluster2
  for (i in 1:nrow(cmat)) {
    old_col_max <- which.max(cmat[i,])
    new_cluster2[cluster2 == old_col_max] <- i
    cmat[,old_col_max] <- -Inf
  }
  list("cluster1"=cluster1, "cluster2"=new_cluster2)
}

#bench(query='brucellosis', num_topics=10, use.irlb=TRUE, doc_cutoff=10000, 
#      dims=40)
