library(itertools)
library(wordnet)
library(Matrix)
source("pm_query.r")
source("doc_project.r")

piglet_query = '(piglet (growth OR weight OR cognitive)) AND ("dietary supplement" OR supplementation) NOT obesity'
piglet_df = create_pm_query_df(piglet_query, max_docs_per_query=500)
piglet_word_freq= apply(text_to_tdm(piglet_df$title_and_abstract), 1, sum)

child_query = '((((child OR infant) (growth OR weight OR cognitive)) AND ("dietary supplement" OR supplementation) AND ((developing country))) NOT obesity'
child_df = create_pm_query_df(child_query, max_docs_per_query=500)
child_word_freq = apply(text_to_tdm(child_df$title_and_abstract), 1, sum)

piglet_not_child_word = setdiff(names(piglet_word_freq), 
                                names(child_word_freq))
piglet_words = sort(piglet_word_freq[piglet_not_child_word], decreasing=TRUE)
x = data.frame(list(piglet_words=names(piglet_words), frequency=piglet_words)) 
rownames(x) = NULL
write.csv(x, "piglet-query-words.csv")

child_not_piglet = setdiff(names(child_word_freq),
                           names(piglet_word_freq))
child_words = sort(child_word_freq[child_not_piglet], decreasing=TRUE)
x= data.frame(list(child_words=names(child_words), frequency=child_words))
rownames(x) = NULL
write.csv(x, "child-query-words.csv")

# Get the wordnet words for each of the names.

get_synonyms = function(x) {
  unique(foreach(pos=c("ADJECTIVE","ADVERB","NOUN","VERB"), .combine=c) %do% {
    filter <- getTermFilter("ExactMatchFilter", x, TRUE)
    ret = NULL
    try({
      suppressWarnings({
        terms <- getIndexTerms(pos, Inf, filter)
        ret = getSynonyms(terms[[1]])
      })
    }, silent=TRUE)
    ret
  })
}

child_word_syn = foreach(word=names(child_words)) %do% {
  unique(c(word, get_synonyms(word)))
}

piglet_word_syn = foreach(word=names(piglet_words)) %do% {
  unique(c(word, get_synonyms(word)))
}

bridge_mat = matrix(data=0, nrow=length(child_word_syn), 
                ncol=length(piglet_word_syn))

# Note that we're taking the wordnet words from the child word synonyms
# comparing them with the piglet words.
library(doMC)
registerDoMC(cores=6)
bridge_mat = foreach (i=1:nrow(bridge_mat), .combine=rbind) %dopar% {
  if (i %% 100 == 0) print(i)
  unlist(Map(function(x) 
    length(intersect(child_word_syn[[i]], x)), as.list(names(piglet_words))))
#                length(intersect(child_word_syn[[i]], x)), piglet_word_syn))
}

non_zero_rows = which(as.vector(apply(bridge_mat, 1, sum) > 0))
bridge_map = NULL
for (i in non_zero_rows) {
  inds = which(bridge_mat[i,] > 0)
  if (length(inds) > 0) {
    bridge_map=rbind(bridge_map, 
                     data.frame(child=rep(names(child_words)[i], length(inds)), 
                                piglet=names(piglet_words)[inds]))
  }
}

write.csv(bridge_map, "bridge_map1.csv")
