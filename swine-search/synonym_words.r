library(itertools)
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
