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

vdbConn("ebola_zoonosis", name="Ebola Zoonosis")

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

#makeDisplay(df_by_journal,
#            name="pcp_journal",
#            group="Journal",
#            width = 300, height = 300,
#            desc = "Documents by Journal in PCP Space",
#            panelFn = proj_doc_panel_gen(),
#            cogFn=proj_doc_cog_fun)

#makeDisplay(df_by_publication_type,
#            name="pcp_publication_type",
#            group="Publication Type",
#            width = 300, height = 300,
#            desc = "Documents by Publication Type in PCP Space",
#            panelFn = proj_doc_panel_gen(),
#            cogFn=proj_doc_cog_fun)

