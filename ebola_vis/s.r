library(datadr)
library(trelliscope)
library(itertools)
library(foreach)

source("pm_query.r")
source("doc_project.r")
source("panels.r")

registerDoSEQ()

queries =c("ebola AND monkey", 
           "ebola AND bats", 
           "ebola AND rodent", 
           "ebola AND dog", 
           "ebola AND pig", 
           "ebola AND human", 
           "ebola AND gorilla",
           "ebola AND chimpanzee")
label_name = "species"
labels = c("monkey", "bat", "rodent", "dog", "pig", 
      "human", "gorilla", "chimpanzee")

cat("Querying PubMed.\n")
df = create_pm_query_df(queries, label_name, labels)
df$species[multiple_inds(df$id)] = "multiple"
df = df[!duplicated(df$id),]
df$species = as.factor(df$species)
df$year = year(make_date_time(df$date_string, "years"))
df$month = month(make_date_time(df$date_string, "month"))
df$day = day(make_date_time(df$date_string, "day"))

vdbConn("ebola_zoonosis", name="Ebola Zoonosis")

df$all = as.factor(1)
df_by_all = divide(df, by="all", update=TRUE)
df_by_species = divide(df, by="species", update=TRUE)
df_by_journal = divide(df, by="journal", update=TRUE)
df_by_publication_type = divide(df, by="publication_type", update=TRUE)
#df_by_year = divide(df, by="year", update=TRUE)


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
      ret = scatter_plot_with_url(x, x_name="pc1", y_name="pc2", by=color)
      #ret = scatter_plot_with_url(pc2 ~ pc1, x, xlab="pc1", ylab="pc2", 
      #  color=color)
    }
    ret
  }
}

proj_doc_cog_fun = function(x) {
  list(num_documents=cog(nrow(x), desc="Number of documents"))
}

doc_count_panel_gen = function(resolution="years", group=NULL) {
  resolution=resolution
  group=group
  function(x) {
    ac = get_article_counts(x, resolution)
    acdf = as.data.frame(ac)
    res = NULL
    # TODO: Create x axis values from the 
    if (resolution != "years")
      stop("Only year resolutions are supported so far.")
    acdf$date = year(time(ac))
    if (is.null(group))
      res = nPlot( count ~ date, data=acdf, type="lineChart")
    else
      res = nPlot( count ~ date, date=acdf, group=group, type="lineChart")
    res
  }
}

makeDisplay(df_by_all,
            name="pcp_species_all",
            group="All",
            width = 300, height = 300,
            desc = "Documents by Species in PCP Space",
            panelFn = proj_doc_panel_gen(color="species"),
            cogFn=proj_doc_cog_fun)

# Too many journals.
#makeDisplay(df_by_all,
#            name="pcp_journal_all",
#            group="All",
#            width = 300, height = 300,
#            desc = "Documents by Journal in PCP Space",
#            panelFn = proj_doc_panel_gen(color="journal"),
#            cogFn=proj_doc_cog_fun)

makeDisplay(df_by_species,
            name="pcp_species",
            group="Species",
            width = 300, height = 300,
            desc = "Documents by Species in PCP Space",
            panelFn = proj_doc_panel_gen(),
            cogFn=proj_doc_cog_fun)

makeDisplay(df_by_journal,
            name="pcp_journal",
            group="Journal",
            width = 300, height = 300,
            desc = "Documents by Journal in PCP Space",
            panelFn = proj_doc_panel_gen(),
            cogFn=proj_doc_cog_fun)

makeDisplay(df_by_journal,
            name="pcp_species_by_journal",
            group="Journal",
            width = 300, height = 300,
            desc = "Documents by Journal in PCP Space",
            panelFn = proj_doc_panel_gen("species"),
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

