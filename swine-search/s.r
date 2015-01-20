library(datadr)
library(trelliscope)
library(itertools)
library(foreach)

source("pm_query.r")
source("doc_project.r")
source("panels.r")
source("ts.r")

db_path = "trelliscope_database"
name = "Database"

registerDoSEQ()

queries = c('(piglet (growth OR weight OR cognitive)) AND ("dietary supplement" OR supplementation) NOT obesity',
'((((child OR infant) (growth OR weight OR cognitive)) AND ("dietary supplement" OR supplementation) AND ((developing country))) NOT obesity')

label_name = "species"
labels = c("piglet", "child")

cat("Querying PubMed.\n")
df = create_pm_query_df(queries, label_name, labels, max_docs_per_query=500)
df[[label_name]][multiple_inds(df$id)] = "multiple"
df = df[!duplicated(df$id),]
df[[label_name]] = as.factor(df[[label_name]])
df$year = year(make_date_time(df$date_string, "years"))
df$month = month(make_date_time(df$date_string, "month"))
df$day = day(make_date_time(df$date_string, "day"))

vdbConn("piglet_child_zoonosis", name="Piglet Child Zoonosis")

df$all = as.factor(1)
df_by_all = divide(df, by="all", update=TRUE)
df_by_label = divide(df, by=label_name, update=TRUE)
df_by_journal = divide(df, by="journal", update=TRUE)
df_by_publication_type = divide(df, by="publication_type", update=TRUE)

# Get rid of the bad years for dividing.
bad_year_ind = which(is.na(df$year))

# Not sure why this dies.
df_by_year = try({divide(df[-bad_year_ind,], 
                         by="year", update=TRUE)}, silent=TRUE)
# and why the following works.
if (inherits(df_by_year, "try-error")) {
  warning('Divide on year failed. Resorting to the "pub_year" hack.')
  df$pub_year = df$year
  df_by_year = divide(df[-bad_year_ind,], by="pub_year", update=TRUE)
}


proj_doc_panel_gen = function(color=NULL, components=2:3) {
  color=color
  components=components
  function(x) { 
    ret = NULL
    if (nrow(x) > 2) {
      pd = project_into_document_space(x$title_and_abstract, 
        components=components, jitter=FALSE)
      x = cbind(x[rownames(pd),], as.matrix(pd))
      names(x)[tail(1:ncol(x), length(components))] = 
        paste("pc", components, sep="")
      x = x[,-which(names(x) == "title_and_abstract")]

      ret = scatter_plot_with_url(x, x_name="pc2", y_name="pc3", by=color)
    } else if (nrow(x) == 2) {
      x$pc1 = c(-0.5, 0.5)
      x$pc2 = c(0, 0)
      ret = scatter_plot_with_url(x, x_name="pc2", y_name="pc3", by=color)
    } else if (nrow(x) == 1) {
      x$pc1 = 0;
      x$pc2 = 0
      ret = scatter_plot_with_url(x, x_name="pc2", y_name="pc3", by=color)
    } else {
      print(x)
      stop("Problem with input")
    }
    ret
  }
}

proj_doc_cog_fun = function(x) {
  list(num_documents=cog(nrow(x), desc="Number of documents"))
}

ts_df = create_ac_ts(df, group=label_name)
ts_df$all = 1
ts_year_by_all = divide(ts_df, by="all", update=TRUE)
ts_year_by_species = divide(ts_df, by="species", update=TRUE)
components = 2:3

try(makeDisplay(df_by_all,
            name=paste("pcp", label_name, "all", sep="_"),
            group="All",
            width = 400, height = 300,
            desc = "Documents_and_Species_in_PCP_Space",
            panelFn = proj_doc_panel_gen(color=label_name, components=components),
            cogFn=proj_doc_cog_fun))

try(makeDisplay(df_by_label,
            name=paste("pcp", label_name, sep="_"),
            group="Species",
            width = 400, height = 300,
            desc = "Documents by Species in PCP Space",
            panelFn = proj_doc_panel_gen(components=components),
            cogFn=proj_doc_cog_fun))

try(makeDisplay(df_by_journal,
            name=paste("pcp", label_name, "by_journal", sep="_"),
            group="Journal",
            width = 400, height = 300,
            desc = "Documents by Journal in PCP Space",
            panelFn = proj_doc_panel_gen(label_name, components=components),
            cogFn=proj_doc_cog_fun))
      
try(makeDisplay(df_by_publication_type,
            name=paste("pcp", label_name, "by_publication_type", sep="_"),
            group="Documents_and_Species_by_Publication_Type",
            width = 400, height = 300,
            desc = "Documents by Publication Type in PCP Space",
            panelFn = proj_doc_panel_gen(label_name, components=components),
            cogFn=proj_doc_cog_fun))

try(makeDisplay(df_by_year,
            name=paste("pcp", label_name, "by_publication_year", sep="_"),
            group="Documents_and_Species_by_Publication_Year",
            width = 400, height = 300,
            desc = "Documents and Species by Year",
            panelFn = proj_doc_panel_gen(label_name, components=components),
            cogFn=proj_doc_cog_fun))

try(makeDisplay(ts_year_by_all,
            name=paste("ts", label_name, "all", sep="_"),
            group="All",
            width=400, height=300,
            desc = "Publication Activity by Year",
            panelFn = group_ts_plot_gen(count ~ date, group="species")))

try(makeDisplay(ts_year_by_species,
            name=paste("ts", label_name, "by_species", sep="_"),
            group="All",
            width=400, height=300,
            desc = "Publication Activity and Species by Year",
            panelFn = ts_plot_gen(count ~ date)))

