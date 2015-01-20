library(datadr)
library(trelliscope)
library(itertools)
library(foreach)

source("doc_project.r")
source("panels.r")
options("repos"="http://cran.rstudio.com/")

db_path = "trelliscope_database"
name = "Database"

registerDoSEQ()

clean_up_date_string <- function(ds) {
  ret <- gsub("NULL", "", ds)
  gsub("[ ]+", " ", ret)
}

create_html_caption <- function(doc_title, author, date, journal) {
  str <- paste("<b>%s</b><table>",
    "<tr><td align='left'><b>Author:</b></td><td>%s</td></tr>",
    "<td align='left'><b>Date:</b></td><td>%s</td></tr>",
    "<td align='left'><b>Journal:</b></td><td>%s</td></tr></table>")
  sprintf(str,
    clean_up_entries(doc_title, width=40),
    clean_up_entries(author),
    clean_up_date_string(date),
    clean_up_entries(journal))
}

clean_up_entry <- function(entry, max_lines=3, width=30) {
  entry_vector <- strwrap(entry, width=width)
#  entry_vector = enc2native(entry)
  too_long = which(nchar(entry_vector) > width)
  for(tl in too_long) {
    s = substr(entry_vector[tl], 1, width)
    entry_vector[tl] = paste(s, "...", sep="")
  }
  if (length(entry_vector) > max_lines) {
    entry_vector <- entry_vector[1:max_lines]
    entry_vector[max_lines] <- paste(entry_vector[max_lines], "...", sep="")
  }
  paste(entry_vector, collapse="<br>")
}

clean_up_entries <- function(entries, max_lines=3, width=30) {
  foreach (entry=entries, .combine=c) %do% {
    clean_up_entry(entry, max_lines, width)
  }
}


# Note that while R can handle UTF-8 it looks like the tooltips in highcharts 
# cannot.
df = read.csv("msr-data.csv", as.is=TRUE)
# Get the last 4 years
df = df[df$year == "1981" | df$year == "1982" | 
        df$year == "1983" | df$year == "2012" | 
        df$year == "2014" | df$year == "2013",]
#df = df[df$year == "2008" | df$year == "2009" | df$year == "2010" | 
#        df$year == "2011",]

# Sample 300
df = df[sample.int(nrow(df), 500),]
rownames(df) = NULL
df$abstract= gsub('"', "", df$abstract)
df$abstract= gsub("'", "", df$abstract)
df$title_and_abstract = paste(df$title, df$abstract)
df$title = clean_up_entries(df$title, 2, 30)

df$author = gsub("\\[", "", df$author)
df$author = gsub("]", "", df$author)

authors = strsplit(df$author, ",")
cleaned_authors = Map(function(x) {
         if(length(x) > 1)
           x = paste(x[1], "et al")
         else
           x = x[1]
         x
       }, authors)
df$author = clean_up_entries(unlist(cleaned_authors), 1, 10)
df$author = enc2native(df$author)
df$title = enc2native(df$title)
df$html_caption = create_html_caption(df$title, df$author, df$year, 
                                      rep("MSR", nrow(df)))
df$html_caption = enc2native(df$html_caption)
df = df[,-1]

vdbConn("memex_msr", name="Memex MSR", autoYes=TRUE)

df$all = as.factor(1)
df_by_all = divide(df, by="all", update=TRUE)
df_by_year = divide(df, by="year", update=TRUE)

proj_doc_panel_gen = function(color=NULL, components=2:3) {
  color=color
  components=components
  function(x) { 
    ret = NULL
    x_name = paste("pc", components[1], sep="")
    y_name = paste("pc", components[2], sep="")
    if (nrow(x) > 3) {
      pd = project_into_document_space(x$title_and_abstract, 
        components=components, jitter=FALSE)
      x = cbind(x[rownames(pd),], as.matrix(pd))
      names(x)[tail(1:ncol(x), length(components))] = c(x_name, y_name)
      x = x[,-which(names(x) == "title_and_abstract")]
      x = x[,-which(names(x) == "abstract")]
      x = x[,-which(names(x) == "title")]
      ret = scatter_plot_with_url(x, x_name=x_name, y_name=y_name, by=color)
    } else if (nrow(x) == 2 || nrow(x) == 3) {
      x[x_name] = c(-0.5, 0.5)
      x[y_name] = c(0, 0)
      ret = scatter_plot_with_url(x, x_name=x_name, y_name=y_name, by=color)
    } else if (nrow(x) == 1) {
      x[x_name] = 0
      x[y_name] = 0
      ret = scatter_plot_with_url(x, x_name=x_name, y_name=y_name, by=color)
      if (inherits(ret, "try-error")) {
        print(ret)
        ret = NULL
      }
    } else {
      ret = NULL #print(x)
      #stop("Problem with input")
    }
    ret
  }
}

#df_temp = df[,c("url", "year", "html_caption", "title_and_abstract")]
#proj_doc_panel_gen("year", components=2:3)(df_temp)

proj_doc_cog_fun = function(x) {
  list(num_documents=cog(nrow(x), desc="Number of documents"))
}

components = 2:3

try(makeDisplay(df_by_all,
            name="pcp_all",
            group="All",
            width = 400, height = 300,
            desc = "Documents_in_PCP_Space",
            panelFn = proj_doc_panel_gen("year", components=components),
            cogFn=proj_doc_cog_fun))

try(makeDisplay(df_by_year,
            name="pcp_by_year",
            group="All",
            width = 400, height = 300,
            desc = "Documents_in_PCP_Space_by_Year",
            panelFn = proj_doc_panel_gen(NULL, components=components),
            cogFn=proj_doc_cog_fun))

view()
