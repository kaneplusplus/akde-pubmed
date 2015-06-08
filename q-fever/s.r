library(datadr)
library(itertools)
library(foreach)
library(rbokeh)

source("pm_query.r")
source("doc_project.r")
#source("panels.r")
#source("ts.r")

#db_path = "trelliscope_database"
#name = "Database"

registerDoSEQ()

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

clean_up_entry <- function(entry, max_lines=3, width=30, collapse="<br>") {
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
  paste(entry_vector, collapse=collapse)
}

clean_up_entries <- function(entries, max_lines=3, width=30, collapse="<b>") {
  foreach (entry=entries, .combine=c) %do% {
    clean_up_entry(entry, max_lines, width, collapse=collapse)
  }
}

queries = c('("Coxiella burnetii" OR Q-fever) AND Bovine AND Susceptibility',
            '("Coxiella burnetii" OR Q-fever) AND Ovine AND Susceptibility',
            '("Coxiella burnetii" OR Q-fever) AND Caprine AND Susceptibility',
            '("Coxiella burnetii" OR Q-fever) AND Murine AND Susceptibility',
            '("Coxiella burnetii" OR Q-fever) AND Human AND Susceptibility')

label_name = "Species"
labels = c("Cattle", "Sheep", "Goat", "Mice", "Humans")

cat("Querying PubMed.\n")
df = create_pm_query_df(queries, label_name, labels, max_docs_per_query=250)
df[[label_name]][multiple_inds(df$id)] = "Multiple"
df = df[!duplicated(df$id),]
df[[label_name]] = as.factor(df[[label_name]])
df$year = year(make_date_time(df$date_string, "years"))
df$month = month(make_date_time(df$date_string, "month"))
df$day = day(make_date_time(df$date_string, "day"))

df$html_caption = create_html_caption(df$title, df$author, df$date_string, 
                                      df$journal)
df$title_short = clean_up_entries(df$title, collapse="\n")

x = project_into_document_space(df$title_and_abstract, components=2:3)
df$PC2 = x[,1]
df$PC3 = x[,2]

p = figure() %>% ly_points(PC2, PC3, data=df, color=Species, 
                       hover=df[,c("title", "author", "journal", "year")],
                       url=df$url) 
p



