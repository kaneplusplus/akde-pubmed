library(iotools)
library(datadr)
library(trelliscope)
library(lubridate)
library(xts)
library(tm)

source("../../ebola_vis/ts.r")
source("../../ebola_vis/panels.r")

# TODO's
# 1. Combine the data sets.
# 2. Create histograms by year for each of them.
# 3. Create histograms by month for each of them.
# 4. Define the text analysis.

# Make the g77 data.
g77_fh = file("../data/g77_statements.csv", "r")
g77_data = dstrsplit(readLines(g77_fh), col_types=rep("character", 4), sep=",")
names(g77_data) = c("type", "date", "url", "text")
g77_data$date = format(strptime(g77_data$date, "%m-%d-%Y"), "%Y-%m-%d")

# The url is not quite right in url http://www.g77.org/Speeches/0207006.htm.
# Confirm row 607 
bad_date_rows = grep("\\d{7}", g77_data$url)
if (length(bad_date_rows) > 0) {
  cat("Bad dates at row(s):", paste(bad_date_rows), "\n")
  g77_data = g77_data[-bad_date_rows,]
}

# Make the UN data.
un_types = c("economic-and-social-council", "secretary-general",
             "security-council", "general-assembly")
un_files_with_path = foreach(un_type=un_types, .combine=c) %do% {
  file.path("../data", paste(un_types, "csv", sep='.'))
}

un_data = foreach (un_file=un_files_with_path, type=un_types, 
  .combine=rbind) %do% {
  fh = file(un_file, "r")
  x = dstrsplit(readLines(fh), col_types=rep("character", 3), sep=",")
  close(fh)
  names(x) = c("date", "url", "text")
  x$type = type
  x
}

x = rbind(un_data, g77_data)
x$type[x$type == "speech" | x$type == "statement"] = "G77"

x$year = year(strptime(x$date, format="%Y-%m-%d"))
x$month = month(strptime(x$date, format="%Y-%m-%d"))

x = x[x$year >= 2010,]

x$year.month = paste(x$year, sprintf("%02d", x$month), sep='.')
x$nchar = nchar(x$text)

# Get the stemmed words.
corpus = Corpus(VectorSource(x$text))
corpus = tm_map(corpus, content_transformer(tolower))
 
corpus = tm_map(corpus, content_transformer(removePunctuation))
corpus = tm_map(corpus, content_transformer(removeWords), stopwords("english"))
corpus = tm_map(corpus, content_transformer(removeNumbers))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, content_transformer(stripWhitespace))
x$stems = unlist(Map(function(x) x$content, corpus))
#tdm = TermDocumentMatrix(corpus)

shorten_names = function(n) {
  n[n == "economic-and-social-council"] = "easc"
  n[n == "secretary-general"] = "sg"
  n[n == "security-council"] = "sc"
  n[n == "general-assembly"] = "easc"
}

source("volume.r")
