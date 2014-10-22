library(iotools)

file_contents = scan("../data/g77_statements.csv", what=character(), sep="\n")
x = dstrsplit(file_contents, col_types=rep("character", 4), sep=",")
names(x) = c("type", "url", "date", "text")

# The url is not quite right in url http://www.g77.org/Speeches/0207006.htm.
# Confirm row 607 
bad_date_rows = grep("\\d{7}", x$url)
if (length(bad_date_row) > 0) {
  cat("Bad dates at row(s):", paste(bad_date_rows), "\n")
}

if (length(bad_date_rows) == 1 && bad_date_rows == 607)
  x$date[607] = "02-07-2006"
else
  error("Bad date code needs to be updated")

x$date = strptime(x$date, "%m-%d-%Y", tz="EST")
