library(iotools)
library(datadr)
library(trelliscope)
library(lubridate)
library(xts)

source("../../ebola_vis/ts.r")
source("../../ebola_vis/panels.r")

# TODO's
# 1. Combine the data sets.
# 2. Create histograms by year for each of them.
# 3. Create histograms by month for each of them.
# 4. Define the text analysis.

file_contents = scan("../data/g77_statements.csv", what=character(), sep="\n")
x = dstrsplit(file_contents, col_types=rep("character", 4), sep=",")
names(x) = c("type", "url", "date", "text")

# The url is not quite right in url http://www.g77.org/Speeches/0207006.htm.
# Confirm row 607 
bad_date_rows = grep("\\d{7}", x$url)
if (length(bad_date_rows) > 0) {
  cat("Bad dates at row(s):", paste(bad_date_rows), "\n")
}
if (length(bad_date_rows) == 1 && bad_date_rows == 607) {
  x$date[607] = "02-07-2006"
} else {
  error("Bad date code needs to be updated")
}

x$year = year(strptime(x$date, "%m-%d-%Y", tz="EST"))

vdbConn("g77", name="G77 Text Visualization", autoYes=TRUE)
#x_by_year = divide(x, by="year", update=TRUE)

x_year_vol = foreach(y=unique(x$year), .combine=rbind) %do% {
  data.frame(year=y, count=nrow(x[x$year == y,]))
}

x_date = xts(rep(1, length(x$date)), 
             order.by=strptime(x$date, "%m-%d-%Y"))
xd = apply.monthly(x_date, length)
xdt = time(xd)
xmv = as.data.frame(xd)
#xmv$year_month = year(xdt) + (month(xdt)-1)/12
xmv$year_month = xdt
xmv= as.data.frame(xmv)
names(xmv)[1] = "count"
xmv$year_month = as.character(xmv$year_month)
mPlot(y='count', x='year_month', data=xmv, type="Line")

x_text_date = xts(x$text,
             order.by=strptime(x$date, "%m-%d-%Y"), unique=FALSE)
ep = endpoints(x_text_date, on="months")
xtd = period.apply(x_text_date, INDEX=ep,
  FUN=function(x) sum(nchar(as.character(x)))/sum(length(as.character(x))))
xtdt = time(xtd)
xtmv = as.data.frame(xtd)
xtmv$year_month = xtdt
xtmv= as.data.frame(xtmv)
names(xtmv)[1] = "count"
xtmv$year_month = as.character(xtmv$year_month)
mPlot(y='count', x='year_month', data=xtmv, type="Line")

x_year_vol = x_year_vol[order(x_year_vol$year),]
x_year_vol$all = factor(1)

time_hist = function(form, group=NULL) {
  form = form
  group = group
  function(x) {
    if (!is.null(group))
      ret = nPlot(form, group=group, data=x, type='multiBarChart')
    else
      ret = nPlot(form, data=x, type='multiBarChart')
    ret
  }
}

x_year_vol_by_all = divide(x_year_vol, by="all", update=TRUE)
makeDisplay(x_year_vol_by_all,
            name="yearly_volume",
            group="G77",
            width=350, height=200,
            desc="Statement and Speech Volume by Year",
            panelFn= time_hist(count ~ year))

