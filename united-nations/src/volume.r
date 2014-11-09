
count_by_year = foreach(t=unique(x$type), .combine=rbind) %do% {
  tab = table(x$year[x$type==t])
  ret = NULL
  if (length(tab)) {
    ret = data.frame(year=as.numeric(names(tab)), count=as.vector(tab), type=t)
  }
  ret
}

#levels(count_by_year$year) = sort(as.character(count_by_year$year))

# fix the years.
uyears = unique(count_by_year$year)
zero_year_counts = foreach(type=unique(x$type), .combine=rbind) %do% {
  xs = x[x$type == type,]
  missing_years = setdiff(uyears, unique(xs$year))
  ret = NULL
  if (length(missing_years > 0)) {
    ret = data.frame(year = missing_years, count=0.1, type=type)
  }
  ret
}

count_by_year = rbind(count_by_year, zero_year_counts)
count_by_year$all = factor(1)
count_by_year_all = divide(count_by_year, by="all", update=TRUE)

cbya = function(x) {
  ret = nPlot(count~year, group="type", data=x, type='multiBarChart')
  ret$yAxis(tickFormat = "#! function(d) {return d} !#")
  ret
}

total_chars_by_year = foreach(t=unique(x$type), .combine=rbind) %do% {
  xs = x[x$type == t,]
  year_char_count = foreach(y=unique(xs$year), .combine=rbind) %do% {
    data.frame(year = y, count=sum(xs$nchar[xs$year == y]))
  }
  year_char_count$type = t
  year_char_count
}

total_chars_by_year = rbind(total_chars_by_year, zero_year_counts)
total_chars_by_year = total_chars_by_year[order(total_chars_by_year$year),]
total_chars_by_year$all = factor(1)
total_chars_by_year_all = divide(total_chars_by_year, by="all", update=TRUE)

vdbConn("united-nations-db", name="UN/G77 Text Visualization", autoYes=TRUE)

makeDisplay(count_by_year_all,
            name="yearly_announcement_volume",
            group="All Longitudinal",
            width=350, height=200,
            desc="Yearly Announcement Volume",
            panelFn= cbya)

makeDisplay(total_chars_by_year_all,
            name="yearly_char_volume",
            group="All Longitudinal",
            width=350, height=200,
            desc="Yearly Character Volume",
            panelFn= cbya)

count_by_month_year = foreach(t=unique(x$type), .combine=rbind) %do% {
  tab=table(x$year.month[x$type==t])
  ret = NULL
  if (length(tab)) {
    ret = data.frame(year.month=as.numeric(names(tab)), count=as.vector(tab),
                     type=t)
  }
  ret
}

zero_year_month_counts = foreach(type=unique(x$type), .combine=rbind) %do% {
  xs = x[x$type == type,]
  uym = unique(x$year.month)
  missing_year_months = setdiff(uym, unique(xs$year.month))
  ret = NULL
  if (length(missing_year_months > 0)) {
    ret = data.frame(year.month = missing_year_months, count=0.1, type=type)
  }
  ret
}

count_by_month_year$year.month = as.numeric(count_by_month_year$year.month)
count_by_month_year$all = factor(1)
count_by_month_year_all = divide(count_by_month_year, by="all", update=TRUE)

cbym = function(x) {
  ret = nPlot(count~year.month, group="type", data=x, 
        type='multiBarChart')
  ret$yAxis(tickFormat = "#! function(d) {return d} !#")
  ret
}

makeDisplay(count_by_month_year_all,
            name="year_and_month_volume",
            group="All Longitudinal",
            width=350, height=200,
            desc="Monthly Announcement Volume",
            panelFn= cbym)
        
chars_by_month_year = foreach(t=unique(x$type), .combine=rbind) %do% {
  xs = x[x$type==t,]
  year_month_char_count = foreach(ym=unique(xs$year.month), .combine=rbind)%do%{
    data.frame(year.month=ym, count=sum(xs$nchar[xs$year.month == ym]))
  }
  year_month_char_count$type = t
  year_month_char_count
}

chars_by_month_year = rbind(chars_by_month_year, zero_year_month_counts)
chars_by_month_year$all = factor(1)
chars_by_month_year$year.month = 
  as.numeric(as.character(chars_by_month_year$year.month))
chars_by_month_year = 
  chars_by_month_year[order(chars_by_month_year$year.month),]
chars_by_month_year_all = divide(chars_by_month_year, by="all", update=TRUE)

makeDisplay(chars_by_month_year_all,
            name="monthly_char_volume",
            group="All Longitudinal",
            width=350, height=200,
            desc="Montly Character Volume",
            panelFn= cbym)
          
