library(doMC)
registerDoMC()

# Find the text novelty in x1 from x0.
text_novelty = function(x1, x0) {
  t1 = unlist(strsplit(x1, " "))
  t0 = unlist(strsplit(x0, " "))
  length(setdiff(t1, t0)) / length(t1) * 100
}

y = x[x$type != 'secretary-general' & x$type != 'general-assembly',]

year_novelties = foreach(type = unique(y$type), .combine=rbind) %dopar% {
  ret = foreach (yr=unique(y$year)[-5], .combine=rbind) %do% {
    y0 = paste(y$stems[y$type == type & y$year == yr-1], collapse=" ")
    y1 = paste(y$stems[y$type == type & y$year == yr], collapse=" ")
    ret = NULL
    if (!(y0 == "" || y1 == "")) {
      novelty = text_novelty(y1, y0)
      ret = data.frame(novelty=novelty, year=yr)
    }
    ret
  }
  ret$type = type
  ret
}

year_novelties = year_novelties[order(year_novelties$year),]
nPlot(novelty ~ year, data=year_novelties, group="type", type="multiBarChart")

year_months = sort(unique(y$year.month))
month_novelties = foreach(type = unique(y$type), .combine=rbind) %dopar% {
  ret = foreach (i=2:length(year_months), .combine=rbind) %do% {
    y0 = paste(y$stems[y$type == type & y$year.month == year_months[i-1]],
               collapse=" ")
    y1 = paste(y$stems[y$type == type & y$year.month == year_months[i]],
               collapse=" ")
    ret = NULL
    if (!(y0 == "" || y1 == "")) {
      novelty = text_novelty(y1, y0)
      ret = data.frame(novelty=novelty, year.month=year_months[i])
    } else {
      ret = data.frame(novelty = 0.1, year.month=year_months[i])
    }
    ret
  }
  ret$type = type
  ret
}

uym = unique(y$year.month)
zero_year_month_counts = foreach(type=unique(y$type), .combine=rbind) %do% {
  ys = y[y$type == type,]
  missing_year_months = setdiff(uym, unique(ys$year.month))
  ret = NULL
  if (length(missing_year_months > 0)) {
    ret = data.frame(year.month = missing_year_months, 
                     novelty=0.1, type=type)
  }
  ret
}
zero_year_month_counts$year.month = 
  as.character(zero_year_month_counts$year.month) 
month_novelties$year.month = 
  as.character(month_novelties$year.month)

month_novelties = rbind(month_novelties, zero_year_month_counts)
month_novelties$year.month = 
  as.numeric(as.character(month_novelties$year.month))

month_novelties = month_novelties[order(month_novelties$type),]
month_novelties = month_novelties[order(month_novelties$year.month),]
row.names(month_novelties) = NULL

nPlot(novelty ~ year.month, data=month_novelties, group="type", 
      type="multiBarChart")


