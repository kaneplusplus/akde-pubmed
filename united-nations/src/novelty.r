library(doMC)
registerDoMC()

# Find the text novelty in x1 from x0.
text_novelty = function(x1, x0) {
  t1 = unlist(strsplit(x1, " "))
  t0 = unlist(strsplit(x0, " "))
  length(setdiff(t1, t0)) / length(t1) * 100
}

text_diff = function(x1, x0) {
  t1 = unlist(strsplit(x1, " "))
  t0 = unlist(strsplit(x0, " "))
  paste(setdiff(t1, t0), collapse=" ")
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
#nPlot(novelty ~ year, data=year_novelties, group="type", type="multiBarChart")

nby = function(x) {
  nPlot(novelty ~ year, data=x, group="type", type="multiBarChart")
}

year_novelties$all = factor(1)
novelty_by_year_all = divide(year_novelties, by="all", update=TRUE)

makeDisplay(novelty_by_year_all,
            name="novelty_by_year",
            group="Yearly",
            width=350, height=200,
            desc="Media Novelty by Year",
            panelFn= nby)

year_months = sort(unique(y$year.month))
month_novelties = foreach(type = unique(y$type), .combine=rbind) %do% {
  ret = foreach (i=3:length(year_months), .combine=rbind) %do% {
    print(type)
    print(i)
    y0 = paste(y$stems[y$type == type & 
                       (y$year.month==year_months[i-1] | 
                        y$year.month==year_months[i-2])],
               collapse=" ")
    y1 = paste(y$stems[y$type == type & y$year.month == year_months[i]],
               collapse=" ")
    ret = NULL
    if (!(y0 == "" || y1 == "")) {
      novelty = text_novelty(y1, y0)
      word_diff = text_diff(y1, y0)
      ret = data.frame(novelty=novelty, year.month=year_months[i], 
                       text_diff=word_diff)
    } else {
      ret = data.frame(novelty = 0.1, year.month=year_months[i],
                       text_diff="")
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
                     novelty=0.1, type=type, text_diff="")
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

nbym_gen = function(xlim=NULL, ylim=NULL) {
  function(x) {
    ret = nPlot(novelty~year.month, data=x, group="type", type="multiBarChart")
    if (!is.null(xlim))
      ret$chart(forceX = xlim)
    if (!is.null(ylim))
      ret$chart(forceY = ylim)
    ret
  }
}

month_novelties$all = factor(1)
novelty_by_month_all = divide(month_novelties, by="all", update=TRUE)
makeDisplay(novelty_by_month_all,
            name="novelty_by_month",
            group="Yearly",
            width=350, height=200,
            desc="Media Novelty by Month",
            panelFn= nbym_gen())

# TODO: Add cognostics.

novelty_by_month_cogs = function(x) {
  easn = sum(x$novelty[x$type == "economic-and-social-council"])
  if (!length(easn))
    easn=0
  scn = sum(x$novelty[x$type == "security-council"])
  if (!length(scn))
    scn=0
  g77n = sum(x$novelty[x$type == "G77"])
  if (!length(g77n))
    g77n=0
  list(
    econ_and_social_nov=cog(easn, desc="Economic and social council novelty"),
    security_council_nov=cog(scn, desc="Security council novelty"),
    g77=cog(g77n, desc="G77 novelty")
  )
}


month_novelties$ym_copy = month_novelties$year.month
novelty_by_month = divide(month_novelties, by="ym_copy", update=TRUE)

makeDisplay(novelty_by_month,
            name="novelty_by_month",
            group="Monthly",
            width=200, height=100,
            desc="Media Novelty by Month",
            panelFn= nbym_gen(ylim=c(0, ceiling(month_novelties$novelty))),
            cogFn=novelty_by_month_cogs)

# Cross sectional novelties.
tn = unique(y$type)
xmonth_novelties = foreach (ym=year_months, .combine=rbind) %do% {
  ns = foreach(type=tn, .combine=c) %do% {
    sw = paste(y$stems[y$year.month == ym & y$type == type], collapse=" ")
    if (!length(sw)) sw=""
    sw
  }
  c1 = text_novelty(ns[1], paste(ns[2], ns[3], collapse=" "))
  c2 = text_novelty(ns[2], paste(ns[1], ns[3], collapse=" "))
  c3 = text_novelty(ns[3], paste(ns[1], ns[2], collapse=" "))
  cs = c(c1, c2, c3)
  if (any(!is.finite(cs)))
    cs = rep(0, 3)
  ret = data.frame(list(cs, tn, rep(ym, 3)))
  names(ret) = c("novelty", "type", "year.month")
  ret
}

xnbym = function(x) {
  nPlot(novelty ~ year.month, data=x, group="type", 
        type="multiBarChart")
}

xmonth_novelties$all = factor(1)
xmonth_novelties_all = divide(xmonth_novelties, by="all", update=TRUE)

makeDisplay(xmonth_novelties_all,
            name="yearly_cross_sectional_novelty",
            group="Year Cross-Sectional",
            width=350, height=200,
            desc="Statement Novelty by Year",
            panelFn= xnbym)

xmonth_novelties$ym_copy = xmonth_novelties$year.month
xmonth_novelty_by_month = divide(xmonth_novelties, by="ym_copy", update=TRUE)
makeDisplay(xmonth_novelty_by_month,
            name="monthly_cross_sectional_novelty",
            group="Month Cross-Sectional",
            width=350, height=200,
            desc="Statement Novelty by Month",
            panelFn= xnbym,
            cogFn=novelty_by_month_cogs)

