library(xts)
library(foreach)

# TODO: this is messed up.
get_article_counts <- function(x, resolution="years") {
  date_matrix <- matrix( unlist(strsplit(x$date_string, " ")), ncol=3, 
    byrow=TRUE )
  if (resolution == "months") {
    date_matrix[,2] <- "1"
  } else if (resolution == "years") {
    date_matrix[,1] <- "Jan"
    date_matrix[,2] = "1"
  } else if (resolution != "days") {
    stop("Unsupported time resolution")
  }
  date_strings <- apply(date_matrix, 1, paste, collapse=" ")
  d <- strptime(date_strings, format="%b %d %Y", tz="GMT")

  if (resolution == "years")
    d <- d + years(1) - days(1)
  if (resolution == "months")
    d <- d + months(1) - days(1)

  if (any(is.na(d))) {
    warnings(paste(length(d) - length(d[!is.na(d)]), "dates could not be",
                   "constructed out of", length(d)))
  }
  d <- d[!is.na(d)]
  ret <- d
  if (length(d) > 0) {
    article_dates <- xts(rep(1, length(d)), order.by=d, unique=FALSE)
    tz(index(article_dates)) <- "GMT"
    vol <- period.apply(article_dates, endpoints(article_dates, resolution),
                        length)
    ret <- vol
    names(ret) <- "count"
    #ret <- data.frame(date=as.character(index(vol)), count=as.vector(vol),
    #                  stringsAsFactors=FALSE)
    #ret$title <- ret$date
  #  ret$annotation <- ret$date
  } else {
    warning("Dates could not be resolved at specified resolution")
  }
  ret
}

create_ac_ts = function(x, resolution="years", group=NULL) {
  if (resolution == "years") {
    pub_years = na.omit(unique(x$year))
    all_years = min(pub_years):max(pub_years)
  }
  foreach(g = unique(x[[group]]), .combine=rbind) %do% {
    ac = get_article_counts(x[x[[group]] == g,], resolution)
    acdf = as.data.frame(ac)
    res = NULL
    # TODO: Create x axis values from the 
    if (resolution != "years")
      stop("Only year resolutions are supported so far.")
    if (resolution == "years") {
      acdf$date = year(time(ac))
      zero_years = setdiff(all_years, acdf$date)
      acdf = rbind(acdf, 
        data.frame(date=zero_years, count=rep(0, length(zero_years))))
    }
    acdf = acdf[order(acdf$date),]
    acdf[[group]] = g
    acdf
  }
}

