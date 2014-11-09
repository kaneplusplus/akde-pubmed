library(irlba)

stems_to_tdm = function(doc_vec, sparse=TRUE) {
  corpus = Corpus(VectorSource(doc_vec))
  tdm = TermDocumentMatrix(corpus)
  col_totals = apply(tdm , 2, sum)
  tdm = tdm[,col_totals > 0]
  x = Matrix(tdm, nrow=nrow(tdm), ncol=ncol(tdm))
  rownames(x) = rownames(tdm)
  colnames(x) = colnames(tdm)
  x
}

tfidf <- function(x, log.scale) {
  UseMethod("tfidf", x)
}

tfidf.default <- function(x, log.scale=TRUE) {
  df <- as.vector(apply(x, 1, function(x) sum(x > 0)))
  if (log.scale)
   log(x+1) * (log2(ncol(x)/df) + 1)
  else
   x * ncol(x)/df
}

tfidf.sparseMatrix <- function(x, log.scale=TRUE) {
  df <- as.vector(apply(x, 1, function(x) sum(x > 0)))
  if (log.scale) {
    x@x <- log(x@x+1)
    x * (log2(ncol(x)/df) + 1)
  }
  else
    x * ncol(x)/df
}

project_into_document_space = function(x, components=1:2, jitter=FALSE) {
  UseMethod("project_into_document_space", x)
}

project_into_document_space.default <- function(x, components=1:2,
  jitter=FALSE) {
  pcp = irlba(x, nv=max(components), tol=1e-5, dU=rep(1,nrow(x)),
              ds=1, dV=colMeans(x))
  ret = t(x) %*% pcp$u
  ret = ret[,components]
  if (jitter) {
    for (j in 1:ncol(x))
      x[,j] = jitter(x[,j])
  }
  rownames(ret) = colnames(x)
  ret
}

project_into_document_space.character <- function(x, components=1:2,
  jitter=FALSE) {
  project_into_document_space.default(tfidf(stems_to_tdm(x)),
    jitter=jitter)
}

scatter_plot_with_url = function(data, x_name="x", y_name="y", by=NULL, xlab="",
                           ylab="", title="",
                           legend_title="", subtitle="", point_radius=5) {
  names(data)[match(c(x_name, y_name), names(data))] <- c("x", "y")

  if (is.null(by)) {
    data$all = as.factor(1)
    by = "type"
  }
  data_list <- lapply(split(data, data[,by]), function(x) {
    res <- lapply(split(x, rownames(x)), as.list)
    names(res) <- NULL
    res
  })

  viz <- rCharts::Highcharts$new()
  invisible(sapply(data_list, function(x) {
      apply_cat <- eval(parse(text=paste("x[[1]]$", by, sep="")))
      viz$series(data=x, type="scatter", name=apply_cat)
    }))

  viz$plotOptions(
    scatter = list(
      cursor = "pointer",
      point = list(
        events = list(
          click = "#! function() { window.open(this.options.url); } !#")),
      marker = list(
        symbol = "circle",
        radius = point_radius 
      )
    )
  )

  viz$xAxis(title = list(text=xlab),
    labels = list(format="{value} "))
  viz$yAxis(title = list(text=ylab),
    labels = list(format="{value} "))
  viz$tooltip(useHTML=TRUE,
    formatter="#! function() { return this.point.html_caption; } !#")

  viz$legend(
    align = 'right',
    verticalAlign = 'middle',
    layout = 'vertical',
    title = list(text=legend_title)
  )
  viz$title(text=title)
  viz$subtitle(text=subtitle)
  #viz$chart( zoomType = 'xy' )
  viz
}

pc_all_docs = project_into_document_space(y$stems)
y$PC1 = pc_all_docs[,1] 
y$PC2 = pc_all_docs[,2]
str = paste("<table>",
            "<tr><td align='left'><b>Type:</b></td><td>%s</td></tr>",
            "<td align='left'><b>Date:</b></td><td>%s</td></tr></tr></table>")
y$html_caption = sprintf(str, y$type, y$date)
ypcs = project_into_document_space(y$stems)
y$PC1 = ypcs[,1]
y$PC2 = ypcs[,2]
      
#scatter_plot_with_url(y, "PC1", "PC2")

spwu_gen =function(point_radius=5) {
  function(x) {
    x$PC1 = x$PC1 - mean(x$PC1)
    x$PC2 = x$PC2 - mean(x$PC2)
    scatter_plot_with_url(x, "PC1", "PC2", point_radius=5)
  }
}

spwu = spwu_gen()

y$all = as.factor(1)
y_all = divide(y, by="all", update=TRUE)

makeDisplay(y_all,
            name="all_announcements",
            group="All",
            width=350, height=200,
            desc="All Articles in the Document Space",
            panelFn= spwu)

# Document clustering quarterly.
year_months = sort(unique(y$year.month))
y_quarter = foreach(ymi = 3:length(year_months), .combine=rbind) %do% {
  ym = year_months[(ymi-2):ymi]
  ys = y[y$year.month %in% ym,]
  yspcs = project_into_document_space(ys$stems)
  ys$PC1 = yspcs[,1]
  ys$PC2 = yspcs[,2]
  ys$quarter = ym[3]
  ys$novelty = foreach(i=1:nrow(ys), .combine=c) %do% {
    ret = month_novelties$novelty[
      month_novelties$year.month == as.numeric(ys$year.month[i]) &
      month_novelties$type == ys$type[i]]
    if(!length(ret)) ret = 0
    ret
  }
#  dists = sqrt(ys$PC1^2 + ys$PC2^2)
#  max_dists = foreach(type = unique(ys$types), .combine=c) %do% {
#    dists = sqrt(
#  }
  ys
}

novelty_cog = function(x) {
  easn = sum(x$novelty[x$type == "economic-and-social-council"][1])
  if (!length(easn))
    easn=0
  scn = sum(x$novelty[x$type == "security-council"][1])
  if (!length(scn))
    scn=0
  g77n = sum(x$novelty[x$type == "G77"][1])
  if (!length(g77n))
    g77n=0
  dists = sqrt(x$PC1^2 + x$PC2^2)
  ss = split(1:nrow(x), x$type)
  maxs = Map(function(s) max(dists[s]), ss)
  g77_max_dist = maxs$G77
  if (is.null(g77_max_dist)) g77_max_dist=0
  sc_max_dist = maxs$`security-council`
  if (is.null(sc_max_dist)) sc_max_dist=0
  easn_max_dist = maxs$`economic-and-social-council`
  if (is.null(easn_max_dist)) easn_max_dist=0
  list(
    econ_and_social_nov=cog(easn, desc="Economic and social council novelty"),
    security_council_nov=cog(scn, desc="Security council novelty"),
    g77=cog(g77n, desc="G77 novelty"),
    easn_max_dist=cog(easn_max_dist, desc="Max distance for EASCN"),
    sc_max_dist=cog(sc_max_dist, desc="Max distance for SC"),
    g77_max_dist=cog(g77_max_dist, desc="Max distance for G77")
  )
}

y_by_quarter = divide(y_quarter, by="quarter", update=TRUE)
makeDisplay(y_by_quarter,
            name="quarter_announcements",
            group="Quarter",
            width=350, height=200,
            desc="Articles the Document Space",
            panelFn= spwu,
            cogFn=novelty_cog)

y_month= foreach(ymi = 1:length(year_months), .combine=rbind) %do% {
  ym = year_months[ymi]
  ys = y[y$year.month %in% ym,]
  yspcs = project_into_document_space(ys$stems)
  ys$PC1 = yspcs[,1]
  ys$PC2 = yspcs[,2]
  ys$month = ym[3]
  ys$novelty = foreach(i=1:nrow(ys), .combine=c) %do% {
    ret = xmonth_novelties$novelty[
      xmonth_novelties$year.month == as.numeric(ys$year.month[i]) &
      xmonth_novelties$type == ys$type[i]]
    if(!length(ret)) ret = 0
    ret
  }
  ys
}

y_month$ym_copy = factor(y_month$year.month)

y_by_month = divide(y_month[,c("date", "url", "html_caption", "type", "PC1", 
                               "PC2", "ym_copy", "novelty")], by="ym_copy", 
                    update=TRUE)

makeDisplay(y_by_month,
            name="monthly_announcements",
            group="Month",
            width=350, height=200,
            desc="Articles the Document Space",
            panelFn=spwu_gen(10),
            cogFn=novelty_cog)
        
