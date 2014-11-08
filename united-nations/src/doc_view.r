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
y$PC1 = pc_all_docs[,1] - mean(pc_all_docs[,1])
y$PC2 = pc_all_docs[,2] - mean(pc_all_docs[,2])
str = paste("<table>",
            "<tr><td align='left'><b>Type:</b></td><td>%s</td></tr>",
            "<td align='left'><b>Date:</b></td><td>%s</td></tr></tr></table>")
y$html_caption = sprintf(str, y$type, y$date)
ypcs = project_into_document_space(y$stems)
y$PC1 = ypcs[,1]
y$PC2 = ypcs[,2]
      
#scatter_plot_with_url(y, "PC1", "PC2")

spwu = function(x) {
  scatter_plot_with_url(x, "PC1", "PC2", point_radius=2)
}

y$all = as.factor(1)
y_all = divide(y, by="all", update=TRUE)

makeDisplay(y_all,
            name="all_announcements",
            group="Year",
            width=350, height=200,
            desc="Articles by Year in the Document Space",
            panelFn= nby)


