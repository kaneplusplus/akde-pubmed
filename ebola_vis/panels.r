library(rCharts)

scatter_plot_with_url <- function(form, x, xlab, ylab, color=NULL) {
  char_form = as.character(form)
  tmp <- x[,c(char_form[-1], "title_short", "author", "date_string_clean", 
    "journal_short", color, "url")]

  # polycharts is dumb and bounds the data points exactly
  # which masks parts of some of the plotting points
  # we need some whitespace around the edges
  getLims <- function(x, frac = 0.07) {
    xRange <- range(x, na.rm = TRUE)
    xRange + c(-1, 1) * diff(xRange) * frac
  }

  xlim <- getLims(x[,char_form[3]])
  ylim <- getLims(x[,char_form[2]])
  tooltip_string = paste("#!function(item){ return item.title_short",
                         "'Author: ' + item.author +",
                         "\n'Date: ' + item.date_string_clean + ",
                         "'Journal: ' + item.journal_short }!#", sep=" + ")

  if (is.null(color)) {
    p <- rPlot(form, data = tmp, type = 'point', size = list(const=3),      
      tooltip = tooltip_string)
  } else {
    p <- rPlot(form, data = tmp, type = 'point', size = list(const=3),      
      color='species', tooltip = tooltip_string)
  }
  p$guides(
    x = list(title = xlab, min = xlim[1], max = xlim[2]),
    y = list(title = ylab, min = ylim[1], max = ylim[2]))
  p$set(dom = 'chart1')
  p$setTemplate(afterScript = "
    <script>
    graph_chart1.addHandler(function(type, e){
       var data = e.evtData
       if (type === 'click') {
          var url = data.url.in[0]
          window.open(url, '_blank')
       }
    })
    </script>    
  ")
  p
}

