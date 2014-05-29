require(shiny)
require(rCharts)
require(rredis)
require(doMC)

source("../entrez.r")
source("../dc.r")
source("../viz.r")

registerDoMC()

init_pm_query_cache()

shinyServer(function(input, output) {
  output$lda_chart <- renderChart2({
    create_viz(input$pm_query, input$max_num_docs, input$num_clusters,
               cluster_algo="lda", verbose=TRUE)
  })

  output$lsi_chart <- renderChart2({
    create_viz(input$pm_query, input$max_num_docs, input$num_clusters,
               cluster_algo="lsa", verbose=TRUE)
  })
})
