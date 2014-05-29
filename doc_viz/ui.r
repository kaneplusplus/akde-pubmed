require(shiny)
require(rCharts)

shinyUI(pageWithSidebar(
  headerPanel("Document View for PubMed Queries"),

  sidebarPanel(
    textInput("pm_query", "PubMed Query:", value="rift valley fever"),
    numericInput("num_clusters", "The number of clusters",
      value=3, min=2, max=20, step=1),
    numericInput("max_num_docs", "The maximum number of documents",
      value=100, min=10, max=5000, step=10),
    submitButton("Submit query")
  ),
  mainPanel(
    h3("Latent Dirichlet Allocation Topics"),
    showOutput("lda_chart", "highcharts"),
    h3("Latent Semantic Indexing Topics"),
    showOutput("lsi_chart", "highcharts")
  )
))

