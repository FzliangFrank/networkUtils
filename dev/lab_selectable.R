
library(shiny)
library(visNetwork)
library(igraph)
devtools::load_all()

ccl = make_ring(10, T)

ui <- fluidPage(
  mod_visNetModification_ui('visNet'),
  mod_visNetInteraction_ui('visNet'),
  verbatimTextOutput('dev')
)

server <- function(input, output, session) {
  # output$visNet <- renderVisNetwork({
  #   visIgraph(ccl)
  # })
  # observe({
  #   visNetworkProxy('visNet') |>
  #     visGetNodes()
  #   print(input$visNet_nodes)
  # })
  graph = reactive(ccl)
  G = mod_visNet_server('visNet', graph)
  clicked_node = reactive({
    G$click_node
  })
  output$dev <- renderPrint({
    print(clicked_node())
  })
}

shinyApp(ui, server)
