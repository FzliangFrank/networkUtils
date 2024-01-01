devtools::load_all()

library(shiny)
library(tidyverse)
library(reactable)
# starwars
#
ui <- fluidPage(
  mod_dynamic_attr_selector_ui('id'),
  reactableOutput("rct")
)

server <- function(input, output, session) {
  exampleData = reactive({
    starwars
  })
  idx=mod_dynamic_attr_selector_server("id", exampleData)
  slicedData = reactive({
    exampleData()[idx(),]
  })
  output$rct<-renderReactable({
    slicedData() |>
      reactable()
  })
}

shinyApp(ui, server)

# Test A Graph =================================================================
# library(visNetwork)
# ui <- fluidPage(
#   mod_dynamic_attr_selector_ui('id'),
#   visNetworkOutput("vn")
# )
#
# server <- function(input, output, session) {
#   exampleData = reactive({
#     create_demo_graph(40,4)
#   })
#   nodeData = reactive({
#     igraph::vertex_attr(exampleData())
#   })
#   idx=mod_dynamic_attr_selector_server("id", nodeData)
#
#   output$vn<-renderVisNetwork({
#     exampleData() |>
#       visIgraph()
#   })
#
#   observe({
#     visNetworkProxy("vn") |>
#       visSelectNodes(idx())
#   })
# }
#
# shinyApp(ui, server)



