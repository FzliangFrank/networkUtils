devtools::load_all()

library(shiny)
library(tidyverse)
library(reactable)
# starwars
#
ui <- fluidPage(
  column(4, mod_dynamic_attr_selector_ui('id')),
  column(8, reactableOutput("rct"))
)

server <- function(input, output, session) {
  exampleData = reactive({
    starwars[["height"]]<-as.double(starwars[["height"]])
    starwars
  })
  idx=mod_dynamic_attr_selector_server("id", exampleData)
  slicedData = reactive({
    if(!is.null(idx())) {
      exampleData()[idx(),]
    } else {
      exampleData()
    }
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



