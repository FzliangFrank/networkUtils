library(shinyjqui)
library(visNetwork)
library(shiny)
ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      jqui_resizable(
       visNetworkOutput("id"),
        options = list(handles = "e,s,n,w")
      )
    )
  )
)

server <- function(input, output) {
  output$id <- renderVisNetwork({
    # generate dot plot
    g = igraph::make_tree(12,3)
    visIgraph(g) |>
      visOptions(clickToUse = T)
  })
}
shinyApp(ui, server)
