library(shinyjqui)
library(visNetwork)
library(shiny)
ui <- dashboardPage(
  sidebar=dashboardSidebar(),
  header = dashboardHeader(title="test"),
  body=dashboardBody(
    box(
      # shinyjqui::jqui_resizable(
      visNetworkOutput("id", width='100%'),
      ## there are a couple more options you can do
      # options = list(handles = "e,s,n,w")
      # options = list(handles = 's')
      # ),
      # mod_visNetModification_ui('net'),
      maximizable = T
    ),
    box(
      mod_visNetModification_ui('net'),
      maximizable = T
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
  G = reactive({
    igraph::make_tree(12,3)
  })
  mod_visNetModification_server('net', G)
}
shinyApp(ui, server)
