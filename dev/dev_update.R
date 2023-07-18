g = igraph::make_tree(20, 4)
# V(g)$id = seq(length(g))
# visIgraph(g)
V(g)$name = paste0('n_',seq(length(g)))
V(g)$attr1 = seq(length(g)) |> as.double()
E(g)$id = seq(length(E(g)))


library(shiny)

ui <- fluidPage(
  # sidebarLayout(
    column(
      4,
      mod_visNetInteraction_ui('id')
    ),
    column(
      8,
      mod_visNetModification_ui("id")
    )
  # )
)

server <- function(input, output, session) {
  # SessionGraph = mod_visNetModification_server('id', reactive(g), dev = T)
  # mod_visNetInteraction_server("id", reactive(SessionGraph$Current))
  mod_visNet_server('id', reactive(g))
}

shinyApp(ui, server)



