g <- igraph::make_tree(40, 3, mode = "out")
nV <- length(V(g))
nE <- length(E(g))
V(g)$names <- sample(letters, nV, replace = T)
V(g)$attr1 <- sample(seq(10), nV, replace = T)
V(g)$attr2 <- sample(LETTERS, nV, replace = T)
V(g)$title <- pasteNodeDetails(g)
if(interactive()) {
  library(shiny)

  ui <- fluidPage(
    mod_visNetInteraction_ui("id"),
    mod_visNetModification_ui("id")
  )

  server <- function(input, output, session) {
    mod_visNetInteraction_server("id",
                                 reactive(g),
                                 v_ignore = c('title')
    )
    mod_visNetModification_server("id", reactive(g))
  }
  options(shiny.autoreload = T)
  shinyApp(ui, server)
}

visNetwork::visIgraph(g) |>
  visNetwork::visConfigure(enabled = T)
