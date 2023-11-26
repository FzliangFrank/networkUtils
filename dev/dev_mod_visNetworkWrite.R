library(igraph)
library(visNetwork)
g <- igraph::make_graph(~ A-+B:C,
                        B-+D:E, C-+F:G
)

# g <- igraph::make_tree(30, 3)
g_length <- length(V(g))
e_length <- length(E(g))
V(g)$name <- seq(g_length) |> as.character() |> paste0(".name")
V(g)$attr_1 <- sample(seq(10), g_length, replace = T)
V(g)$attr_2 <- sample(LETTERS, g_length, replace = T)
E(g)$attr1 <- sample(LETTERS, e_length, replace = T)
E(g)$attr2 <- sample(seq(10), e_length, replace = T)
# E(g)$name <- seq(e_length) |> as.character()
E(g)$id <- seq(e_length) |> as.character() |> paste0(".id") # this attribute won't
#' get recognised

g <- tidygraph::as_tbl_graph(g)

##' The Optimal formate to export is probably gml.
##' DOT is good igraph won't able to read them immediately
##'
# This Return Error
# create an example graph
g <- igraph::make_tree(40, 3, mode = "out")
nV <- length(V(g))
nE <- length(E(g))
V(g)$names <- sample(letters, nV, replace = T)
V(g)$attr1 <- sample(seq(10), nV, replace = T)
V(g)$attr2 <- sample(LETTERS, nV, replace = T)
V(g)$title <- pasteNodeDetails(g)

E(g)$attr1 <- sample(LETTERS, nE, replace = T)
E(g)$attr2 <- sample(seq(10), nE, replace = T)
E(g)$title <- pasteEdgeDetails(g)
if(interactive()) {
  library(shiny)

  ui <- fluidPage(
    mod_visNetInteraction_ui("id"),
    mod_visNetModification_ui("id"),
    shiny::verbatimTextOutput("dev-print")
  )

  server <- function(input, output, session) {
    mod_visNetInteraction_server("id",
                                 reactive(g),
                                 v_ignore = c('title')
                                 )
    G <- mod_visNetModification_server("id",
                                  reactive(g)
                                  )
    output$`dev-print`=renderPrint({
      print(sprintf('edge clicked %s', G$click_edge))
      eidx=G$click_edge
      print(E(g)$title[1])
    })
  }
  options(shiny.autoreload = T)
  shinyApp(ui, server)
}

library(shiny)

ui <- fluidPage(
  visNetwork::visNetworkOutput('id')
)
ns=function(x) x
server <- function(input, output, session) {
  gr = reactive({g})
  output$id <- visNetwork::renderVisNetwork({
    visNetwork::visIgraph(gr()) |>
      visNetwork::visOptions(
        clickToUse = T,
        collapse = T,
        manipulation = list(
          enabled = input$edit,
          addNodeCols = c("label")
        ),
        highlightNearest = list(
          enabled = T,
          degree = 0,
          algorithm = "hierarchical"
        )
      ) |>
      # Custom Events
      visNetwork::visEvents(
        selectNode = htmlwidgets::JS(sprintf("function(properties){
                    Shiny.setInputValue('%s',
                    this.body.data.nodes.get(properties.nodes[0]).id)
                    ;}", ns("click_node") # Your shiny module have namespace
        )),
        selectEdge = htmlwidgets::JS(sprintf("function(properties){
                    Shiny.setInputValue('%s',
                    properties.edges)
                    }", ns("click_edge")
        ))
      )
  })
}

shinyApp(ui, server)
