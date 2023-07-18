#' networkUtilApp
#'
#' @description Explore your own graph and explort
#'
#' @return shiny session that quickly call you whatever graph
#' you put in into a graph
#' @param graph an igraph or tbl_graph object;
#' if you prefer node and edge notation call `igraph::graph_from_data_frame`
#' to convert node and edge sheet
#' @export
networkUtilApp = function(graph) {

  stopifnot(is.null(graph) || inherits(graph, "igraph"))

  ui <- fluidPage(
    shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        mod_visNetInteraction_ui("id")
      ),
      mainPanel = mainPanel(
        mod_visNetModification_ui("id")
      )
    )
  )

  server <- function(input, output, session) {
    Out = mod_visNetModification_server("id", reactive({graph}))
    mod_visNetModification_server("id", igraphObj = reactive({Out$Main}))
  }
  shinyApp(ui, server)
}



