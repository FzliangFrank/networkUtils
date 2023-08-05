#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @param graph igraph object to set up as default
#' @import shiny
#' @export
simpleNetworkUtilApp_server <- function(input, output, session) {
  # Your application server logic
  gfile <- mod_fileUploader_server("file")
  data = reactiveValues(demo = NULL, prod = NULL)
  observe(label="Example Graph", {
    if(is.null(graph) || !inherits(graph, 'igraph')) {
      data$demo = create_demo_graph(input$n_node, input$bch)
    } else {
      data$demo = graph
    }
  })
  observe({
    data$prod = gfile()
  })
  g <- reactive({
    if(is.null(data$prod)) return(data$demo)
    return(data$prod)
  })
  # gr <- reactive(grv$Current)
  observe({
    layout_options = c(
      "layout_nicely",
      "component_wise",
      "layout_in_circle",
      "layout_as_bipartite",
      "layout_as_tree",
      "layout_as_star",
      "layout_on_grid",
      "layout_on_sphere",
      "layout_with_dh",
      "layout_with_fr",
      "layout_with_lgl",
      "layout_with_kk",
      "layout_with_kk",
      "layout_with_mds",
      "layout_with_sugiyama"
    )
    updateSelectizeInput(session, "g_layout", choices = layout_options)
  })
  G = mod_visNetModification_server(
    "id", g, dev = F,
    layout = reactive(input$g_layout)
    )
  mod_visNetInteraction_server("id", reactive(G$Current))


  # something else
}
