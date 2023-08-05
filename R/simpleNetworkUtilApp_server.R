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
      data$demo = create_demo_graph()
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

  G = mod_visNetModification_server("id", g, dev = F, layout = 'layout_with_kk')
  mod_visNetInteraction_server("id", reactive(G$Current))


  # something else
}
