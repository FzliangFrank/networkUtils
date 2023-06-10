#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
simpleNetworkUtilApp_server <- function(input, output, session) {
  # Your application server logic
  gfile <- mod_fileUploader_server("file")
  data = reactiveValues(demo = NULL, prod = NULL)
  observe(label="Example Graph", {
    data$demo = create_demo_graph()
  })
  observe({
    data$prod = gfile()
  })
  g <- reactive({
    if(is.null(data$prod)) return(data$demo)
    return(data$prod)
  })
  G = mod_visNetworkWrite_server("id", g, dev = F)
  # gr <- reactive(grv$Current)
  mod_visNetworkReadControler_server("id", reactive(G$Current))
}
