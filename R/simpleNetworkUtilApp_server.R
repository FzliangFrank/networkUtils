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
  observe({
    g <- igraph::make_tree(20, 3, mode = "out")
    nV <- length(V(g))
    nE <- length(E(g))
    V(g)$names <- sample(letters, nV, replace = T)
    V(g)$attr1 <- sample(seq(10), nV, replace = T)
    V(g)$attr2 <- sample(LETTERS, nV, replace = T)
    data$demo = g
  })
  observe({
    data$prod = gfile()
  })
  g <- reactive({
    if(is.null(data$prod)) return(data$demo)
    return(data$prod)
  })
  grv <- mod_visNetworkWrite_server("id", g, dev = F)
  # gr <- reactive(grv$Current)
  mod_visNetworkReadControler_server("id", grv)
}
