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
    g <- igraph::make_tree(20, 3, mode = "out")
    nV <- length(V(g))
    nE <- length(E(g))
    V(g)$label <- sample(letters, nV, replace = T) # fix this latter NAME needs to not identical
    V(g)$attr1_int <- sample(seq(10), nV, replace = T)
    V(g)$attr2_letter <- sample(LETTERS, nV, replace = T)
    V(g)$attr3_numb <- runif(nV) * 100
    sp_df = data.frame(x = runif(nV), y= runif(nV)) |> sf::st_as_sf(coords=c('x', 'y'))
    V(g)$attr4_geom <- sp_df$geom
    E(g)$attr1 <- runif(nE) * 100

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
