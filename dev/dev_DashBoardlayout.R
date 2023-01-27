pkgload::load_all()
library(shinydashboard)
library(visNetwork)
library(igraph)
options(shiny.autoreload = T)
ui <- dashboardPage(
  dashboardHeader(
    dropdownMenu(
      type = "tasks"
    ),
    title = "My First @pp"
  ),
  dashboardSidebar(

  ),
  dashboardBody(
    fluidRow(
      box(mod_visNetworkWrite_ui("id")),
      box(mod_visNetworkReadControler_ui("id"))
    )
  )
)

server <- function(input, output, session){
  g <- reactive({
    g <- igraph::make_tree(40, 3, mode = "out")
    nV <- length(V(g))
    nE <- length(E(g))
    V(g)$names <- sample(letters, nV, replace = T)
    V(g)$attr1 <- sample(seq(10), nV, replace = T)
    V(g)$attr2 <- sample(LETTERS, nV, replace = T)
    return(g)
  })
  mod_visNetworkWrite_server("id", g, dev = F)
  mod_visNetworkReadControler_server("id", g)
}

shinyApp(ui, server)
