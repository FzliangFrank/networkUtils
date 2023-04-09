pkgload::load_all()
library(bs4Dash)
library(visNetwork)
library(igraph)
options(shiny.autoreload = T)
ui <- dashboardPage(
  dashboardHeader(
    dropdownMenu(
      type = "tasks"
    ),
    title = "A Graph Modification App"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard")
    )

  ),
  dashboardBody(
    tabItem(
      tabName = "Dashboard",
      fluidRow(
        column(12, box(mod_visNetworkWrite_ui("id"),
            width = 12,
            action = "update",
            maximizable = T
            # sidebar = boxSidebar(
            #   id = "sidebar",
            #   width = 30,
            #   easyClose = F,
            #   background = "#34495E",
            #     boxPad(mod_visNetworkReadControler_ui("id")),
            #   startOpen = T
            # )
        ))
      )
    )
  ),
  controlbar = dashboardControlbar(
    preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
    pinned = F,
    width = 250,
    controlbarMenu(
      controlbarItem(
        title = "",
        icon = icon(name = "magnifying-glass-dollar", c("fa-shake")),
        fileInput("file", "Upload my own sheet"),
        h4("Find a Node"),
        div(class = 'p-3', mod_visNetworkReadControler_ui("id"))
      ),
      controlbarItem(
        title = "",
        icon = icon("palette"),
        div(class = 'p-3', skinSelector())
      )
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

  grv <- mod_visNetworkWrite_server("id", g, dev = F)
  # gr <- reactive(grv$Current)
  mod_visNetworkReadControler_server("id", grv)
}

shinyApp(ui, server)
