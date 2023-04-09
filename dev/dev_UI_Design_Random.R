ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
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
      # jumbotron(
      #   title = "Welcome!",
      #   lead = "This app let you modify a node and edge data automatically"
      # ),
      # accordion(
      #   id = "uploader",
      #   accordionItem(
      #     title = "Step 1: Upload a File",
      #     fileInput("file", "Upload Edge Sheet")
      #   ),
      #   accordionItem(
      #     title = "Step 2: Column Definition"
      #   ),
      #   accordionItem(
      #     title = "Step 3: Validation"
      #   )
      # ),
      fluidRow(
        column(12, box(mod_visNetworkWrite_ui("id"),
                       width = 12,
                       action = "update",
                       maximizable = T
        ))
      )
    )
  ),
  controlbar = dashboardControlbar(
    pinned = F,
    width = 300,
    controlbarMenu(
      controlbarItem(
        title = "",
        icon = icon(name = "magnifying-glass-arrow-right"),
        # accordion(
        #   id = "ctrl",
        #   accordionItem(
        #     title = "File Upload",
        #     fileInput("file", "Upload my own sheet")
        #   )
        #   # accordionItem(
        #   #   title = "Find a Node",
        #   #
        #   # ),
        # ),
        div(class = 'p-3',
            mod_visNetworkReadControler_ui("id")
            )
      ),
      controlbarItem(
        title = "",
        icon = icon("upload", "fa-shake"),
        mod_fileUploader_ui("file")
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
  gfile <- mod_fileUploader_server("file")
  data = reactiveValues(demo = NULL, prod = NULL)
  observe({
      g <- igraph::make_tree(20, 3, mode = "out")
      nV <- length(V(g))
      nE <- length(E(g))
      V(g)$names <- sample(letters, nV, replace = T)
      V(g)$attr1 <- sample(seq(10), nV, replace = T)
      V(g)$attr2 <- sample(LETTERS, nV, replace = T)
      # V(g)$shape <- "box"
      # V(g)$color <- "midnightblue"
      # V(g)$font.color <- "white"
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

shinyApp(ui, server)
