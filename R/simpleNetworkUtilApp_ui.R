#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
simpleNetworkUtilApp_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      preloader = list(html = tagList(waiter::spin_2(), "Loading ..."), color = "#212F3D"),
      bs4Dash::dashboardHeader(
        title = "A Graph Modification App"
      ),
      bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          bs4Dash::menuItem("Graph", tabName = "graph", icon = icon("circle-nodes", "fa-beat-fade")),
          bs4Dash::menuItem("About", tabName = "about", icon = icon("github"))
        )
      ),
      bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          # TABITEMS ===========================================================
          bs4Dash::tabItem(
            tabName = "graph",
            fluidRow(
              column(12, bs4Dash::box(mod_visNetworkWrite_ui("id"),
                                      width = 12,
                                      action = "update",
                                      maximizable = T
              ))
            )
          ),
          bs4Dash::tabItem(
            tabName = "about",
            fluidRow(
              # column(
              #   width = 6,
              #   # title = "About",
              # ),
              # column(
                # ----------------------------------------
                width = 6,
                bs4Dash::jumbotron(
                  title = "NetworkUtils Apps",
                  lead = ""
                    ,
                  href = "https://github.com/FzliangFrank/networkUtils",
                  btnName = icon("github"),
                  status = NULL,
                  div(
                    tags$figure(
                      align = "center",
                      tags$img(
                        src = "www/hex-shinySpider.png",
                        width = 200
                      ))
                  ),
                  p("{shinySpider} is part {networkUtil}, a R package
                  extension aims to make creating, update wraggling network
                  data easy, lucide, and interactive.
                    ")
                )
                # ----------------------------------------
              # )
            )
          )
          #  ========================================================== TABITEMS
        )
        ),
      controlbar = bs4Dash::dashboardControlbar(
        pinned = F,
        width = 300,
        bs4Dash::controlbarMenu(
          bs4Dash::controlbarItem(
            title = "",
            icon = icon("magnifying-glass-arrow-right"),
            div(class = 'p-3',
                mod_visNetworkReadControler_ui("id")
            )
          ),
          bs4Dash::controlbarItem(
            title = "",
            icon = icon("upload", "fa-shake"),
            mod_fileUploader_ui("file")
          ),
          bs4Dash::controlbarItem(
            title = "",
            icon = icon("palette", `data-toggle`="tooltip", `data-toggle`="top", title='Themes'),
            div(class = 'p-3', bs4Dash::skinSelector())
          )
        )

      )
    )
  )
}

