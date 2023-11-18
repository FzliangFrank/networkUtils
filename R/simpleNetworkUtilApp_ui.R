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
              column(12, bs4Dash::box(mod_visNetModification_ui("id"),
                                      width = 12,
                                      action = "update",
                                      maximizable = T,
                                      tags$script(maximize_helper(visNetId('id')))
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
                  btnName =icon("github"),
                  status = "info",
                  div(
                    tags$figure(
                      align = "center",
                      tags$img(
                        src = "www/hex-NetworkUtils.png",
                        width = 200
                      ))
                  ),
                  p("{networkUtil}, R package encapsulated shiny module
                  template aims to make creating, update graph edge and node
                  data interactive, rjun_lucide and ease.

                  `mod_visNetwork_read` let you query node and edge based on attributes.
                  `mod_visNetowrk_write` let you edit and download as node edge
                  sheets. It keeps data from original file you uploaded.
                    "),
                  HTML('<head><script type="text/javascript" src="https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js" data-name="bmc-button" data-slug="fzliangukr" data-color="#FFDD00" data-emoji=""  data-font="Cookie" data-text="Buy me a coffee" data-outline-color="#000000" data-font-color="#000000" data-coffee-color="#ffffff" ></script></head>')
                ),

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
                mod_visNetInteraction_ui("id")
            )
          ),
          bs4Dash::controlbarItem(
            title = "",
            icon = icon("upload"#, "fa-shake"
                        ),
            mod_fileUploader_ui("file")
          ),
          bs4Dash::controlbarItem(
            title = "",
            icon = icon("gear", `data-toggle`="tooltip", `data-toggle`="top",
                        title='Options'),
            div(class = 'p-3',
                selectizeInput('g_layout','graph layout', choices = NULL),
                sliderInput('n_node', 'generate n nodes',
                            value = 20, min = 20, max = 500),
                numericInput('bch', "branches",
                             value = 3, min = 1, max = 20)
                )
          )
        )

      )
    )
  )
}

