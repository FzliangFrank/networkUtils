#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
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
    layout = reactive(input$g_layout),
    visNet_options=list(clickToUse=F),
    )
  mod_visNetInteraction_server("id", reactive(G$Current))
  # something else
  changeLog <- reactiveVal(value=list())
  observe({
    req(G$editing)
    req(!is.null(G$editing$cmd) || is.na(G$editing))
    # print('recording change')
    x =isolate(changeLog()) |>
      append(list(list(
        time = Sys.time(),
        change = G$editing
      )), after=0)
    changeLog(x)
  })
  output$timeline <- renderUI({
      bs4Dash::timelineBlock(
        width=12,elevation=4,reversed=F,
        style='overflow: auto important!;',
        purrr::map(
          changeLog(),
          log_timeline_item
        )
      )
    # )
  })
  output$logjson <- renderPrint({
    changeLog() |>
      jsonlite::toJSON(auto_unbox = T, simplifyMatrix = T) |>
      jsonlite::prettify()
  })
}
