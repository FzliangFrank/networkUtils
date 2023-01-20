#' visNetworkWrite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visNetworkWrite_ui <- function(id){
  ns <- NS(id)
  tagList(
    visNetwork::visNetworkOutput(ns("plot")),
    shinyWidgets::switchInput(ns("edit"), "edit mode"),
    shinyWidgets::actionGroupButtons(ns("save"), "save"),
    shiny::uiOutput("AttrEditor"),
    downloadButton(ns("export")),
    shiny::verbatimTextOutput(ns("dev"))
  )
}

#' visNetworkWrite Server Functions
#'
#' @noRd
mod_visNetworkWrite_server <- function(id, igraphObj){
  # stop if not reactive
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    curGraph <- reactiveValues(g = NULL)
    mainGraph <- reactiveValues(g = NULL)
    observe({
      curGraph$g <- igraphObj()
      mainGraph$g <- igraphObj()
    })
    observe({
      mainGraph$g <- isolate(curGraph$g)
    }) |>
      bindEvent(input$save)

    output$plot <- visNetwork::renderVisNetwork({
      g <- mainGraph$g
      # this to should be done first before adding visNetwork default namespace
      V(g)$title <- pasteNodeDetails(g)
      E(g)$title <- pasteEdgeDetails(g)
      base_graph <- visNetwork::visIgraph(g, randomSeed = "3") |>
        visNetwork::visOptions(
          manipulation = input$edit,
          highlightNearest = list(
            enabled = T,
            degree = 0,
            algorithm = "hierarchical"
            )
          ) |>
        visEvents(selectNode = htmlwidgets::JS(sprintf("function(properties){
                  Shiny.setInputValue('%s',
                  properties.nodes)
                  ;}", ns("click_node") # Your shiny module have namespace
        )),
     #    select = "function(properties) {
     # alert('selected nodes: ' + properties.nodes);}",
        # selectEdge = htmlwidgets::JS(sprintf("function(properties){
        #           Shiny.setInputValue('%s',
        #           this.body.data.edges.get(properties.edges[0]).id)
        #           }", ns("click_edge")
        # )),
       selectEdge = htmlwidgets::JS(sprintf("function(properties){
                    Shiny.setInputValue('%s',
                    properties.edges[0])
                    }", ns("click_edge")
       ))
        )
    })
    observe(label = "htmlwidgets",{
      visNetworkProxy(ns("plot"))
    })

    output$dev <- shiny::renderPrint({
        # print(input$click)

        print(paste("click node:", input$click_node))
        print(paste("click edge (id):", input$click_edge, class(input$click_edge)))
        print(paste("try find edge:"))
        print(input$plot_graphChange)
        print(curGraph$g)

        #' $cmd == "addEdge"
        #' $cmd == "addNode"
        #' $cmd == "edgeNode"
        #' You might want to capture input$plot_graphChange as a req
        #' The other thing only use default value
    })
    observeEvent(input$plot_graphChange, {
      req(!is.null(input$plot_graphChange$cmd))
      if(input$plot_graphChange$cmd == "addNode") {
        id <- isolate(input$plot_graphChange$id)
        curGraph$g <- curGraph$g + vertex(id)
      } else if (input$plot_graphChange$cmd == "addEdge") {
        from = isolate(input$plot_graphChange$from)
        to = isolate(input$plot_graphChange$to)
        curGraph$g <- curGraph$g + edge(c(from, to))
      } else if (input$plot_graphChange$cmd == "editEdge") {
        id <- isolate(input$plot_graphChange$id)
        from = isolate(input$plot_graphChange$from)
        to = isolate(input$plot_graphChange$to)
        g <- curGraph$g
        attrs <- edge_attr(g, index = id)

        g <- g - edge(id)
        curGraph$g <- add_edges(g, c(from, to), attr = attrs)
      }
    })
  })
}

## To be copied in the UI
# mod_visNetworkWrite_ui("visNetworkWrite_1")
## To be copied in the server
# mod_visNetworkWrite_server("visNetworkWrite_1")
