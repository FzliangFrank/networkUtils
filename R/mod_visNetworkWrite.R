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
    shinyjs::useShinyjs(),
    shinyWidgets::switchInput(ns("edit"), "enable edit", size = "small"),
    p("you are in editing mode, exit without save will revert to original", id = ns("note")),
    visNetwork::visNetworkOutput(ns("plot")),
    wellPanel(
      actionButton(ns("save"), "Commit Change"),
      downloadButton(ns("export")),
    ),
    shiny::uiOutput("AttrEditor"),

    shiny::verbatimTextOutput(ns("dev"))
  )
}

#' visNetworkWrite Server Functions
#'
#' @noRd
mod_visNetworkWrite_server <- function(id, igraphObj){
  # stop if not reactive
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # DYNAMIC UI ---------------------------------------------------------------
    observeEvent(input$edit, {
      shinyjs::toggle("save", T)
      shinyjs::toggle("export", T)
      shinyjs::toggle("note", T)
    })
    # STAGE  -------------------------------------------------------------------
    curGraph <- reactiveValues(g = NULL)
    mainGraph <- reactiveValues(g = NULL)
    observe({
      g <- igraphObj()
      #' id will be used by igraph to pick up edges. It has to be a numeric vector
      #' other input will cause function `edge()` to crash
      #' Needs to clean up on session end?
      if("id" %in% vertex_attr_names(g)) {
        V(g)$.ref_id <-V(g)$id
        V(g)$id <- seq(length(V(g)))
      }
      if("id" %in% edge_attr_names(g)) {
        E(g)$.ref_id <- E(g)$id
        E(g)$id <- seq(length(E(g)))
      }
      curGraph$g <- g
      mainGraph$g <- g
    })
    # COMMIT LOGIC -------------------------------------------------------------
    observe(label = "Save to Main", {
      mainGraph$g <- isolate(curGraph$g)
    }) |>
      bindEvent(input$save)
    # SAVE A FILE ANY TIME
    output$export <- downloadHandler(
      filename = function() {
        paste("graph-", Sys.Date(), ".gml", sep = "")
      },
      content = function(file) {
        warning("Need To Clean up .ref_id before export")
        igraph::write_graph(curGraph$g,file, format = "gml")
      }
    )
    # MAIN VISUALISATION + CUSTOM EVENT-----------------------------------------
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
                    properties.edges)
                    }", ns("click_edge")
       ))
        )
    })
    observe(label = "VizProxy",{
      visNetworkProxy(ns("plot"))
    })
    # GRAPH EDITING LOGIC ------------------------------------------------------
    observeEvent(input$plot_graphChange, {
      req(!is.null(input$plot_graphChange$cmd))
      if(input$plot_graphChange$cmd == "addNode") {
        id <- isolate(input$plot_graphChange$id)
        curGraph$g <- curGraph$g + vertex(id)
      } else if (input$plot_graphChange$cmd == "addEdge") {
        if(is.null(NULL)) id = length(E(curGraph$g)) + 1
        from = isolate(input$plot_graphChange$from)
        to = isolate(input$plot_graphChange$to)
        curGraph$g <- curGraph$g + edge(c(from, to), id = id)
      } else if (input$plot_graphChange$cmd == "editEdge") {
        id <- isolate(input$plot_graphChange$id)
        from = isolate(input$plot_graphChange$from)
        to = isolate(input$plot_graphChange$to)
        g <- curGraph$g
      # save attributes asided
        attrs <- edge_attr(g, index = id)
      # add and delete edges
        g <- g - edge(id)
        curGraph$g <- add_edges(g, c(from, to), attr = attrs)
      } else if (input$plot_graphChange$cmd == "deleteElements") {

        g <- curGraph$g
        edges <- isolate(unlist(input$plot_graphChange$edges))
        nodes <- isolate(unlist(input$plot_graphChanges$nodes))
        g <- g - edge(edges)
        g <- igraph::delete_vertices(g, nodes)
        curGraph$g <- g
      }
    })
    # DEV AREA -----------------------------------------------------------------
    output$dev <- shiny::renderPrint({
      # print(input$click)

      print(paste("click node:", input$click_node))
      print(paste("click edge (id):", input$click_edge, class(input$click_edge)))
      # print(paste("try find edge:", E(curGraph$g)[input$click_edge]))
      print(input$plot_graphChange)

      if(!is.null(input$plot_graphChange)) {
        if(input$plot_graphChange$cmd == "deleteElements") {
          print(input$plot_graphChange$edges |> unlist())
          print(unlist(input$plot_graphChanges$nodes))
        }
      }
      print(curGraph$g)
    })
    # MODULE END ---------------------------------------------------------------
  })
}

## To be copied in the UI
# mod_visNetworkWrite_ui("visNetworkWrite_1")
## To be copied in the server
# mod_visNetworkWrite_server("visNetworkWrite_1")
