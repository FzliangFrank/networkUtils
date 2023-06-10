#' visNetworkWrite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @export
mod_visNetworkWrite_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyWidgets::switchInput(ns("edit"), "enable edit", size = "small"),
    p("You are in editing mode, exit without save will revert to original", id = ns("note")),
    visNetwork::visNetworkOutput(ns("visNetworkId")),
    wellPanel(
      actionButton(ns("save"), "Commit Change"),
      downloadButton(ns("export")),
    ),
    shiny::uiOutput("AttrEditor"),
    shiny::verbatimTextOutput(ns("dev"))
  )
}

#' visNetworkWrite Server Functions

#' @param id shiny server id
#' @param igraphObj a reactive graph object
#' @return reactiveValues $Curent and $Main
#' @details
#' $Current is a reactive igraph Object that every is being modified now
#' $Main is the igraph Object that has been committed and saved
#' @export
mod_visNetworkWrite_server <- function(id, igraphObj, dev = T){
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
    # curGraph <- reactiveValues(g = NULL)
    # mainGraph <- reactiveValues(g = NULL)

    Graph <- reactiveValues(
      Current = NULL,
      Main = NULL
    )
    observe({
      g <- igraphObj()
      req(!is.null(g))
      # id will be used by igraph to pick up edges. It has to be a numeric vector
      # other input will cause function `edge()` to crash
      # Needs to clean up on session end?
        if("id" %in% vertex_attr_names(g)) V(g)$.ir_id <- V(g)$id
        if("id" %in% edge_attr_names(g)) E(g)$.ir_id <- E(g)$id
        if(!"name" %in% vertex_attr_names(g)) V(g)$name <- seq(length(V(g))) |> as.character()
      E(g)$id <- seq(length(E(g))) |> as.character()
      Graph$Current <- g
      Graph$Main <- g
    })
    # COMMIT LOGIC -------------------------------------------------------------
    observe(label = "Save to Main", {
      g <- Graph$Current
      # Below code reverse actions
        # if(".ir_id" %in% vertex_attr_names(g)) {
        #   V(g)$id <- V(g)$.ir_id
        #   delete_vertex_attr(g, ".ir_id")
        # }
        # if(".ir_id" %in% edge_attr_names(g)) {
        #   E(g)$id <- E(g)$.ir_id
        #   delete_edge_attr(g, ".ir_id")
        # }
        # if(!"name" %in% vertex_attr_names(Graph$Main)) delete_graph_attr(g, "name")
      Graph$Main <- g
      print("current write into main")
    }) |>
      bindEvent(input$save)
    # SAVE A FILE ANY TIME -----------------------------------------------------
    output$export <- downloadHandler(
      filename = function() {
        # paste("graph-", Sys.Date(), ".gml", sep = "")
        paste("graph-", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        g <- Graph$Current
        # Below code reverse actions
        if(".ir_id" %in% vertex_attr_names(g)) {
          V(g)$id <- V(g)$.ir_id
          delete_vertex_attr(g, ".ir_id")
        }
        if(".ir_id" %in% edge_attr_names(g)) {
          E(g)$id <- E(g)$.ir_id
          delete_edge_attr(g, ".ir_id")
        }
        if(!"name" %in% vertex_attr_names(igraphObj())) delete_graph_attr(g, "name")
        # igraph::write_graph(Graph$Current,file, format = "gml")
        edge = igraph::as_data_frame(Graph$Current, what = "edges")
        node = igraph::as_data_frame(Graph$Current, what = "vertices")

        node = dplyr::mutate_if(node,
                                ~inherits(., "sfc"),
                                ~sf::st_as_text(.)
                                )

        wb = openxlsx::createWorkbook()
        wb |> openxlsx::addWorksheet("node")
        wb |> openxlsx::addWorksheet("edge")
        wb |> openxlsx::writeDataTable(sheet = "node", node)
        wb |> openxlsx::writeDataTable(sheet = "edge", edge)
        openxlsx::saveWorkbook(wb, file = file)
      }
    )
    # MAIN VISUALISATION + CUSTOM EVENT-----------------------------------------
    output$visNetworkId <- visNetwork::renderVisNetwork({
      req(!is.null(Graph$Main))
      g <- Graph$Main
      # this to should be done first before adding visNetwork default namespace
      V(g)$title <- pasteNodeDetails(g)
      E(g)$title <- pasteEdgeDetails(g)
      base_graph <- visNetwork::visIgraph(g, randomSeed = "3", type = "square") |>
        visNetwork::visOptions(
          clickToUse = T,
          collapse = T,
          manipulation = list(
            enabled = input$edit,
            addNodeCols = c("label")
            ),
          highlightNearest = list(
            enabled = T,
            degree = 0,
            algorithm = "hierarchical"
            )
          ) |>
        visNetwork::visEvents(selectNode = htmlwidgets::JS(sprintf("function(properties){
                  Shiny.setInputValue('%s',
                  this.body.data.nodes.get(properties.nodes[0]).id)
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
    # GRAPH EDITING LOGIC ------------------------------------------------------
    observeEvent(input$visNetworkId_graphChange, {
      req(!is.null(input$visNetworkId_graphChange$cmd))
      if(input$visNetworkId_graphChange$cmd == "addNode") {
        # ADD NODE
        id <- isolate(input$visNetworkId_graphChange$id)
        Graph$Current <- add_vertex_sf(Graph$Current, id)
      } else if (input$visNetworkId_graphChange$cmd == "addEdge") {
        id = as.character(length(E(Graph$Current)) + 1)
        from = isolate(input$visNetworkId_graphChange$from)
        to = isolate(input$visNetworkId_graphChange$to)
        tryCatch({
          Graph$Current <- Graph$Current + edge(c(from, to), id = id)
        }, error = function(e){
          print(sprintf("Error occur at: from: %s, to: %s, edge id: %s", from, to, id))
          print(e)
        })
      } else if (input$visNetworkId_graphChange$cmd == "editEdge") {
        id <- isolate(input$visNetworkId_graphChange$id)
        from = isolate(input$visNetworkId_graphChange$from)
        to = isolate(input$visNetworkId_graphChange$to)
        g <- Graph$Current
      # save attributes asided
        attrs <- edge_attr(g, index = id)
      # add and delete edges
        g <- g - edge(id)
        Graph$Current <- add_edges(g, c(from, to), attr = attrs)
      } else if (input$visNetworkId_graphChange$cmd == "deleteElements") {

        g <- Graph$Current
        edges <- isolate(unlist(input$visNetworkId_graphChange$edges))
        nodes <- isolate(unlist(input$visNetworkId_graphChange$nodes))
        g <- g - edge(edges)
        g <- igraph::delete_vertices(g, nodes)
        Graph$Current <- g
      }
    })
    # DEV AREA -----------------------------------------------------------------
    output$dev <- shiny::renderPrint({
      # print(input$click)
      print(paste("click node:", input$click_node, class(input$click_node)))
      print(paste("click edge (id):", input$click_edge, class(input$click_edge)))
      # print(paste("try find edge:", E(Graph$Current)[input$click_edge]))
      print(input$visNetworkId_graphChange)

      if(!is.null(input$visNetworkId_graphChange)) {
        if(input$visNetworkId_graphChange$cmd == "deleteElements") {
          print(input$visNetworkId_graphChange$edges |> unlist())
          print(unlist(input$visNetworkId_graphChanges$nodes))
        }
      }
      print(Graph$Current)
    })
    observe({
      if(dev) {
        shinyjs::show("dev")
      } else {
        shinyjs::hide("dev")
      }
    })
    # RETURN -------------------------------------------------------------------
    return(reactive(Graph$Main))
    # MODULE END ---------------------------------------------------------------
  })
}

## To be copied in the UI
# mod_visNetworkWrite_ui("visNetworkWrite_1")
## To be copied in the server
# mod_visNetworkWrite_server("visNetworkWrite_1")
