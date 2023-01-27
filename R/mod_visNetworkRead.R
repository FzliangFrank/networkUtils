#' visNetworkRead UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visNetworkReadControler_ui <- function(id){
  ns <- NS(id)
  tagList(
      selectizeInput(ns("nodeId"), "select a node by id", NULL),
      shinyWidgets::switchInput(ns("phy"), "enable physics", NULL),
      selectInput(ns("edgeAttrName"), "select an edge attribute", NULL),
      selectizeInput(ns("edgeAttr"), "query edge that are these", NULL)
  )
}
mod_visNetworkReadDisplay_ui <- function(id) {
  ns <- NS(id)
  visNetwork::visNetworkOutput(ns("NetworkWidget"))
}

# SERVER SIDE ------------------------------------------------------------------

#' NetworkDisplayServer

mod_visNetworkReadDisplay_server <- function(id, graph) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$NetworkWidget <- visNetwork::renderVisNetwork({
      g <- graph()
      # this to should be done first before adding visNetwork default namespace
      V(g)$title <- pasteNodeDetails(g)
      E(g)$title <- pasteEdgeDetails(g)
      # adding local visNetwork default namespace

      base_graph <- visNetwork::visIgraph(g, physics = input$phy) |>
        visNetwork::visOptions(
          highlightNearest = list(
            enabled = T,
            degree = 0,
            algorithm = "hierarchical"
          ))
    })
  })
}

#' visNetworkReadControler Server Functions
#'
#' @noRd
mod_visNetworkReadControler_server <- function(id, graph) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    selectNode <- reactive(input$nodeId)
    observe({
      edgeAttrNames <- igraph::edge_attr_names(graph())
      updateSelectizeInput(session, "nodeId", choices = c("ALL", V(graph())$name))
      updateSelectInput(session, "edgeAttrName", choices = c(edgeAttrNames))

      # attr <- igraph::edge_attr(graph(), input$edgeAttrNames)
      # updateSelectizeInput(session, "edgeAttr", choices = attr)
    })
    observe({
      if(selectNode() != "ALL") {
        visNetwork::visNetworkProxy(ns("NetworkWidget")) |>
          visNetwork::visSelectNodes(selectNode())
      }
    }, label = "select node")
    observe({
      attr <- igraph::edge_attr(graph(), input$edgeAttrName)
      updateSelectizeInput(session, "edgeAttr", choices = attr)
    })
    observe({
      edgeList <- igraph::as_edgelist(graph())
      edgeFound <- which(edge_attr(graph(), input$edgeAttrName) == input$edgeAttr)
      nodeFound <-
        c(edgeList[edgeFound, 1], edgeList[tail(edgeFound, 1), 2])
      visNetwork::visNetworkProxy(ns("NetworkWidget")) |>
        visNetwork::visSelectNodes(nodeFound)
    })
  })
}

## To be copied in the UI
# mod_visNetworkRead_ui("visNetworkRead_1")

## To be copied in the server
# mod_visNetworkRead_server("visNetworkRead_1")
