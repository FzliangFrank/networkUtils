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
      # shinyWidgets::switchInput(ns("phy"), "enable physics", NULL),
      selectInput(ns("nodeAttrName"), "Node attribute to query", NULL),
      selectizeInput(ns("nodeAttr"), "Query node attributes fits these", NULL),
      selectInput(ns("edgeAttrName"), "Edge attribute to query", NULL),
      selectizeInput(ns("edgeAttr"), "Query edge attributes fits these", NULL)
  )
}
mod_visNetworkReadDisplay_ui <- function(id) {
  ns <- NS(id)
  visNetwork::visNetworkOutput(ns("visNetworkId"))
}

# SERVER SIDE ------------------------------------------------------------------

#' NetworkDisplayServer

mod_visNetworkReadDisplay_server <- function(id, graph) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$visNetworkId <- visNetwork::renderVisNetwork({
      g <- graph()
      # this to should be done first before adding visNetwork default namespace
      V(g)$title <- pasteNodeDetails(g)
      E(g)$title <- pasteEdgeDetails(g)
      # adding local visNetwork default namespace

      base_graph <- visNetwork::visIgraph(g, physics = T) |>
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
mod_visNetworkReadControler_server <- function(id, igraph_rct) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    graph = reactive({
      req(!is.null(igraph_rct()) )
      req(inherits(igraph_rct(), "igraph") )
      igraph_rct()
    })
    selectNode <- reactive(input$nodeId)
    observe({
      edgeAttrNames <- igraph::edge_attr_names(graph())
      nodeAttrNames <- igraph::vertex_attr_names(graph())
      updateSelectInput(session, "nodeAttrName", choices = c(nodeAttrNames))
      updateSelectInput(session, "edgeAttrName", choices = c(edgeAttrNames))

      # attr <- igraph::edge_attr(graph(), input$edgeAttrNames)
      # updateSelectizeInput(session, "edgeAttr", choices = attr)
    })
    observe({
      edgeAttr <- igraph::edge_attr(graph(), input$edgeAttrName)
      updateSelectizeInput(session, "edgeAttr", choices = edgeAttr)
    })
    observe({
      nodeAttr <- igraph::vertex_attr(graph(), input$nodeAttrName)
      updateSelectizeInput(session, "nodeAttr", choices = nodeAttr)
    })
    observe({
      edgeList <- igraph::as_edgelist(graph())
      edgeFound <- which(edge_attr(graph(), input$edgeAttrName) == input$edgeAttr)
      nodeFound <-
        c(edgeList[edgeFound, 1], edgeList[tail(edgeFound, 1), 2])
      visNetwork::visNetworkProxy(ns("visNetworkId")) |>
        visNetwork::visSelectNodes(nodeFound)
    })
    observe({
      nodeList <- V(graph())
      nodeFound <- which(vertex_attr(graph(), input$nodeAttrName) == input$nodeAttr)
      visNetwork::visNetworkProxy(ns("visNetworkId")) |>
        visNetwork::visSelectNodes(nodeFound)
    })
    observe({
      visNetwork::visNetworkProxy(ns("visNetworkId")) |>
        visNetwork::visPhysics(
          solver = 'barnesHut',
          enabled = input$phy)
    })
  })
}

## To be copied in the UI
# mod_visNetworkRead_ui("visNetworkRead_1")

## To be copied in the server
# mod_visNetworkRead_server("visNetworkRead_1")
