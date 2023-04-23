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
      uiOutput(ns("nodeAttrUi")),
      selectInput(ns("edgeAttrName"), "Edge attribute to query", NULL),
      uiOutput(ns("edgeAttrUi"))
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
    graph = reactive(label='Validate Graph', {
      req(!is.null(igraph_rct()) )
      req(inherits(igraph_rct(), "igraph") )
      igraph_rct()
    })
    selectNode <- reactive(input$nodeId)
    observe(label='Update Attribute Names', {
      g <- graph()
      edgeAttrNames <- igraph::edge_attr_names(g)
      nodeAttrNames <- igraph::vertex_attr_names(g)
      updateSelectInput(session, "nodeAttrName", choices = c(nodeAttrNames))
      updateSelectInput(session, "edgeAttrName", choices = c(edgeAttrNames))
    })
    edgeAttr = reactive({
      igraph::edge_attr(isolate(graph()), input$edgeAttrName)
    })
    nodeAttr = reactive({
      igraph::vertex_attr(isolate(graph()), input$nodeAttrName)
    })
    output$edgeAttrUi <- renderUI({
      edgeAttrLabel=glue::glue("Query edge attributes fits these")
      if(typeof(edgeAttr())=='double') {
        # Resolution is seet to be 0.01 this is not always Correct
        sliderInput(ns("edgeAttr"), edgeAttrLabel,
                    min=blurry_range(edgeAttr())[1], max=blurry_range(edgeAttr())[2],
                    value=c(quantile(edgeAttr(), 0.33), quantile(edgeAttr(), 0.66)))
      } else {
        selectizeInput(ns("edgeAttr"), edgeAttrLabel,
                       choices=edgeAttr(), selected = edgeAttr()[1]) # if selected is NULL app crash when switching to a numeric input
      }
    })
    output$nodeAttrUi <- renderUI({
      nodeAttrLabel ="Query node attributes fits these"
      if(typeof(nodeAttr())=="double") {
        sliderInput(ns("nodeAttr"), nodeAttrLabel,
                    min=blurry_range(nodeAttr())[1], max=blurry_range(nodeAttr())[2],
                    value=c(quantile(nodeAttr(), 0.33), quantile(nodeAttr(), 0.66))
        )
      } else {
        selectizeInput(ns('nodeAttr'),
                       nodeAttrLabel,
                       choices=nodeAttr(),
                       selected=nodeAttr()[1]
                       )
      }
    })
    # observe({
    #   nodeAttr <- igraph::vertex_attr(graph(), input$nodeAttrName)
    #   updateSelectizeInput(session, "nodeAttr", choices = nodeAttr)
    # })
    observe(label= "Edge Selecter", {
      g <- isolate(graph())
      edgeList <- igraph::as_edgelist(g)
      edgeAttrType=typeof(edgeAttr())
      if(edgeAttrType == 'double') {
        req(length(input$edgeAttr)==2)
        inbond=input$edgeAttr[1]
        outbond=input$edgeAttr[2]
        edgeFound <- E(g)[.data[[isolate(input$edgeAttrName)]] >= inbond
                              & .data[[isolate(input$edgeAttrName)]] <= outbond]
      } else {
        req(length(input$edgeAttr)==1)
        edgeFound <- E(g)[.data[[isolate(input$edgeAttrName)]] == input$edgeAttr]
      }
      nodeFound <- V(g)[.inc(edgeFound)] |> as_ids()
        # c(edgeList[edgeFound, 1], edgeList[tail(edgeFound, 1), 2])
      visNetwork::visNetworkProxy(ns("visNetworkId")) |>
        visNetwork::visSelectNodes(nodeFound)
    })
    observe(label = "Node Selecter", {
      g <- isolate(graph())
      req(input$nodeAttr)
      cur_attr_name = isolate(input$nodeAttrName)
      if(typeof(nodeAttr())=="double") {
        req(length(input$nodeAttr) == 2) #whenever UI render this notify
        inbond=input$nodeAttr[1]
        outbond=input$nodeAttr[2]
        nodeFound <- V(g)[.data[[cur_attr_name]] >= inbond
                              & .data[[cur_attr_name]] <= outbond
                                ] |> as_ids()
      } else {
        req(length(input$nodeAttr)==1)
        message(cur_attr_name)
        nodeFound <- which(vertex_attr(g, cur_attr_name) == input$nodeAttr)
      }
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
