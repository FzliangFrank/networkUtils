#' visNetworkRead UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @importFrom shiny NS tagList
#' @export
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
#' @param id id
#' @param igraph_rct reactive expression for igraph
#' @export
mod_visNetworkReadControler_server <- function(id, igraph_rct) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    graph = reactive(label='Validate Graph', {
      req(!is.null(igraph_rct()) )
      req(inherits(igraph_rct(), "igraph") )
      igraph_rct()
    })
    selectNode <- reactive(input$nodeId)
    observe(label='Populate Attribute Names', {
      g <- graph()
      edgeAttrNames <- igraph::edge_attr_names(g)
      nodeAttrNames <- igraph::vertex_attr_names(g)
      updateSelectInput(session, "nodeAttrName", choices = c(nodeAttrNames))
      updateSelectInput(session, "edgeAttrName", choices = c(edgeAttrNames))
    })
    edgeAttr = reactive({
      req(input$edgeAttrName)
      igraph::edge_attr(graph(), input$edgeAttrName)
    })
    nodeAttr = reactive({
      req(input$nodeAttrName)
      igraph::vertex_attr(graph(), input$nodeAttrName)
    })
    # DETERMINE UI -------------------------------------------------------------
    output$edgeAttrUi <- renderUI({
      edgeAttrLabel=glue::glue("Query edge attributes fits these")
      if(typeof(edgeAttr())=='double') {
        # Resolution is seet to be 0.01 this is not always Correct
        sliderInput(ns("edgeAttr"), edgeAttrLabel,
                    min=blurry_range(edgeAttr())[1], max=blurry_range(edgeAttr())[2],
                    value=c(quantile(edgeAttr(), 0.33,na.rm=T), quantile(edgeAttr(), 0.66, na.rm=T)))
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
      } else if(inherits(nodeAttr(), "sfc")) {
        plotOutput(ns("nodeAttrPlot"), brush=ns("nodeAttr"),
                   inline=F,
                   height='200px')
      } else {
        selectizeInput(ns('nodeAttr'),
                       nodeAttrLabel,
                       choices=nodeAttr(),
                       selected=nodeAttr()[1]
                       )
      }
    })
    observe({
      nodeAttrs = nodeAttr()
      if(inherits(nodeAttrs, "sfc_POINT")) {
        nodeAttrs = nodeAttrs |> purrr::discard(sf::st_is_empty)
        output$nodeAttrPlot <- renderPlot({
          sf::st_as_sf(nodeAttrs) |> plotPPPdensity()
        }, bg='transparent')
      } else if(inherits(nodeAttr, "sfc")) {
        output$nodeAttrPlot <- renderPlot({
          sf::st_as_sf(nodeAttrs) |> plot()
        })
      } else {
        NULL
      }
    })
    # SELECTING NODE/EDGE BASED ON ATTRIBUTES ----------------------------------
    observe(label= "Edge Selecter", {
      g <- isolate(graph())
      req(input$edgeAttr)
      edgeList <- igraph::as_edgelist(g)
      edgeAttrType=typeof(edgeAttr())
      if(edgeAttrType == 'double') {
        req(length(input$edgeAttr)==2)
        message("double edge.attr selected")
        message(input$edgeAttrName)
        message(input$edgeAttr)
        inbond=input$edgeAttr[1]
        outbond=input$edgeAttr[2]
        edgeFound <- try({
          which(edgeAttr() >= inbond & edgeAttr() <= outbond)
        })
      } else {
        req(length(input$edgeAttr)==1)
        message("single edge.attr selected")
        edgeFound <- which(edgeAttr() == input$edgeAttr)
      }
      if(inherits(edgeFound, 'try-error')) return()
      nodeFound <- V(g)[.inc(edgeFound)] |> as_ids()
        # c(edgeList[edgeFound, 1], edgeList[tail(edgeFound, 1), 2])
      visNetwork::visNetworkProxy(ns("visNetworkId")) |>
        visNetwork::visSelectNodes(nodeFound)
    })
    observe(label = "Node Selecter", {
      g <- isolate(graph())
      req(input$nodeAttr)
      req(!is.null(input$nodeAttr))
      # DEBUG
      message(sprintf("node is type of %s", typeof(nodeAttr())))
      # cur_attr_name = isolate(input$nodeAttrName)
      if(typeof(nodeAttr())=="double") {
        req(length(input$nodeAttr) == 2) #whenever UI render this notify
        inbond=input$nodeAttr[1]
        outbond=input$nodeAttr[2]
        nodeFound <- which(nodeAttr() >= inbond & nodeAttr() <= outbond)
      } else if(nodeAttr() |> inherits("sfc_POINT")) {
        req(input$nodeAttr |> inherits('list'))
        req(input$nodeAttr$xmin)
        req(input$nodeAttr$xmin)
        message("geometry selected")
        cur = input$nodeAttr
        # debug
        # debug/
        brash_area = sf::st_bbox(c(xmin =cur$xmin, xmax = cur$xmax,
                               ymin=cur$ymin, ymax =cur$ymax)) |>
          sf::st_as_sfc()
        nodeFound = sf::st_intersects(sf::st_as_sf(nodeAttr()), brash_area, sparse=F) |> which()
      } else {
        req(length(input$nodeAttr)==1)
        nodeFound <- which(nodeAttr() == input$nodeAttr)
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
