#' visNetInteraction
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' a set of controller that allows interact with graphic
#'
#' @importFrom shiny NS tagList
#' @export
mod_visNetInteraction_ui <- function(id){
  ns <- NS(id)
  tagList(
      # Insert Search UI
      # shinyWidgets::searchInput(ns('searchAny'),
      #                           'Search Any Control Element',
      #                           btnSearch = icon('search')
      #                           ),
      # ===========DIP (Development In Progress) ==========
      # Here we want segregate node control with edge contorl
      # radioGroupButtons(
      #   inputId = "searchIn",
      #   label = "Search in Node/Edge",
      #   choices = c("Node",
      #               "Edge"),
      #   status = "primary",
      #   checkIcon = list(
      #     yes = icon("ok",
      #                lib = "glyphicon"),
      #     no = icon("remove",
      #               lib = "glyphicon"))
      # ),
      # ================== DIP ==================
      # Advanced Options Control
      # shinyWidgets::prettyCheckbox(ns('showAdvanced'), 'Show Advanced Options'),
      div(
        id = ns('advancedOpts'),
        # Node Control
        selectInput(ns("nodeAttrName"), "Node attribute to query", NULL),
        uiOutput(ns("nodeAttrUi")),
        # Edge Control
        selectInput(ns("edgeAttrName"), "Edge attribute to query", NULL),
        uiOutput(ns("edgeAttrUi"))
      )
  )
}
# mod_visNetworkReadDisplay_ui <- function(id) {
#   ns <- NS(id)
#   visNetwork::visNetworkOutput(ns("visNetworkId"))
# }

# SERVER SIDE ------------------------------------------------------------------

#' NetworkDisplayServer

# mod_visNetworkReadDisplay_server <- function(id, graph) {
#   moduleServer(id, function(input, output, session){
#     ns <- session$ns
#     output$visNetworkId <- visNetwork::renderVisNetwork({
#       g <- graph()
#       # this to should be done first before adding visNetwork default namespace
#       V(g)$title <- pasteNodeDetails(g)
#       E(g)$title <- pasteEdgeDetails(g)
#       # adding local visNetwork default namespace
#       base_graph <- visNetwork::visIgraph(g, physics = T) |>
#         visNetwork::visOptions(
#           highlightNearest = list(
#             enabled = T,
#             degree = 0,
#             algorithm = "hierarchical"
#           ))
#     })
#   })
# }
#' visNetworkReadControler Server Functions
#' @param id id
#' @param igraph_rct reactive expression for igraph
#' @param e_ignore a vector of edge attributes name to ignore
#' @param v_ignore a vector of node attributes name to ignore
#' @description
#' This module let you interact with graph
#' Require visnetwork rendered in shiny to have base id `visNetworkId`
#' @export
mod_visNetInteraction_server <- function(
    id,
    igraph_rct,
    e_ignore = c(),
    v_ignore = c()
    ) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # =========== DIP ==================
    # Dynamic UI to uncollabse more functions
    # observe(label = "Advanced Functions ",
    #   {
    #   shinyjs::toggle("advancedOpts", condition = input$showAdvanced)
    # })
    # =========== DIP =================
    # Data Transfer
    graph = reactive(label='Validate Graph', {
      golem::print_dev("Validating input")
      req(!is.null(igraph_rct()) )
      req(inherits(igraph_rct(), "igraph") )
      golem::print_dev("Graph validated")
      igraph_rct()
    })
    selectNode <- reactive(input$nodeId)
    observe(label='Populate Attribute Names', {
      g <- graph()
      golem::print_dev("Populating attribute name")
      edgeAttrNames <- igraph::edge_attr_names(g) |> purrr::discard(~.x %in% e_ignore)
      nodeAttrNames <- igraph::vertex_attr_names(g) |> purrr::discard(~.x %in% v_ignore)
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
        # message("double edge.attr selected")
        inbond=input$edgeAttr[1]
        outbond=input$edgeAttr[2]
        edgeFound <- try({
          which(edgeAttr() >= inbond & edgeAttr() <= outbond)
        })
      } else {
        req(length(input$edgeAttr)==1)
        # message("single edge.attr selected")
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
      # message(sprintf("node is type of %s", typeof(nodeAttr())))
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
