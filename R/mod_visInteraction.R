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
      # ========= DIP (Development In Progress) ====
      # Insert Search UI
      shinyWidgets::searchInput(ns('searchBar'),
                                'Search Any Control Element',
                                placeholder = 'attr1 < 23; node_is_root()',
                                btnSearch = icon('search')
      ),
      # Here we want segregate node control with edge contorl
      shinyWidgets::radioGroupButtons(
        inputId = ns("graphSelector"),
        label = "Search in Node/Edge",
        choices = c(
          "Nodes",
          "Edges",
          "All"
                ),
        status = "primary",
        checkIcon = list(
          yes = icon("ok",
                     lib = "glyphicon")
          # no = icon("remove",
          #           lib = "glyphicon")
          )
      ),
      # ~====================================~
      # Advanced Options Control
      shinyWidgets::prettyCheckbox(ns('showAdvanced'), 'More options'),
      div(
      id = ns('advancedOpts'),
        # Color or Select
      shinyWidgets::prettyToggle(
        inputId = ns('paint'),
        label_on = "Fix Color..",
        label_off = "Select only..",
        outline = TRUE,
        plain = TRUE,
        inline = T,
        status_on = 'info',
        status_off = 'info',
        icon_on = icon("paint-roller"),
        icon_off = icon("hand-pointer")
      ),
        # Node Control
        div(
          id = ns('nodeSelectors'),
          selectInput(ns("nodeAttrName"), "Specify Node Attribute:", NULL),
          uiOutput(ns("nodeAttrUi")),
        ),
        # Edge Control
        div(
          id = ns('edgeSelectors'),
          selectInput(ns("edgeAttrName"), "Specify Edge Attribute:", NULL),
          uiOutput(ns("edgeAttrUi"))
        )
      )
  )
}
# mod_visNetworkReadDisplay_ui <- function(id) {
#   ns <- NS(id)
#   visNetwork::visNetworkOutput(ns("visNetworkId"))
# }

# SERVER SIDE ------------------------------------------------------------------

#' NetworkDisplayServer
#' visNetworkReadControler Server Functions
#' @param id id
#' @param igraph_rct reactive expression for igraph
#' @param e_ignore a vector of edge attributes name to ignore
#' @param v_ignore a vector of node attributes name to ignore
#' @param domain session for when nesting module
#' @description
#' This module let you interact with graph
#' Require visnetwork rendered in shiny to have base id `visNetworkId`
#' @export
mod_visNetInteraction_server <- function(
    id,
    igraph_rct,
    e_ignore = c(),
    v_ignore = c(),
    show_hidden = F,
    domain = getDefaultReactiveDomain()
    ) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # =========== DIP ==================
    # Dynamic UI to uncollabse more functions
    observe(label = "Advanced Functions ",
      {
      shinyjs::toggle("advancedOpts", condition = input$showAdvanced)
    })
    observe({
      selected = input$graphSelector
      cond = selected == 'Edges' || selected == "All"
      shinyjs::toggle("edgeSelectors",anim = T, condition = cond)
    })
    observe({
      selected = input$graphSelector
      cond = selected == 'Nodes' || selected == 'All'
      shinyjs::toggle("nodeSelectors",anim = T, condition = cond)
    })
    observe({
      selected = input$graphSelector
      cond = selected == 'All'
      shinyjs::toggle("paint", anim = T, condition = cond)
    })
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
      if(!show_hidden) {
        # if hide attr start with a . append those ones to e_ignore
        e_ignore = c(e_ignore, (edge_attr_names(g) |> stringr::str_detect('^\\.')))
        v_ignore = c(v_ignore, (vertex_attr_names(g) |> stringr::str_detect('^\\.')))
      }
      edgeAttrNames <- igraph::edge_attr_names(g) |>
        purrr::discard(~.x %in% e_ignore)
      nodeAttrNames <- igraph::vertex_attr_names(g) |>
        purrr::discard(~.x %in% v_ignore)
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
      golem::print_dev('Creating node UI..')
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
    NodeFound = reactiveValues(id = NULL)
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
          E(g)[which(edgeAttr() >= inbond & edgeAttr() <= outbond)] |> as_ids()
        })
      } else {
        req(length(input$edgeAttr)==1)
        # message("single edge.attr selected")
        edgeFound <- E(g)[which(edgeAttr() == input$edgeAttr)] |> as_ids()
      }
      if(inherits(edgeFound, 'try-error')) return()
      nodeFound <- V(g)[.inc(edgeFound)] |> as_ids()
      NodeFound$id = nodeFound
      # visNetwork::visNetworkProxy(ns("visNetworkId")) |>
      #   visNetwork::visSelectNodes(nodeFound)
    })
    # Node Selector ---------------------------------------------------------
    observe(label = "Node Selecter", {
      g <- isolate(graph())
      req(input$nodeAttr)
      req(!is.null(input$nodeAttr))
      # DEBUG
      # golem::print_dev(sprintf("Enter node selector: %s", typeof(nodeAttr() )))
      # cur_attr_name = isolate(input$nodeAttrName)
      if(typeof(nodeAttr()) =="double") {
        req(length(input$nodeAttr) == 2) #whenever UI render this notify
        inbond=input$nodeAttr[1]
        outbond=input$nodeAttr[2]
        nodeFound <- V(g)[which(nodeAttr() >= inbond & nodeAttr() <= outbond)]
      } else if(nodeAttr() |> inherits("sfc_POINT")) {
        req(input$nodeAttr |> inherits('list'))
        req(input$nodeAttr$xmin)
        req(input$nodeAttr$xmin)
        message("geometry selected")
        cur = input$nodeAttr
        brash_area = sf::st_bbox(c(xmin =cur$xmin, xmax = cur$xmax,
                               ymin=cur$ymin, ymax =cur$ymax)) |>
          sf::st_as_sfc()
        nodeFound = V(g)[sf::st_intersects(sf::st_as_sf(nodeAttr()), brash_area, sparse=F) |> which()]
      } else if(length(input$nodeAttr) == 1) {
        req(length(input$nodeAttr) == 1 )
        nodeFound <- V(g)[which(as.character(nodeAttr()) == input$nodeAttr)]
      } else if(length(input$nodeAttr) > 1) {
        nodeFound <- V(g)[which(as.character(nodeAttr()) %in% input$nodeAttr)]
      } else {
        message('unrecognised graph input')
        nodeFound = NULL
      }
      NodeFound$id <- nodeFound |> as_ids()
    })
    observeEvent(input$searchBar,{
      # golem::print_dev('search bar in context %s', input$graphSelector)
      g <- isolate(graph())
      search_input = tolower(input$graphSelector)
      req(search_input %in% c("nodes", "edges"))
      nodeFound = search_idx(g,
                             input$searchBar,
                             search_in = search_input,
                             as_ids = T)
      # golem::print_dev(sprintf('search bar found: %s', paste(nodeFound, collapse = ',')))
      NodeFound$id = nodeFound
    })
    observe({
      # Inject if Select Or Color Selected node
      # ============ DIP ================
      # if() {
      visNetwork::visNetworkProxy(ns("visNetworkId")) |>
        visNetwork::visSelectNodes(NodeFound$id)
      # } else {
      # visNetwork::visNetworkProxy(ns("visNetworkId")) |>
      # visNetwork::visNodes(id = nodeFound)
      # }
      # =================================
    })
  }, session = domain)
}

## To be copied in the UI
# mod_visNetworkRead_ui("visNetworkRead_1")

## To be copied in the server
# mod_visNetworkRead_server("visNetworkRead_1")
