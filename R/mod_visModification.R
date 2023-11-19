
#' visNetworkWrite UI Function
#'
#' @description A shiny Module.
#' @name mod_visNet_Modifictaion
#' @importFrom shiny NS tagList
#' @export
mod_visNetModification_ui <- function(id, useJQ=F, dev=F){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyWidgets::prettyCheckbox(ns("edit"), "Edit", animation = "smooth", status = 'primary',inline = T),
    shinyWidgets::prettyCheckbox(ns('phy'), 'Physics', animation = "smooth", inline = T),
    p("You are in editing mode, exit without save will revert to original", id = ns("note")),
    # /DEV/ ======
    # Vacumn this component that this goes into visNetworkOutput
    #shinyjqui::jqui_resizable(
      # visNetwork::visNetworkOutput(ns("visNetworkId"), width = "100%"),
      #options = list(handles = "e,s,n,w")
    #),
    {if(useJQ) {
      shinyjqui::jqui_resizable(options = list(handles = "s,e"),
        visNetwork::visNetworkOutput(ns("visNetworkId"), width = "100%")
      )
    } else {
      visNetwork::visNetworkOutput(ns("visNetworkId"), width = "100%")
    }},
    # \DEV\=========
    div(id=ns('editor'),
      wellPanel(
        actionButton(ns("save"), "Commit Change"),
        downloadButton(ns("export"))# build in export option
      )
    ),
    shiny::uiOutput(ns("AttrEditor")),
    if(dev) {
      shiny::verbatimTextOutput(ns("dev"))
    }
  )
}

#' visModification Server Functions
#' @rdname mod_visNet_Modifictaion
#' @param id shiny server id
#' @param igraphObj a reactive graph object
#' @param domain session
#' @param dev open a printer
#' @param hard_delete when this flag is turn off, no edge can be actually deleted/
#' @param visNet_options list of option passed to `visSetOptions`
#' @param layout igraph layout to put in `visNetwork::visIgraphLayout`
#' @param NodeAttrTooltip,EdgeAttrTooltip when these two flag are set to ture
#' attributes will be automatically parsed into tooltips, following formula
#' 'attrName: attr'. There is no good parsing for time series type.
#' @param offset number of pixel to off set when use maixmize button
#' @return reactiveValues $Curent and $Main and more
#' @details
#' $Current is a reactive igraph Object that every is being modified now
#' $Main is the igraph Object that has been committed and saved
#' In addition it return a set of `reactiveValues` which monitor graph changes
#' and track node that is currently clicked.
#'
#' Two utility function are added `maximize_helper` return a javascript allow you
#' to resize monitored object to full size. You can use this script on any shiny
#' widget (where you know id), when you use `bs4Dash::box`
#'
#' To resize this network specifically add this script below `bs4Dash::box`
#' `tags$script(maximize_helper(visNetId('<id>')))`
#'
#' @export
mod_visNetModification_server <- function(
    id,
    igraphObj,
    dev = F,
    hard_delete = T,
    NodeAttrTooltip = T,
    EdgeAttrTooltip = T,
    domain = getDefaultReactiveDomain(),
    visNet_options = NULL,
    layout = 'layout_nicely'
  ){
  # stop if not reactive
  stopifnot(igraphObj |> is.reactive())
  valid_layout = c(
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
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    layout_input = reactive(label ='layout input', {
      if(!is.reactive(layout)) {
        if(layout %in% valid_layout) {
          layout
        } else {
          warning("Static layout is not on the list")
          'layout_nicely'
        }
      } else {
        if(layout() %in% valid_layout) {
          layout()
        } else {
          warning('Reactive layout is not on the the list')
          'layout_nicely'
        }
      }
    })
    option_input = reactive(label = 'option input',{
      if(is.reactive(visNet_options)) {
        message('visNetOptions is reactive')
        visNet_options()
      } else {
        visNet_options
      }
    })
    # DYNAMIC UI ---------------------------------------------------------------
    observeEvent(input$edit, {
      shinyjs::toggle("editor",T)
      # shinyjs::toggle("save", T)
      # shinyjs::toggle("export", T)
      shinyjs::toggle("note", T)
    })
    # STAGE  -------------------------------------------------------------------
    # curGraph <- reactiveValues(g = NULL)
    # mainGraph <- reactiveValues(g = NULL)

    Graph <- reactiveValues(
      Current = NULL,
      Main = NULL,
      click_node = NULL,
      click_edge = NULL,
      editing = list(NULL),
      commit = NULL
    )
    observe({
      g <- igraphObj()
      req(!is.null(g))
      # id will be used by igraph to pick up edges. It has to be a numeric vector
      # other input will cause function `edge()` to crash
      # Needs to clean up on session end?
        if("id" %in% vertex_attr_names(g)) V(g)$.id <- V(g)$id
        if("id" %in% edge_attr_names(g)) E(g)$.id <- E(g)$id
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
      if(dev) message("current write into main")
      # shinyWidgets::updatePrettyCheckbox(session = session,inputId = "edit", status = 'success')
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
        if(".id" %in% vertex_attr_names(g)) {
          V(g)$id <- V(g)$.id
          delete_vertex_attr(g, ".id")
        }
        if(".id" %in% edge_attr_names(g)) {
          E(g)$id <- E(g)$.ir_id
          delete_edge_attr(g, ".id")
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
    visNetObj <- reactive({
      req(!is.null(Graph$Main))
      g <- Graph$Main
      # this to should be done first before adding visNetwork default namespace
      if(NodeAttrTooltip) V(g)$title <- pasteNodeDetails(g)
      if(EdgeAttrTooltip) E(g)$title <- pasteEdgeDetails(g)
      if(dev) message(sprintf("rendering graph using layout %s", layout_input()))
      # /Dev/ This part to be replaced by a customer renderer?
      g |>
        visNetwork::visIgraph(
          idToLabel = F,
          randomSeed = "3",
          type = "square",
          layout = layout_input(),
          physics = input$phy,
          smooth = input$phy
        ) |>
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
        # Custom Events
        visNetwork::visEvents(
          selectNode = htmlwidgets::JS(sprintf("function(properties){
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
        ) |>
        visNetwork::visSetOptions(options = option_input())
    })
    output$visNetworkId <- visNetwork::renderVisNetwork({
      visNetObj()
    })
    observe({Graph$click_node = input$click_node})
    observe({Graph$click_edge = input$click_edge})
    # GRAPH EDITING LOGIC ------------------------------------------------------
    observeEvent(input$visNetworkId_graphChange, {
      req(!is.null(input$visNetworkId_graphChange$cmd))
      G = Graph$Current
      G = try(modify_graph_i(G, input$visNetworkId_graphChange, hard_delete = hard_delete))
      if(G |> inherits('try-error')) {
        warning(sprintf(
          "error occured when %s",
          input$visNetworkId_graphChange$cmd
          ))
        G = Graph$Current
        shinyjs::click('save')
      } else {
        Graph$editing = input$visNetworkId_graphChange
        Graph$Current = G
        # shinyWidgets::updatePrettyCheckbox(session = session, inputId = 'edit')
      }
    })
    observe({Graph$commit = input$save})
    # DEV AREA -----------------------------------------------------------------
    if(dev) {
      output$dev <- shiny::renderPrint({
        if(dev) {
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
        }
      })
    }
    # RETURN -------------------------------------------------------------------
    return(Graph)
    # MODULE END ---------------------------------------------------------------
  }, session = domain)
}

# UTILITY ======================================================================


## To be copied in the UI
# mod_visNetworkWrite_ui("visNetworkWrite_1")
## To be copied in the server
# mod_visNetworkWrite_server("visNetworkWrite_1")
