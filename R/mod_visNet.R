
#' visNet Server Functions
#' @name mod_visNet_
#' @param id shiny
#' @param graph_rct reactive expression of igraph
#' @param visNet_options list of option passed to `visSetOptions`
#' @param layout igraph layout to put in `visNetwork::visIgraphLayout`
#'
#' The easiest way to use both modification server and interaction server.
#' mod_visNet_server links both modification and interaction UI and by doing so
#' make it easy
#' @return reactive list
#'
#' @details
#' list of `Current`, `Main` `click_node` and `click_edge`
#' `Main` for graph that has been committed editing;
#' `Current` for graph on display;
#' Two additional `click_node` and `click_edge` let you track
#' edges and node currently clicked
#' @export
mod_visNet_server <- function(id,
                              graph_rct,
                              debug = F,
                              visNet_options = NULL,
                              layout = NULL,
                              NodeAttrTooltip = F,
                              EdgeAttrTooltip = F
                              ){
  domain = getDefaultReactiveDomain()
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    if(is.null(layout)) layout = 'layout_nicely'
    SessionGraph = mod_visNetModification_server(id, graph_rct,
                                                 dev = debug,
                                                 domain = domain,
                                                 NodeAttrTooltip = NodeAttrTooltip,
                                                 EdgeAttrTooltip = EdgeAttrTooltip,
                                                 visNet_options = visNet_options,
                                                 layout = layout
                                                 )
    Graph = reactive({
      req(!is.null(SessionGraph$Current))
      req(inherits(SessionGraph$Current, 'igraph'))
      SessionGraph$Current
    })
    mod_visNetInteraction_server(id, Graph, domain = domain)
    return(SessionGraph)
  })
}

## To be copied in the UI
# mod_visNet_ui("visNet_1")

## To be copied in the server
# mod_visNet_server("visNet_1")
