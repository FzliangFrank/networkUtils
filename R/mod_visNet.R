
#' visNet Server Functions
#' @name mod_visNet_
#' @param id shiny
#' @param graph_rct reactive expression of igraph
#' @param visNet_options list of option passed to `visSetOptions`
#' this could also be reactive
#' @param debug this flag will create a text output for tracking change.
#' @param layout igraph layout to put in `visNetwork::visIgraphLayout`
#' could be static or reactive.
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

#' @examples
#' if(interactive()) {
#'   library(shiny)
#'   g = igraph::make_tree(20, 4)
#'   V(g)$name = paste0('n_',seq(length(g)))
#'   V(g)$attr1 = seq(length(g)) |> as.double()
#'   E(g)$id = seq(length(E(g)))
#'   ui <- fluidPage(
#'     column(
#'       4,
#'       # UI for the control button set
#'       mod_visNetInteraction_ui('id')
#'     ),
#'     column(
#'       8,
#'       # UI for the main network graph
#'       mod_visNetModification_ui("id")
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     # generic server
#'     mod_visNet_server('id', reactive(g))
#'   }
#'
#'   shinyApp(ui, server)
#' }

