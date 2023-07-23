
#' visNet Server Functions
#' @param id shiny
#' @param graph_rct reactive expression of igraph
#' @param options
#' mod_visNet_server links both modification and interaction UI and by doing so
#' make it easy
#' @return list of `Current` and `Main`
#' `Main` for graph that has been committed editing;
#' `Current` for graph on display;
#' @export
mod_visNet_server <- function(id, graph_rct, debug = F, options){
  domain = getDefaultReactiveDomain()
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    SessionGraph = mod_visNetModification_server(id, graph_rct,
                                                 dev = debug,
                                                 domain = domain,
                                                 options = options
                                                 )
    mod_visNetInteraction_server(id, reactive(SessionGraph$Current), domain = domain)
    return(SessionGraph)
  })
}

## To be copied in the UI
# mod_visNet_ui("visNet_1")

## To be copied in the server
# mod_visNet_server("visNet_1")
