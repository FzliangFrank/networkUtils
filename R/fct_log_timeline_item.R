#' log_timeline_item
#'
#' @description A convenient function to help render
#' a single bs4Dash::timeline_item. Useful for visualise version change
#' of a graph modified.
#' @param chglog_1
#' A list of two item change and item
#' @return timeline_item UI component
#' @examples
#' graph_chg_log = list(
#'   time = '2021-01-05',
#'   change = list(
#'     cmd = 'addEdge',
#'     from= 1,
#'     to = 2
#'   )
#' )
#' log_timeline_item(graph_chg_log)
#'
#' if(FALSE) {
#'   library(shiny)
#'   library(visNetwork)
#'   library(igraph)
#'   library(bs4Dash)
#'   devtools::load_all()
#'
#'   ccl = make_ring(10, T)
#'   log_timeline_item = function(chglog_1) {
#'     if(chglog_1 |> is.null()) return(NULL)
#'     chglog_1$change -> x
#'     chglog_1$time -> time
#'     content = jsonlite::toJSON(x, auto_unbox = T, simplifyMatrix = T) |>
#'       jsonlite::prettify()
#'
#'     if(is.null(x) || is.na(x) || length(x) == 0) {
#'       return(timelineStart())
#'     } else if(x$cmd == 'addNode') {
#'       icon = icon("circle-plus")
#'       title = "add node"
#'       color = 'success'
#'     } else if (x$cmd == 'addEdge') {
#'       icon = icon('ruler')
#'       title = "add edge"
#'       color = 'info'
#'       content = p('add edge from:', x$from, 'to', x$to)
#'     } else if (x$cmd == 'deleteElements') {
#'       icon = icon("trash")
#'       title = sprintf("deleted %i element", length(x$nodes) + length(x$edges))
#'       color = 'maroon'
#'     } else if (x$cmd == 'editEdge') {
#'       icon = icon("pencil")
#'       color = 'indigo'
#'       title = "edge edited"
#'       content = paste(x$from, '-->',x$to)
#'     } else {
#'       stop("unrecognised command")
#'     }
#'     timelineItem(
#'       icon = icon,
#'       time = time,
#'       title = title,
#'       color = color,
#'       content
#'     )
#'   }
#'   # basic UI
#'   # dashboard UI
#'   ui <- bs4Dash::dashboardPage(
#'     header = dashboardHeader(
#'       title = 'Graph Change Tracker',
#'       tags$head(
#'         tags$script('window.onresize = function() {network.fit();}')
#'       )
#'     ),
#'     sidebar = dashboardSidebar(),
#'     controlbar = dashboardControlbar(
#'       controlbarMenu(
#'         type = 'hidden',
#'         controlbarItem(
#'           title = 'Find Node',
#'           icon = icon('magnifying-glass-arrow-right'),
#'           mod_visNetInteraction_ui('visNet'),
#'         )
#'       ),
#'       width = 300
#'     ),
#'     body = dashboardBody(
#'       fluidRow(
#'         sortable(
#'           width = 6,
#'           box(
#'             width = 12,
#'             style = 'overflow-y: scroll;',
#'             title = 'Network',
#'             maximizable = T,
#'             height = 200,
#'             mod_visNetModification_ui('visNet')
#'           ),
#'         ),
#'         sortable(
#'           width = 6,
#'           box(
#'             width = 12,
#'             title = 'Time Line',
#'             div(
#'               style = 'overflow: scroll',
#'               height = '300px',
#'               uiOutput("timeline_ui")
#'             )
#'           )
#'         )
#'       ),
#'       box(
#'         title = "Dev Log",
#'         verbatimTextOutput('dev')
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     graph = reactive(ccl)
#'     G = mod_visNet_server('visNet', graph)
#'     clicked_node = reactive({
#'       G$click_node
#'     })
#'     change_log = reactiveValues(log=NULL)
#'     # create a series of change logs based on whenever a
#'     # change happens.
#'     observe({
#'       req(G$editing)
#'       req(!is.null(G$editing$cmd) || is.na(G$editing))
#'       change_log$log = isolate(change_log$log) |>
#'         append(list(list(
#'           time = Sys.time(),
#'           change = G$editing
#'         )))
#'     })
#'     output$dev <- renderPrint({
#'       print(clicked_node())
#'       print(change_log$log)
#'       print(change_log$log |>
#'               jsonlite::toJSON(auto_unbox = T, simplifyMatrix = T) |>
#'               jsonlite::prettify())
#'     })
#'     output$save = downloadHandler(
#'       filename = function() {
#'         paste(Sys.time(), '.json')
#'       },
#'       content = function(file) {
#'         change_log$log |>
#'           jsonlite::toJSON() |>
#'           jsonlite::write_json(file)
#'       }
#'     )
#'     output$timeline_ui <- renderUI({
#'       timelineBlock(
#'         width = 12,
#'         purrr::map(
#'           change_log$log,
#'           log_timeline_item
#'         )
#'       )
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#' @export
log_timeline_item = function(chglog_1) {
  if(chglog_1 |> is.null()) return(NULL)
  chglog_1$change -> x
  chglog_1$time -> time
  content = jsonlite::toJSON(x, auto_unbox = T, simplifyMatrix = T) |>
    jsonlite::prettify()

  if(length(x) == 0 || is.null(x$cmd)) {
    return(bs4Dash::timelineStart())
  } else if(x$cmd == 'addNode') {
    icon = icon("circle-plus")
    title = "add node"
    color = 'success'
    content = purrr::imap_chr(
      purrr::discard_at(x,'cmd'),
      ~paste0(.y, ': ',.x)
    ) |>
      paste(collapse = '<br>') |>
      HTML()
  } else if (x$cmd == 'addEdge') {
    icon = icon('ruler')
    title = "add edge"
    color = 'info'
    content = p('add edge from:', x$from, 'to', x$to)
  } else if (x$cmd == 'deleteElements') {
    icon = icon("trash")
    title = sprintf("deleted %i element", length(x$nodes) + length(x$edges))
    color = 'maroon'
  } else if (x$cmd == 'editEdge') {
    icon = icon("pencil")
    color = 'indigo'
    title = "edge edited"
    content = paste(x$from, '-->',x$to)
  } else {
    stop("unrecognised command")
  }
  bs4Dash::timelineItem(
    icon = icon,
    time = time,
    title = title,
    color = color,
    content,
    style='overflow: auto;'
  )
}
