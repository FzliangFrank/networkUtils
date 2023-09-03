#' log_timeline_item
#'
#' @description A convenient function to help render a single bs4Dash::timeline_item
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
    content
  )
}
