#' modify_graph
#'
#' @description
#' Modify graph from a single command line
#' @importFrom igraph vertex
#' @importFrom igraph delete_vertex_attr
#' @importFrom igraph set_vertex_attr
#' @importFrom igraph V
#' @importFrom igraph E
#' @importFrom igraph `E<-`
#' @importFrom igraph `V<-`
#' @importFrom igraph add_edges
#' @importFrom igraph edge
#' @importFrom igraph edge_attr
#' @param igraphObj object that inherit igraph
#' @param visNetwork_graphChange a single list of command as a result of
#' visnetwork_graphChange, explained in their package
#' This list should always has following element:
#' - *addNode*
#' - *addEdge*
#' - *editEdge*
#' - *deleteElements*
#' If you ever need side effect, it is possible to add in your function
#' every time you make some change, add a new attribute to node are edge.
#' (for example a time stamp) so you can trick what has been changed and what
#' has not.
#' @param sideEffect this namespace is meant to be written down as a function
#' to trigger side effect different cmd type
#' @param hard_delete if set to false element will not be deleted but added an
#' attribute `.deleted_at` instead.
#' @return a igraph object that has been changed
#' @export
modify_graph_i = function(
    igraphObj,
    visNetwork_graphChange,
    sideEffect = NULL,
    hard_delete = T
) {
  if(visNetwork_graphChange$cmd == "addNode") {
    # ADD NODE
    id <- (visNetwork_graphChange$id)
    igraphObj <- add_vertex_sf(igraphObj, id)
  } else if (visNetwork_graphChange$cmd == "addEdge") {
    # ADD EDGE
    id = as.character(length(E(igraphObj)) + 1)
    from = (visNetwork_graphChange$from)
    to = (visNetwork_graphChange$to)
    igraphObj <- igraphObj + edge(c(from, to), id = id)
  } else if (visNetwork_graphChange$cmd == "editEdge") {
    # REWIRTE EDGE
    id <- (visNetwork_graphChange$id)
    from = (visNetwork_graphChange$from)
    to = (visNetwork_graphChange$to)
    g <- igraphObj
    # save attributes asided
    attrs <- edge_attr(g, index = id)
    # add and delete edges
    g <- g - edge(id)
    igraphObj <- add_edges(g, c(from, to), attr = attrs)
  } else if (visNetwork_graphChange$cmd == "deleteElements" & hard_delete) {
    # DELETE ELEMENTS
    g <- igraphObj
    edges <- (unlist(visNetwork_graphChange$edges))
    nodes <- (unlist(visNetwork_graphChange$nodes))
    g <- g - edge(edges)
    g <- igraph::delete_vertices(g, nodes)
    igraphObj <- g
  } else if (visNetwork_graphChange$cmd == "deleteElements" & !hard_delete) {
    g <- igraphObj
    edges <- (unlist(visNetwork_graphChange$edges))
    nodes <- (unlist(visNetwork_graphChange$nodes))
    if(
      !".delete_at" %in% igraph::vertex_attr_names(g)
    ) igraph::V(g)$.delete_at = NA
    if(
      !".delete_at" %in% igraph::edge_attr_names(g)
    ) igraph::E(g)$.delete_at = NA

    igraph::V(g)$.delete_at[nodes] = Sys.time()
    igraph::E(g)$.delete_at[edges] = Sys.time()
  }
  return(igraphObj)
}
