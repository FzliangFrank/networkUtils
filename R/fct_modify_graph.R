#' modify_graph
#'
#' @description
#' Modify graph from a single command line
#' @param igraphObj object that inherit igraph
#' @param visNetwork_graphChage a single list of command as a result of
#' visnetwork_graphChange, explained in their package
#' @param sideEffect this namespace is meant to be written down as a function
#' to trigger side effect different cmd type
#' @return The return value, if any, from executing the function.
#'
#' @export
modify_graph_i = function(
    igraphObj,
    visNetwork_graphChange,
    sideEffect = NULL
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
  } else if (visNetwork_graphChange$cmd == "deleteElements") {
    # DELETE ELEMENTS
    g <- igraphObj
    edges <- (unlist(visNetwork_graphChange$edges))
    nodes <- (unlist(visNetwork_graphChange$nodes))
    g <- g - edge(edges)
    g <- igraph::delete_vertices(g, nodes)
    igraphObj <- g
  }
  return(igraphObj)
}
