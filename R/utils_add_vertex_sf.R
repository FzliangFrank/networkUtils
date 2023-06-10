#' add_vertex_sf
#'
#' @description When there is sf object in attributes
#' sf comparable adding vertex function.
#'
#' @param g igraph object
#' @param id a chr or that is passing to igraph::vertex
#' @param ... other arg pass to igraph::vertex
#' @param geom a geometry that to add if any
#' @return new added igraph
#'
#' @noRd


add_vertex_sf = function(g, id, ..., geom=NULL) {
  stopifnot(is.null(geom) || sf::st_is_valid(geom))
  schema = purrr::imap(vertex_attr(g), ~ class(.x))
  sf_attr_i = purrr::detect_index(schema, ~'sfc' %in% .x)
  sf_attr_name = names(schema)[sf_attr_i]

  if(sf_attr_i != 0) {
    sf_attr = vertex_attr(g, sf_attr_name)
    if(!is.null(geom)) sf_attr = c(sf_attr, geom)
    g = delete_vertex_attr(g, sf_attr_name)
    g = g + vertex(id, ...)
    set_vertex_attr(g, name = sf_attr_name, value = sf_attr[seq(length(V(g)))])
  } else {
    g = g + vertex(id, ...)
  }
}
