#' Search index
#'
#' @description A utils function
#'
#' @return a vector
#' @param g igraph object
#' @param expr_txt string of expression or names of node to search
#' @param as_ids when T return only index
#' @param search_in dip argument that when set to edge can search in edges
#' @export
#' @examples
#' set.seed(1)
#' g = make_tree(12, 3)
#' V(g)$attr <- sample(letters, length(g))
#' V(g)$name <- paste0(seq(length(g)), ".", V(g)$attr)
#' search_idx(g, "attr == 'v'")
#' search_idx(g, '9.w')
#' # if tidygraph is loaded in environment this will work
#' library(tidygraph)
#' search_idx(g, 'node_is_root()')
#'
search_idx = function(g,
                      expr_txt,
                      as_ids = F,
                      search_in = 'nodes'
                        ) {
  g = tidygraph::as_tbl_graph(g)
  actived = as.symbol(search_in)
  G = g |>
    tidygraph::activate({{actived}})
  # {G |> tidygraph::filter(eval(parse(text = expr_txt)))}
  G_res = try({
    rlang::inject(tidygraph::filter(G, name == !!expr_txt))
  }, silent = T)
  if(inherits(G_res, "try-error") || length(G_res) == 0) {
    G_res = try({G |> tidygraph::filter(eval(parse(text = expr_txt)))})
  }
  if(inherits(G_res, "try-error") || length(G_res) == 0) {G_res = G; message("No Search")}
  idx = V(G_res)
  if(as_ids) idx = as_ids(idx)
  return(idx)
}

