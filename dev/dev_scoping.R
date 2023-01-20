library(rlang)
readLines("R/mod_visNetworkRead.R")
parse("R/mod_visNetworkRead.R") |>
  as.character() |>
  rlang::parse_exprs() |>
  purrr::map(rlang::call_name)

# rlang::parse_expr(file("R/mod_visNetworkRead.R"))
helper_defunc <- function(x) {
  x <- gsub("^function(.+) \\{", "", x)
  gsub("\\}$", "", x)
}
parse("R/fct_pasteDetails.R") |>
  as.character() |>
  rlang::parse_exprs() |>
  purrr::map(~c(function_name  = .x[2], function_body = .x[3])) |>
  list(x = _) |>
  with({
    graph <- igraph::make_empty_graph()
    # initial filter which function depends on the other function (to reduce computaton)
    v1 <- x |> purrr::map_chr(~.x$function_name |> call_name())
    v2 <- x |> purrr::map_chr(~.x$function_body |> as.character() |> helper_defunc())
    v2 <- set_names(v2, v1)
    derivatives = purrr::map_lgl(v2, stringr::str_detect, paste(v1, collapse = "|"))

    graph |> add_vertices(length(v1), name = v1)
    # second function which function ddepends on which function
    for(i in v2[derivatives]) {
      # logic to add edges
      root <- which(sapply(v1, grepl, i)) |> names()

      e1 <- match(root, v1)
      e2 <- match(i, v2)
      print(paste(e1, e2))
      add_edges(graph, c(e1, e2))
    }
})
graph
# from this we can create a adjacency matrix.


