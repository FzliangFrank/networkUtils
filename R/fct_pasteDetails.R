#' sumAttributes
#' Make a vector
#' @description paste list of vector (same length) together.
#' Usefull with graph node and edge attributes.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
pasteDetails <- function(namedList,
                         include,
                         sep.attr = "<br>",
                         sep.label = ":",
                         show_hidden = F
                         ) {
  if(!missing(include)) namedList <- namedList[include]
  if(length(namedList) == 0) {
    "empty"
  } else {
    if(!show_hidden) {
      hidden_lgl = grepl("^\\.",names(namedList))
      namedList = namedList[!hidden_lgl]
    }
    namedList |>
      purrr::imap(~paste(.y, sep.label, .x)) |>
      purrr::reduce(paste, sep = sep.attr)
  }
}

pasteNodeDetails <- function(g, include, ...) {
  vertex_attr(g) |>
    pasteDetails(include = include, ...)
}

pasteEdgeDetails <- function(g, include, ...) {
  edge_attr(g) |>
    pasteDetails(include = include, ...)
}
