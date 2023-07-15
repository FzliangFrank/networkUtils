#' Paste attributes into a rectangular tools
#' @title pasteDetails
#' @description paste list of vector (same length) together.
#' You can use these function to write html paragraph for simple just one row
#' Use-full with graph node and edge attributes.
#' @name pasteDetails
#' @return The return value, if any, from executing the function.
#' @param namedList a named r list
#' @param include a vector of names from much you want to include
#' @param sep.attr this is typically line breaks in between each attribute
#' @param sep.label the speperator between attribute name and attributes
#' @param show_hidden attributes begain with '.' will be pasted into this.
#' @param g igraph object specifically used for graph objects
#' @examples
#' # example code
#'
#'# paste normal object
#' nL = list(
#'  id = 1,
#'   object = "Apple",
#'   category = "Fruit",
#'   size = 24.22
#' )
#' x_html = pasteDetails(nL)
# 'x_paste = pasteDetails(nL, sep.attr = "\n")
#' cat(x_paste)

# paste a graph attributes
#' @export
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

#'
#' @name pasteDetails
#' @export
pasteNodeDetails <- function(g, include, ...) {
  vertex_attr(g) |>
    pasteDetails(include = include, ...)
}

#' @details
#' exclusivedly works for pasting
#'
#' @name pasteDetails
#' @export
pasteEdgeDetails <- function(g, include, ...) {
  edge_attr(g) |>
    pasteDetails(include = include, ...)
}
