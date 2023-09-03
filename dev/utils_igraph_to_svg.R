#' igraph_to_svg
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
igraph_to_svg <- function(igraphObj, exportPath) {
  if(grepl("\\.svg", exportPath)) stop("please write svg as file extension")
  dummyFile <- paste0(tempfile(pattern = "something"), ".dot")
  igraph::write_graph(igraphObj, file=dummyFile, format = "dot")
  dotString <- readr::read_file(dummyFile)
  svg <- DiagrammeRsvg::export_svg(DiagrammeR::grViz(dotString))
  readr::write_file(svg, exportPath)
}


