#' callmebyName
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
evalParse = function(oneString) {
  stopifnot(is.character(oneString))
  eval(parse(text = oneString))
}
