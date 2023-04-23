#' blurry_range
#'
#' @description A utils function
#' Instead of Range create it to nears approximation
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

blurry_range = function(x) {
  res <- 0.01
  rg = range(x, na.rm=T)
  rg[1] <- signif(rg[1] - res*10, 2)
  rg[2] <- signif(rg[2] + res*10, 2)
  return(rg)
}
