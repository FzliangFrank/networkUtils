#' plotPPPdensity
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
plotPPPdensity = function(xy) {
  maxRange =xy |> sf::st_coordinates() |> apply(2, max)
  minRange =xy |> sf::st_coordinates() |> apply(2, min)
  w1 <- sf::st_bbox(c(xmin = minRange[['X']],
                      ymin = minRange[['Y']],
                      xmax = maxRange[['X']],
                      ymax = maxRange[['Y']])) |>
    sf::st_as_sfc()
  # xy |> st_combine() |> st_centroid()
  # w1 |> st_centroid()
  #   st_buffer(0.2)
  (pp1 <- c(w1, sf::st_geometry(xy)) |> spatstat.geom::as.ppp())
  den1 <- spatstat.explore::density.ppp(pp1, sigma = spatstat.explore::bw.diggle)
  par(mar =c(0.2, 0.2, 0.2, 0.2))
  plot(den1,
       col = colorRampPalette(RColorBrewer::brewer.pal(9,"BuGn"))(500),
       main=NULL,
       legend=F, show.all=F
  )
  plot(pp1, add = T)
}
