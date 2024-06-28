#' toolDownscaleMagpieClassic
#'
#' classic MAgPIE downscaling method using \code{luscale::interpolate2} as
#' downscaling function.
#'
#' @param x magpie dataset containing land to be downscaled
#' @param xTarget target land use dataset as SpatRaster for initialization year
#' @return downscaled land use dataset
#' @author Jan Philipp Dietrich, Pascal Sauer
toolDownscaleMagpieClassic <- function(x, xTarget) {
  mapping <- calcOutput("ResolutionMapping", aggregate = FALSE)
  mapping$cluster <- mapping$lowRes
  mapping <- mapping[, c("cell", "cluster")]

  mTarget <- as.magpie(xTarget[[terra::time(xTarget) == terra::time(xTarget)[1]]])

  "!# @monitor luscale::interpolate2"

  # interpolate2 assumes constant total over time, but only warns if unfulfilled, convert that to error
  tryCatch({
    out <- luscale::interpolate2(x[, -1, ], mTarget, mapping)
  }, warning = stop)
  getSets(out)[1:2] <- c("x", "y")

  return(out)
}
