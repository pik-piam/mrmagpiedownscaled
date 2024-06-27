#' toolDownscaleMagpieClassic
#'
#' classic MAgPIE downscaling method using \code{luscale::interpolate2} as
#' downscaling function.
#'
#' @param x magpie dataset containing land to be downscaled
#' @param xTarget target land use dataset as SpatRaster for initialization year
#' @return downscaled land use dataset
#' @author Jan Philipp Dietrich
toolDownscaleMagpieClassic <- function(x, xTarget) {

  .getDownscaleMap <- function(x, xTarget) {
    tmp <- as.SpatVector(x[, 1, 1])[, ".id"]
    tmp[[1]]$.id <- as.integer(tmp[[1]]$.id)
    tmp <- terra::rasterize(tmp, xTarget, ".id", touches = FALSE)
    names(tmp) <- "id"
    tmp <- as.magpie(tmp)
    map <- data.frame(cell = getItems(tmp, dim = 1), cluster = as.vector(tmp))

    clusternames <- getItems(x, dim = 1)
    names(clusternames) <- getItems(x, dim = 1.1)
    map$cluster <- clusternames[map$cluster]

    return(map)
  }

  mapping_old <- .getDownscaleMap(x, xTarget) # TODO remove old mapping and .getDownscaleMap
  mapping <- calcOutput("ResolutionMapping", aggregate = FALSE)
  mapping$cell <- paste0(sub("\\.", "p", mapping$x), ".", sub("\\.", "p", mapping$y))
  mapping$cluster <- mapping$lowRes
  mapping <- mapping[, c("cell", "cluster")]

  mTarget <- as.magpie(xTarget)

  rm(xTarget)
  gc()

  "!# @monitor luscale::interpolate2"

  # interpolate2 assumes constant total over time, but only warns if unfulfilled, convert that to error
  withr::with_options(c(warn = 2), {
    out <- luscale::interpolate2(x[, -1, ], mTarget[, 1, ], mapping)
  })
  getSets(out)[1:2] <- c("x", "y")

  return(out)
}
