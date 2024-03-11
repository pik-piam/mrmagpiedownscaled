#' toolDownscaleMagpieClassic
#'
#' classic MAgPIE downscaling method using \code{luscale::interpolate2} as
#' downscaling function.
#'
#' @param x magpie dataset containing land to be downscaled
#' @param xTarget target land use dataset as SpatRaster for initialization year
#' @return downscaled land use dataset
#' @author Jan Philipp Dietrich
#' @importFrom mstools toolStatusMessage
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

  map <- .getDownscaleMap(x, xTarget)

  mTarget <- as.magpie(xTarget)

  rm(xTarget)
  gc()

  intersect <- intersect(getItems(mTarget, dim = 1), map$cell)
  missingInTarget <- (length(map$cell) - length(intersect)) / length(map$cell)
  if (missingInTarget > 0) {
    map <- map[map$cell %in% intersect, ]
    message <- paste0(round(missingInTarget * 100, 2),
                      "% of cells from downscale mapping do not exist in target data and thus will be ignored!")
    toolStatusMessage("warn", message)
  }  else {
    toolStatusMessage("ok", "input data area is fully covered by target data")
  }

  missingInX <- (dim(mTarget)[1] - length(intersect)) / dim(mTarget)[1]
  if (missingInX > 0) {
    mTarget <- mTarget[intersect, , ]
    message <- paste0(round(missingInX * 100, 2),
                      "% of cells missing in input data and thus removed from target data!")
    toolStatusMessage("warn", message)
  } else {
    toolStatusMessage("ok", "target data area is fully covered by input data")
  }

  "!# @monitor luscale::interpolate2"

  # interpolate2 assumes constant total over time, but only warns if unfulfilled, convert that to error
  withr::with_options(c(warn = 2), {
    out <- luscale::interpolate2(x[, -1, ], mTarget[, 1, ], map)
  })
  getSets(out)[1:2] <- c("x", "y")

  return(out)
}
