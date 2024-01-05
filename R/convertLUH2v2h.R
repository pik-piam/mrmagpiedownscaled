#' convertLUH2v2h
#'
#' Convert LUH2 cell area shares to absolute areas by multiplying with cell area, convert to Mha.
#'
#' @param x SpatRaster with LUH2 cell area shares
#' @param subtype Only "states" and "woodHarvestArea" are converted
convertLUH2v2h <- function(x, subtype = "states") {
  cellArea <- terra::rast("staticData_quarterdeg.nc", "carea")
  stopifnot(terra::units(cellArea) == "km2")
  # convert from km2 to Mha
  cellAreaMha <- cellArea / 10000

  if (subtype == "states") {
    stopifnot(max(terra::values(x), na.rm = TRUE) <= 1.0001,
              all(terra::units(x) == "1"))
    x <- x * cellAreaMha
    unit <- "Mha"
    terra::units(x) <- unit
  } else if (subtype == "transitions") {
    harv <- x["harv"]
    harv <- harv * cellAreaMha
    names(harv) <- sub("harv", "wood_harvest_area", names(harv))
    terra::units(harv) <- "Mha yr-1"

    bioh <- x["bioh"]
    bioh <- bioh * 1 # force bioh into memory, otherwise the result cannot be cached
    terra::units(bioh) <- "kg C yr-1"

    x <- c(harv, bioh)
    unit <- "*_bioh: kg C yr-1, *_harv: Mha yr-1"
  } else {
    stop("subtype must be states or transitions, for management and cellArea pass convert = FALSE")
  }

  return(list(x = x,
              class = "SpatRaster",
              unit = unit))
}
