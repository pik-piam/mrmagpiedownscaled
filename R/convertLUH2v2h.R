#' convertLUH2v2h
#'
#' Convert LUH2 cell area shares to absolute areas by multiplying with cell area.
#'
#' @param x SpatRaster with LUH2 cell area shares
#' @param subtype only "states" is supported, for other subtypes pass convert = FALSE
convertLUH2v2h <- function(x, subtype = "states") {
  if (subtype != "states") {
    stop("Pass convert = FALSE when subtype != states.")
  }

  stopifnot(max(terra::values(x), na.rm = TRUE) <= 1.0001)

  cellArea <- terra::rast("staticData_quarterdeg.nc", "carea")
  stopifnot(terra::units(cellArea) == "km2",
            all(terra::units(x) == "1"))
  x <- x * cellArea / 10000 # multiply shares by area, / 10000 for km2 -> Mha
  unit <- "Mha"
  terra::units(x) <- unit

  stopifnot(min(terra::values(x), na.rm = TRUE) >= 0)

  return(list(x = x,
              class = "SpatRaster",
              unit = unit))
}
