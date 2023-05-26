convertLUH2v2h <- function(x, subtype = "LUH") {
  if (subtype == "cellArea") {
    stop("Pass convert = FALSE when getting cellArea.")
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
