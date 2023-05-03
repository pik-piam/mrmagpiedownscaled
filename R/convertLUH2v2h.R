convertLUH2v2h <- function(x) {
  # add residual category (e.g. ocean)
  years <- unique(terra::time(x))
  residual <- lapply(years, function(year) max(1 - sum(x[as.character(year)]), 0))
  residual <- terra::tighten(do.call(c, residual))
  names(residual) <- paste0("residual_", years)
  terra::time(residual, "years") <- years
  terra::units(residual) <- "1"
  terra::varnames(residual) <- "residual"
  x <- c(x, residual)

  stopifnot(max(terra::values(x), na.rm = TRUE) <= 1.0001)

  cellArea <- terra::rast("staticData_quarterdeg.nc", "carea")
  stopifnot(terra::units(cellArea) == "km2",
            all(terra::units(x) == "1"))
  x <- x * cellArea / 10000 # multiply shares by area, / 10000 for km2 -> Mha
  # TODO check sum of all categories from e.g. 1995 sum up to cellArea again
  unit <- "Mha"
  terra::units(x) <- unit

  stopifnot(min(terra::values(x), na.rm = TRUE) >= 0)

  return(list(x = x,
              class = "SpatRaster",
              unit = unit))
}
