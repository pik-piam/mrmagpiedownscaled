convertLUH2v2h <- function(x) {
  # add residual category
  years <- unique(terra::time(x))
  residual <- lapply(years, function(year) 1 - terra::app(x[as.character(year)], sum))
  residual <- terra::tighten(do.call(c, residual))
  names(residual) <- paste0("residual_", years)
  terra::time(residual, "years") <- years
  terra::units(residual) <- "1"
  terra::varnames(residual) <- "residual"
  x <- c(x, residual)

  cellArea <- terra::rast("staticData_quarterdeg.nc", "carea")
  stopifnot(terra::units(cellArea) == "km2",
            all(terra::units(x) == "1"))
  x <- x * cellArea / 10000 # multiply shares by area, / 10000 for km2 -> Mha
  # TODO check sum of all categories from e.g. 1995 sum up to cellArea again
  unit <- "Mha"
  terra::units(x) <- unit
  return(list(x = x,
              class = "SpatRaster",
              unit = unit))
}
