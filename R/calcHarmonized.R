calcHarmonized <- function() {
  # magpie <- calcOutput("HarmonizedCategories", aggregate = FALSE)
  magpieMag <- read.magpie("calcHarmonizedCategories.mz")
  magpie <- as.data.frame(magpieMag, rev = 3)
  magpie$j <- NULL
  stopifnot(identical(names(magpie), c("clusterId", "year", "data", ".value")))
  names(magpie) <- c("region", "period", "variable", "value")
  magpie$region <- as.character(magpie$region)
  magpie$scenario <- "scenario"
  magpie$model <- "MAgPIE"
  magpie$dummy <- "dummy" # mip::harmonize expects exactly 7 columns

  luhVector <- calcOutput("LowResLUH2v2h", aggregate = FALSE)
  luhMag <- as.magpie(luhVector, spatial = which(terra::datatype(luhVector) != "double"))
  luh <- as.data.frame(luhMag, rev = 3)
  stopifnot(identical(names(luh), c("clusterId", "year", "data", ".value")))
  names(luh) <- c("region", "period", "variable", "value")
  luh$region <- as.character(luh$region)
  luh$scenario <- "scenario"
  luh$model <- "LUH"
  luh$dummy <- "dummy" # mip::harmonize expects exactly 7 columns

  harmonized <- mip::harmonize(magpie, luh, harmonizeYear = "1995",
                               finalYear = "2040", method = "ratio")

  harmonized <- harmonized[, c("region", "period", "variable", "value")]
  names(harmonized) <- c("clusterId", "year", "category", "value")
  harmonizedMag <- as.magpie(harmonized, spatial = "clusterId", temporal = "year")
  attr(harmonizedMag, "geometry") <- attr(luhMag, "geometry")
  out <- magclass::as.SpatVector(harmonizedMag)
  terra::crs(out) <- terra::crs(luhVector)
  return(out)
}
