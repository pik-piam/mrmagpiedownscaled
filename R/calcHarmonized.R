calcHarmonized <- function() {
  magpieMag <- calcOutput("HarmonizedCategories", aggregate = FALSE)
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
  luh <- luh[luh$variable != "residual", ]
  luh$region <- as.character(luh$region)
  luh$scenario <- "scenario"
  luh$model <- "LUH"
  luh$dummy <- "dummy" # mip::harmonize expects exactly 7 columns

  tolerance <- 0.05 # for deciding if clusters have equal area
  magpieClusterAreas <- toolClusterAreas(magpie, tolerance)
  luhClusterAreas <- toolClusterAreas(luh, tolerance)

  stopifnot(isTRUE(all.equal(magpieClusterAreas, luhClusterAreas, tolerance = tolerance)))

  harmonized <- mip::harmonize(magpie, luh, harmonizeYear = "1995",
                               finalYear = "2040", method = "offset")

  harmonized <- harmonized[, c("region", "period", "variable", "value")]

  harmonizedClusterAreas <- toolClusterAreas(harmonized, tolerance)
  stopifnot(isTRUE(all.equal(magpieClusterAreas, harmonizedClusterAreas, tolerance = tolerance)))

  names(harmonized) <- c("clusterId", "year", "category", "value")
  harmonizedMag <- as.magpie(harmonized, spatial = "clusterId", temporal = "year")
  attr(harmonizedMag, "geometry") <- attr(luhMag, "geometry")
  out <- magclass::as.SpatVector(harmonizedMag)
  terra::crs(out) <- terra::crs(luhVector)
  return(out)
}

# get the area of each cluster by summing up all land types
toolClusterAreas <- function(x, tolerance) {
  # sum up value col aggregated by region + period cols
  areas <- aggregate(value ~ region + period, data = x, FUN = sum)

  allEqual <- function(a) as.character(all.equal(rep(a[[1]], length(a)), a, tolerance = tolerance))
  consistent <- aggregate(value ~ region, data = areas, FUN = allEqual)
  if (!all(consistent$value == "TRUE")) {
    print(consistent[consistent$value != "TRUE", ])
    warning("Found inconsistent region sizes, see data frame above.")
  }

  areas <- areas[areas$period == areas$period[[1]], ]
  areas <- areas[order(areas$region), ]
  out <- areas$value
  names(out) <- areas$region
  return(out)
}
