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

  magpieClusterAreas <- toolClusterAreas(magpie)
  luhClusterAreas <- toolClusterAreas(luh)
  stopifnot(isTRUE(all.equal(magpieClusterAreas, luhClusterAreas)))

  harmonized <- mip::harmonize(magpie, luh, harmonizeYear = "1995",
                               finalYear = "2040", method = "offset")

  harmonized <- harmonized[, c("region", "period", "variable", "value")]

  harmonizedClusterAreas <- toolClusterAreas(harmonized)
  stopifnot(isTRUE(all.equal(magpieClusterAreas, harmonizedClusterAreas)))

  names(harmonized) <- c("clusterId", "year", "category", "value")
  harmonizedMag <- as.magpie(harmonized, spatial = "clusterId", temporal = "year")
  attr(harmonizedMag, "geometry") <- attr(luhMag, "geometry")
  out <- magclass::as.SpatVector(harmonizedMag)
  terra::crs(out) <- terra::crs(luhVector)
  return(out)
}

# get the area of each cluster by summing up all land types
toolClusterAreas <- function(x) {
  .area <- function(x, clusterId, year) {
    sum(x[x$region == clusterId & x$period == year, "value"], na.rm = TRUE)
  }
  areas <- lapply(unique(x$region), function(clusterId) {
    area <- .area(x, clusterId, year = unique(x$period)[1])
    for (year in unique(x$period)[-1]) {
      areaOtherYear <- .area(x, clusterId, year)
      if (abs(areaOtherYear - area) > 0.05) {
        warning("clusterId: ", clusterId, ", year: ", year, ", diff: ",
                abs(areaOtherYear - area), " - ", areaOtherYear, " != ", area)
      }
    }
    return(area)
  })
  names(areas) <- unique(x$region)
  return(unlist(areas[as.character(1:200)]))
}
