

toolRemapCategories <- function(x, input2fao, output2fao) {

  # prepare FAO reference data
  fao <- readRDS(system.file("extdata/faoAreaHarvested2019.rds", package = "mrdownscale"))
  geometryCountries <- readRDS(system.file("extdata/geometryCountries.rds", package = "mrdownscale"))
  attr(fao, "geometry") <- geometryCountries
  vFao <- as.SpatVector(fao)


  # dataFao <- toolAggregate(x, input2fao, weight = faoArea)
  # dataOut <- toolAggregate(dataFao, output2fao)
  return(dataOut)
}
