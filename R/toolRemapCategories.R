

toolRemapCategories <- function(data, input2fao, output2fao) {

  # temporary data to get test data (will be moved somewhere else later on)
  data <- readSource("Magpie")
  data <- magpie4::clusterOutputToTerraVector(data, attr(data, "clustermap"))

  # prepare FAO reference data
  fao <- readRDS(system.file("extdata/faoAreaHarvested2019.rds", package = "mrdownscale"))
  geometryCountries <- readRDS(system.file("extdata/geometryCountries.rds", package = "mrdownscale"))
  attr(fao, "geometry") <- geometryCountries
  vFao <- as.SpatVector(fao)


  # dataFao <- toolAggregate(data, input2fao, weight = faoArea)
  # dataOut <- toolAggregate(dataFao, output2fao)
  return(dataOut)
}
