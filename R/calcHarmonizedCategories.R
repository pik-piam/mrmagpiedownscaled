calcHarmonizedCategories <- function() {
  x <- madrat::readSource("Magpie")
  clustermap <- attr(x, "clustermap")
  attr(x, "clustermap") <- NULL

  # map cluster to country
  # TODO this is wrong because a cluster contains cells of multiple countries
  clusterToCountry <- mappingVector(clustermap, "cluster", "country")
  countries <- as.character(clusterToCountry[magclass::getItems(x, 1, full = TRUE)])
  x <- magclass::add_dimension(x, 1.3, "country")
  magclass::getItems(x, dim = 1.3, full = TRUE) <- countries

  # map magpie crops to LUH2 croptypes
  # magclass::write.magpie(calcOutput(type = "LUH2MAgPIE", aggregate = FALSE, share = "LUHofMAG",
  #                                   bioenergy = "fix", missing = "fill", rice = "total"), 'LUHofMAG.cs5')
  # TODO this is just a rough approximation, also using shares from 2010 is arbitrary
  mapping <- magclass::read.magpie(system.file("extdata", "LUHofMAG.cs5", package = "mrdownscale"))[, 2010, ]
  magclass::getYears(mapping) <- NULL

  crops <- x[, , magclass::getItems(mapping, dim = 3.2)]
  x <- x[, , c("crop", magclass::getItems(mapping, dim = 3.2)), invert = TRUE]

  mapping <- mapping[magclass::getItems(crops, dim = 1.3), , ]
  crops <- crops * mapping
  crops <- magclass::dimSums(crops, dim = 3.1)

  x <- magclass::mbind(x, crops)

  # map primforest -> primf
  stopifnot("primforest" %in% dimnames(x)[[3]])
  dimnames(x)[[3]][dimnames(x)[[3]] == "primforest"] <- "primf"

  # map secdforest + forestry -> secdf
  x[, , "secdforest"] <- x[, , "secdforest"] + x[, , "forestry"]
  dimnames(x)[[3]][dimnames(x)[[3]] == "secdforest"] <- "secdf"
  x <- x[, , "forestry", invert = TRUE]

  # TODO map other -> primn & secdn
  # x <- magpie4::PrimSecdOtherLand(x, "avl_land_full_t_0.5.mz", level = "cell", unit = "share") # TODO level = "grid"?

  # TODO map past -> pastr & range


  # TODO consistency checks
  # luhCategories <- terra::varnames(madrat::readSource("LUH2v2h"))

  return(x)
}

mappingVector <- function(mapping, namesColumn, valueColumn) {
  mapping <- unique(mapping[, c(namesColumn, valueColumn)])
  x <- mapping[[valueColumn]]
  names(x) <- mapping[[namesColumn]]
  return(x)
}
