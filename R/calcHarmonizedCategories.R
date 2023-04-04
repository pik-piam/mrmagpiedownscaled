calcHarmonizedCategories <- function() {
  x <- madrat::readSource("Magpie")
  clustermap <- attr(x, "clustermap")
  attr(x, "clustermap") <- NULL

  # map primforest -> primf
  stopifnot("primforest" %in% dimnames(x)[[3]])
  dimnames(x)[[3]][dimnames(x)[[3]] == "primforest"] <- "primf"

  # map secdforest + forestry -> secdf
  x[, , "secdforest"] <- x[, , "secdforest"] + x[, , "forestry"]
  dimnames(x)[[3]][dimnames(x)[[3]] == "secdforest"] <- "secdf"
  x <- x[, , "forestry", invert = TRUE]

  # TODO map other -> primn & secdn
  # x <- magpie4::PrimSecdOtherLand(x, "avl_land_full_t_0.5.mz", level = "cell", unit = "share") # TODO level = "grid"?

  # map magpie crops to LUH2 croptypes
  # magclass::write.magpie(calcOutput(type = "LUH2MAgPIE", aggregate = FALSE, share = "LUHofMAG", bioenergy = "fix", missing = "fill", rice = "total"), 'LUHofMAG.cs5')
  mapping <- magclass::read.magpie(system.file("extdata", "LUHofMAG.cs5", package = "mrdownscale"))

  clusterToCountry <- mappingVector(clustermap, "cluster", "country")
  countries <- as.character(clusterToCountry[magclass::getItems(x, 1, full = TRUE)])
  xx <- magclass::add_dimension(x, 1.3, "country")
  magclass::getItems(xx, dim = 1.3, full = TRUE) <- countries

  xx <- magclass::add_dimension(xx, 3.2, "luh")
  xx <- magclass::add_columns(xx, unique(mapping$LUH2), dim = 3.2)



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
