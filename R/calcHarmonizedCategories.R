calcHarmonizedCategories <- function() {
  x <- madrat::readSource("Magpie")
  clustermap <- attr(x, "clustermap")
  attr(x, "clustermap") <- NULL

  x <- x[, , c("crop", "other"), invert = TRUE]

  # map cluster to country
  # TODO this is wrong because a cluster contains cells of multiple countries
  clusterToCountry <- mappingVector(clustermap, "cluster", "country")
  countries <- as.character(clusterToCountry[magclass::getItems(x, 1, full = TRUE)])
  x <- magclass::add_dimension(x, 1.3, "country")
  magclass::getItems(x, dim = 1.3, full = TRUE) <- countries

  # map magpie crops to LUH2 croptypes
  magpie2luh <- magpie2luh # magpie2luh is defined via R/sysdata.rda

  crops <- x[, , magclass::getItems(magpie2luh, dim = 3.2)]
  x <- x[, , magclass::getItems(magpie2luh, dim = 3.2), invert = TRUE]

  magpie2luh <- magpie2luh[magclass::getItems(crops, dim = 1.3), , ]
  crops <- crops * magpie2luh
  crops <- magclass::dimSums(crops, dim = 3.1)

  x <- magclass::mbind(x, crops)

  # map primforest -> primf
  stopifnot("primforest" %in% dimnames(x)[[3]])
  dimnames(x)[[3]][dimnames(x)[[3]] == "primforest"] <- "primf"

  # map secdforest + forestry -> secdf
  x[, , "secdforest"] <- x[, , "secdforest"] + x[, , "forestry"]
  dimnames(x)[[3]][dimnames(x)[[3]] == "secdforest"] <- "secdf"
  x <- x[, , "forestry", invert = TRUE]

  # map primother -> primn
  stopifnot("primother" %in% dimnames(x)[[3]])
  dimnames(x)[[3]][dimnames(x)[[3]] == "primother"] <- "primn"

  # map secdother -> secdn
  stopifnot("secdother" %in% dimnames(x)[[3]])
  dimnames(x)[[3]][dimnames(x)[[3]] == "secdother"] <- "secdn"

  # TODO map past -> pastr & range


  # TODO consistency checks
  # luhCategories <- terra::varnames(madrat::readSource("LUH2v2h"))

  # TODO weights, unit, description
  return(list(x = x,
              weight = x,
              isocountries = FALSE))
}

mappingVector <- function(mapping, namesColumn, valueColumn) {
  mapping <- unique(mapping[, c(namesColumn, valueColumn)])
  x <- mapping[[valueColumn]]
  names(x) <- mapping[[namesColumn]]
  return(x)
}
