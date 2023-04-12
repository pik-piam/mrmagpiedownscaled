calcHarmonizedCategories <- function() {
  x <- madrat::readSource("Magpie")
  clustermap <- attr(x, "clustermap")
  attr(x, "clustermap") <- NULL
  historic <- attr(x, "historic")
  attr(x, "historic") <- NULL

  # map cluster to country
  # TODO this is wrong because a cluster contains cells of multiple countries
  clusterToCountry <- toolMappingVector(clustermap, "cluster", "country")
  countries <- as.character(clusterToCountry[magclass::getItems(x, 1, full = TRUE)])
  x <- magclass::add_dimension(x, 1.3, "country")
  magclass::getItems(x, dim = 1.3, full = TRUE) <- countries

  # map magpie crops to LUH2 croptypes
  magpie2luh <- magpie2luh # magpie2luh is defined via R/sysdata.rda

  crops <- x[, , magclass::getItems(magpie2luh, dim = 3.2)]
  x <- x[, , c("crop", magclass::getItems(magpie2luh, dim = 3.2)), invert = TRUE]

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

  # map past -> pastr & range
  x <- toolSplitPasture(x, historic)

  # TODO consistency checks
  # luhCategories <- terra::varnames(madrat::readSource("LUH2v2h"))

  # TODO weights, unit, description
  return(list(x = x,
              weight = x,
              isocountries = FALSE))
}

toolMappingVector <- function(mapping, namesColumn, valueColumn) {
  mapping <- unique(mapping[, c(namesColumn, valueColumn)])
  x <- mapping[[valueColumn]]
  names(x) <- mapping[[namesColumn]]
  return(x)
}

toolSplitPasture <- function(x, historic) {
  areas <- x[, , "past"]
  areas <- magclass::add_columns(areas, "range")

  historicShares <- historic[, , c("past", "range")] / magclass::dimSums(historic[, , c("past", "range")], dim = 3)
  historicShares[!is.finite(historicShares)] <- 0.5

  historicYears <- magclass::getYears(historicShares)
  areas[, historicYears, ] <- historicShares * x[, historicYears, "past"]
  rangeArea2015 <- magclass::setYears(areas[, 2015, "range"], NULL)

  after2015 <- magclass::getYears(areas, as.integer = TRUE)[magclass::getYears(areas, as.integer = TRUE) > 2015]
  areas[, after2015, "range"] <- rangeArea2015[, , "range"]
  areas[, after2015, "range"] <- magclass::magpply(X = magclass::mbind(areas[, after2015, "range"],
                                                                       x[, after2015, "past"]),
                                                   FUN = min, DIM = 3)
  areas[, after2015, "past"] <- x[, after2015, "past"] - areas[, after2015, "range"]

  stopifnot(isTRUE(all.equal(magclass::collapseDim(x[, , "past"]),
                             magclass::dimSums(areas))))

  x <- x[, , "past", invert = TRUE]
  x <- magclass::mbind(x, areas)
  return(x)
}
