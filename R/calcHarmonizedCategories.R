calcHarmonizedCategories <- function() {
  x <- madrat::readSource("Magpie")
  clustermap <- attr(x, "clustermap")
  attr(x, "clustermap") <- NULL
  historic <- attr(x, "historic")
  attr(x, "historic") <- NULL

  # add country based on cluster
  # TODO this is wrong because a cluster contains cells of multiple countries
  clusterToCountry <- toolMappingVector(clustermap, "cluster", "country")
  countries <- as.character(clusterToCountry[magclass::getItems(x, 1, full = TRUE)])
  x <- magclass::add_dimension(x, 1.3, "country")
  magclass::getItems(x, dim = 1.3, full = TRUE) <- countries

  # map magpie crops to LUH2 croptypes
  # TODO move magpieCropsToLuh data to inst/extdata as .mz or .rds
  magpieCropsToLuh <- magpieCropsToLuh # magpieCropsToLuh is defined via R/sysdata.rda

  crops <- x[, , magclass::getItems(magpieCropsToLuh, dim = 3.2)]
  x <- x[, , c("crop", magclass::getItems(magpieCropsToLuh, dim = 3.2)), invert = TRUE]

  magpieCropsToLuh <- magpieCropsToLuh[magclass::getItems(crops, dim = 1.3), , ]
  crops <- crops * magpieCropsToLuh
  crops <- magclass::dimSums(crops, dim = 3.1)

  x <- magclass::mbind(x, crops)

  # sum up secdforest + forestry -> secdf
  x[, , "secdforest"] <- x[, , "secdforest"] + x[, , "forestry"]
  x <- x[, , "forestry", invert = TRUE]

  # split past -> past & range
  x <- toolSplitPasture(x, historic)

  # rename magpie to LUH2 categories
  luhCategoryMapping <- c(primforest = "primf",
                          secdforest = "secdf",
                          primother = "primn",
                          secdother = "secdn",
                          past = "pastr")
  categoryMapper <- function(category) {
    if (category %in% names(luhCategoryMapping)) luhCategoryMapping[[category]] else category
  }
  dimnames(x)[[3]] <- vapply(dimnames(x)[[3]], categoryMapper, character(1), USE.NAMES = FALSE)

  names(dimnames(x)) <- c("region.cluster.country", "year", "category")

  # check differences between categories in x and LUH2
  luhCategories <- unique(sub("_[0-9]+$", "", names(madrat::readSource("LUH2v2h"))))
  stopifnot(setequal(luhCategories, c(dimnames(x)$category, "residual")))

  return(list(x = x,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "magpie output mapped to LUH2 categories"))
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
