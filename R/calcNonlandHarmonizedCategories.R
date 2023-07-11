calcNonlandHarmonizedCategories <- function(input = "magpie", target = "luh2") {
  x <- toolAddCheckReport(calcOutput("NonlandInputData", input = input, aggregate = FALSE))

  # separate categories that need to be mapped from those that don't
  wood <- x[, , c("rndwd", "fulwd")]
  x <- x[, , c("rndwd", "fulwd"), invert = TRUE]

  # map categories using weights from land categorization
  getItems(x, 3) <- sub("_fertilizer", "", getItems(x, 3))
  map <- toolLandCategoriesMapping(input, target)

  # get weights for disaggregation to reference categories
  ref <- toolAddCheckReport(calcOutput("LandCategorizationWeight", map = map, geometry = attr(x, "geometry"),
                                       crs = attr(x, "crs"), aggregate = FALSE))
  map <- map[map$dataInput %in% getItems(x, 3), ]
  ref <- ref[, , unique(map$merge)]
  y   <- toolAggregate(x, map, dim = 3, from = "dataInput", to = "merge", weight = ref)
  out <- toolAggregate(y, map, dim = 3, from = "merge",     to = "dataOutput")
  out[, , "c3per"] <- out[, , "c3per"] + out[, , "c3per_biofuel_2nd_gen"]
  out[, , "c4per"] <- out[, , "c4per"] + out[, , "c4per_biofuel_2nd_gen"]
  out <- out[, , c("c3per_biofuel_2nd_gen", "c4per_biofuel_2nd_gen"), invert = TRUE]
  getItems(out, 3) <- paste0(getItems(out, 3), "_fertilizer")

  out <- mbind(out, wood)
  attr(out, "crs") <- attr(x, "crs")
  attr(out, "geometry") <- attr(x, "geometry")

  # check data for consistency
  toolCheck("Nonland Harmonized Categories output", {
    toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
    expectedCategories <- c(paste0(c("c3ann", "c4ann", "c3per", "c4per", "c3nfx"), "_fertilizer"), "rndwd", "fulwd")
    toolExpectTrue(setequal(getItems(out, dim = 3), expectedCategories), "Nonland categories match target definition")
    toolExpectTrue(all(out >= 0), "All values are >= 0")
  })

  attr(out, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = out,
              isocountries = FALSE,
              unit = "rndwd, fulwd: 1; *_fertilizer: kg yr-1",
              min = 0,
              description = "Input data with nonland categories remapped to categories of target data set"))
}
