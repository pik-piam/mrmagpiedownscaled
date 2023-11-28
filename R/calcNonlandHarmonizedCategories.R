#' calcNonlandHarmonizedCategories
#'
#' Harmonize categories by mapping nonland input data categories to the categories of the nonland target dataset.
#' See \code{\link{calcLandHarmonizedCategories}} for an explanation of the mapping procedure.
#'
#' @param input name of the input dataset, currently only "magpie"
#' @param target name of the target dataset, currently only "luh2"
#' @return nonland data with target categories
#' @author Pascal Sauer
calcNonlandHarmonizedCategories <- function(input = "magpie", target = "luh2mod") {
  x <- calcOutput("NonlandInput", input = input, aggregate = FALSE)

  # separate categories that need to be mapped from those that don't
  # wood <- x[, , c("rndwd", "fulwd")]
  x <- x[, , c("rndwd", "fulwd"), invert = TRUE]

  # map categories using weights from land categorization
  getItems(x, 3) <- sub("_fertilizer", "", getItems(x, 3))
  map <- toolLandCategoriesMapping(input, target)

  # get weights for disaggregation to reference categories
  ref <- calcOutput("LandCategorizationWeight", map = map, geometry = attr(x, "geometry"),
                    crs = attr(x, "crs"), aggregate = FALSE)

  # sum up weights for irrigated/rainfed
  irrigatedNames <- grep("irrigated", getItems(ref, 3), value = TRUE)
  rainfedNames <- gsub("irrigated", "rainfed", irrigatedNames)
  irrigated <- ref[, , irrigatedNames]
  getItems(irrigated, 3) <- gsub("_irrigated", "", irrigatedNames)
  rainfed <- ref[, , rainfedNames]
  getItems(rainfed, 3) <- gsub("_rainfed", "", rainfedNames)
  ref <- mbind(ref[, , c(irrigatedNames, rainfedNames), invert = TRUE], irrigated + rainfed)
  stopifnot(!grepl("irrigated|rainfed", getItems(ref, 3)))

  map$reference <- sub(", irrigated", "", map$reference)
  map$dataInput <- sub("_irrigated", "", map$dataInput)
  map$dataOutput <- sub("_irrigated", "", map$dataOutput)
  map$merge <- gsub("_irrigated", "", map$merge)
  map <- map[map$dataInput %in% getItems(x, 3), ]

  ref <- ref[, , unique(map$merge)]
  y   <- toolAggregate(x, map, dim = 3, from = "dataInput", to = "merge", weight = ref)
  out <- toolAggregate(y, map, dim = 3, from = "merge",     to = "dataOutput")
  out[, , "c3per"] <- out[, , "c3per"] + out[, , "c3per_biofuel_2nd_gen"]
  out[, , "c4per"] <- out[, , "c4per"] + out[, , "c4per_biofuel_2nd_gen"]
  out <- out[, , c("c3per_biofuel_2nd_gen", "c4per_biofuel_2nd_gen"), invert = TRUE]
  getItems(out, 3) <- paste0(getItems(out, 3), "_fertilizer")

  out <- mbind(out)#, wood)
  attr(out, "crs") <- attr(x, "crs")
  attr(out, "geometry") <- attr(x, "geometry")

  # check data for consistency
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
  expectedCategories <- c(paste0(c("c3ann", "c4ann", "c3per", "c4per", "c3nfx"), "_fertilizer"))#, "rndwd", "fulwd")
  toolExpectTrue(setequal(getItems(out, dim = 3), expectedCategories), "Nonland categories match target definition")
  toolExpectTrue(all(out >= 0), "All values are >= 0")

  return(list(x = out,
              isocountries = FALSE,
              unit = "rndwd, fulwd: 1; *_fertilizer: kg yr-1",
              min = 0,
              description = "Input data with nonland categories remapped to categories of target dataset"))
}
