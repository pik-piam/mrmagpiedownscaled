#' calcLandHarmonizedCategories
#'
#' Computes the land input data in target land categories. Splitting of land
#' categories is performed under use of internal land weights reflecting the
#' prevalence of a certain land category in the given area.
#'
#' Mapping from input to target categories is achieved via a merge of a land input
#' mapping to reference categories and a mapping between land target categories and
#' the same reference categories. Thereby a new source or new target can be supported
#' by supplying a map of that new input and/or target to the reference categories.
#'
#' @param input name of the land input source to be used
#' @param target name of the land target source to be used
#' @author Jan Philipp Dietrich

calcLandHarmonizedCategories <- function(input = "magpie", target = "luh2mod") {
  x   <- calcOutput("LandInput", input = input, aggregate = FALSE)
  map <- toolLandCategoriesMapping(input, target)

  # get weights for disaggregation to reference categories
  ref <- calcOutput("LandCategorizationWeight", map = map, geometry = attr(x, "geometry"),
                    crs = attr(x, "crs"), aggregate = FALSE)
  y   <- toolAggregate(x, map, dim = 3, from = "dataInput", to = "merge", weight = ref)
  out <- toolAggregate(y, map, dim = 3, from = "merge",     to = "dataOutput")
  attr(out, "crs") <- attr(x, "crs")
  attr(out, "geometry") <- attr(x, "geometry")

  # check data for consistency
  mstools::toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")),
                          "Dimensions are named correctly")
  mstools::toolExpectTrue(setequal(getItems(out, dim = 3), map$dataOutput), "Land categories match target definition")
  mstools::toolExpectTrue(all(out >= 0), "All values are >= 0")
  outSum <- dimSums(out, dim = 3)
  mstools::toolExpectLessDiff(outSum, outSum[, 1, ], 10^-6, "Total areas stay constant over time")
  mstools::toolExpectLessDiff(outSum, dimSums(x, dim = 3), 10^-6,
                              "Total areas are not affected by recategorization")

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Input data with land categories remapped to categories of target dataset"))
}
