calcLandHarmonizedCategories <- function(input = "magpie", target = "luh2") {

  if (input == "magpie") {
    x <- madrat::readSource("Magpie")
  } else {
    stop("Unsupported input type \"", input, "\"")
  }
  map <- toolLandCategoriesMapping(input, target)

  # get weights for disaggregation to reference categories
  ref <- calcOutput("LandCategorizationWeight", map = map, geometry = attr(x, "geometry"),
                    crs = attr(x, "crs"), aggregate = FALSE)
  y   <- toolAggregate(x, map, dim = 3, from = "dataInput", to = "merge", weight = ref)
  out <- toolAggregate(y, map, dim = 3, from = "merge",     to = "dataOutput")
  attr(out, "crs") <- attr(x, "crs")
  attr(out, "geometry") <- attr(x, "geometry")

  # check data for consistency
  sep <- paste(rep("-", 100), collapse = "")
  vcat(1, sep, show_prefix = FALSE)
  vcat(1, "Check LandHarmonizedCategories output", show_prefix = FALSE)
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), map$dataOutput), "Land categories match target definition")
  toolExpectTrue(all(out >= 0), "All values are > 0")
  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-6, "Total areas stay constant over time")
  toolExpectLessDiff(outSum, dimSums(x, dim = 3), 10^-6, "Total areas are not affected by recategorization")
  vcat(1, sep, show_prefix = FALSE)

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Input data with land categories remapped to categories of output target data set"))
}
