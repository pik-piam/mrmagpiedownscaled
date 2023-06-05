calcLandInputData <- function(input = "magpie") {
  if (input == "magpie") {
    out <- madrat::readSource("Magpie")
  } else {
    stop("Unsupported input type \"", input, "\"")
  }

  # check data for consistency
  toolCheck("Land input data", {
    toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
    toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
    toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
    map <- toolLandCategoriesMapping(input = input, target = "luh2")
    toolExpectTrue(setequal(getItems(out, dim = 3), map$dataInput),
                   "Land input categories match the corresponding mapping")
    toolExpectTrue(all(out >= 0), "All values are > 0")
    outSum <- dimSums(out, dim = 3)
    toolExpectLessDiff(outSum, outSum[, 1, ], 10^-5, "Total areas in output stay constant over time")
  })

  return(list(x = out,
              weight = NULL,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              cache = FALSE,
              description = "Land input data for data harmonization and downscaling pipeline"))
}
