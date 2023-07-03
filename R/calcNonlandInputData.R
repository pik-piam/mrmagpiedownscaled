calcNonlandInputData <- function(input = "magpie") {
  if (input == "magpie") {
    wood <- readSource("Magpie", subtype = "woodHarvest")
    geometry <- attr(wood, "geometry")
    crs <- attr(wood, "crs")

    stopifnot(!is.na(wood),
              identical(getNames(wood), c("wood", "woodfuel")))
    getNames(wood) <- c("rndwd", "fulwd")
    wood <- wood / dimSums(wood, dim = 3)
    wood[is.na(wood)] <- 0 # replace NAs introduced by division by zero

    out <- wood
    attr(out, "geometry") <- geometry
    attr(out, "crs") <- crs
  } else {
    stop("Unsupported input dataset \"", input, "\"")
  }

  # check data for consistency
  toolCheck("Nonland input data", {
    toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
    toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
    toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
    toolExpectTrue(all(out >= 0), "All values are >= 0")
    toolExpectTrue(all(out <= 1.0001), "All values are < 1.0001")
  })
  attr(out, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = out,
              isocountries = FALSE,
              unit = "1",
              min = 0,
              max = 1.0001,
              description = "Nonland input data for data harmonization and downscaling pipeline"))
}
