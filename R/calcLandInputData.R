# Pascal

calcLandInputData <- function(input = "magpie") {
  if (input == "magpie") {
    land <- readSource("Magpie")
    crop <- readSource("Magpie", subtype = "crop")
    getItems(crop, dim = 3.1, full = TRUE) <- sub("\\.", "_", getItems(crop, dim = 3, full = TRUE))
    getItems(crop, dim = 3.2) <- NULL

    geometry <- attr(land, "geometry")
    crs <- attr(land, "crs")

    out <- mbind(land[, , "crop", invert = TRUE], crop)

    # 1st gen biofuel is only modeled implicitly in magpie via demand, and
    # because of trade it is unclear on what area 1st gen biofuel is grown,
    # also 1st gen biofuel is quickly phased out in magpie, so we fill
    # biofuel_1st_gen with zeros and rely on the harmonization to produce
    # a plausible 1st gen biofuel time series
    out <- add_columns(out, "biofuel_1st_gen", fill = 0)

    attr(out, "geometry") <- geometry
    attr(out, "crs") <- crs
  } else {
    stop("Unsupported input type \"", input, "\"")
  }

  # check data for consistency
  toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
  toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
  map <- toolLandCategoriesMapping(input = input, target = "luh2")
  toolExpectTrue(setequal(getItems(out, dim = 3), map$dataInput),
                 "Land input categories match the corresponding mapping")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-4, "Total area is constant over time")

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Land input data for data harmonization and downscaling pipeline"))
}
