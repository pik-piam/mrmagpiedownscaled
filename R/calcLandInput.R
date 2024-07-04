#' calcLandInput
#'
#' Prepare the land input data for the category mapping, checking data for consistency before returning.
#' All "Land" functions deal with area data, as opposed to "Nonland" functions which deal with non-area
#' data such as the amount of applied fertilizer. These are treated differently, because for area
#' data other constraints apply, e.g. the total area must be constant over time.
#'
#' @param input name of an input dataset, currently only "magpie"
#' @return land input data
#' @author Jan Philipp Dietrich, Pascal Sauer
calcLandInput <- function(input = "magpie") {
  if (input == "magpie") {
    land <- readSource("MagpieFulldataGdx")
    crop <- readSource("MagpieFulldataGdx", subtype = "crop")
    getItems(crop, dim = 3.1, full = TRUE) <- sub("\\.", "_", getItems(crop, dim = 3, full = TRUE))
    getItems(crop, dim = 3.2) <- NULL

    out <- mbind(land[, , "crop", invert = TRUE], crop)

    # 1st gen biofuel is only modeled implicitly in magpie via demand, and
    # because of trade it is unclear on what area 1st gen biofuel is grown,
    # also 1st gen biofuel is quickly phased out in magpie, so we fill
    # biofuel_1st_gen with zeros and rely on the harmonization to produce
    # a plausible 1st gen biofuel time series
    out <- add_columns(out, "biofuel_1st_gen", fill = 0)
  } else {
    stop("Unsupported input type \"", input, "\"")
  }

  # check data for consistency
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")),
                 "Dimensions are named correctly")
  map <- toolLandCategoriesMapping(input = input, target = "luh2mod")
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
