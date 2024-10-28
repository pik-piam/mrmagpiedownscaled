#' calcLandInput
#'
#' Prepare the land input data for the category mapping, checking data for consistency before returning.
#' All "Land" functions deal with area data, as opposed to "Nonland" functions which deal with non-area
#' data such as the amount of applied fertilizer. These are treated differently, because for area
#' data other constraints apply, e.g. the total area must be constant over time.
#'
#' input = "magpie7categories": includes the 7 land use categories crop,
#' past (pasture, including rangeland),
#' forestry (managed forest), primforest, secdforest, urban,
#' and other (other land, not other crop types).
#'
#' input = "magpie": deviates from magpie7categories in that crop is
#' disaggregated to specific crop types, furthermore 1st gen biofuel is
#' added and filled with zeros. 1st gen biofuel is only modeled implicitly in
#' magpie via demand, and because of trade it is unclear on what area 1st gen
#' biofuel is grown, also 1st gen biofuel is quickly phased out in magpie, so
#' we fill biofuel_1st_gen with zeros and rely on the harmonization to produce
#' a plausible 1st gen biofuel time series.
#'
#' @param input name of an input dataset, "magpie" or "magpie7categories"
#' @return land input data
#' @author Jan Philipp Dietrich, Pascal Sauer
calcLandInput <- function(input) {
  land <- readSource("MagpieFulldataGdx")

  if (input == "magpie") {
    crop <- readSource("MagpieFulldataGdx", subtype = "crop")
    getItems(crop, dim = 3.1, full = TRUE) <- sub("\\.", "_", getItems(crop, dim = 3, full = TRUE))
    getItems(crop, dim = 3.2) <- NULL

    out <- mbind(land[, , "crop", invert = TRUE], crop)

    # see note in the documentation of this function
    out <- add_columns(out, "biofuel_1st_gen", fill = 0)

    expectedCategories <- toolLandCategoriesMapping(input = input, target = "luh2mod")$dataInput
  } else if (input == "magpie7categories") {
    out <- land
    expectedCategories <- c("crop", "past", "forestry", "primforest", "secdforest", "urban",  "other")
  } else {
    stop("Unsupported input type \"", input, "\"")
  }

  # check data for consistency
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")),
                 "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), expectedCategories),
                 "Land input categories match expactation")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-4, "Total area is constant over time")
  toolExpectTrue(all(out[, -1, "primforest"] <= setYears(out[, -nyears(out), "primforest"], getYears(out[, -1, ]))),
                 "primforest is never expanding", falseStatus = "warn")

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Land input data for data harmonization and downscaling pipeline"))
}
