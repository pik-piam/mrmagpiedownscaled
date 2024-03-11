#' calcNonlandHarmonized
#'
#' Harmonize nonland input data to target data using the specified method, checking
#' data for consistency before returning.
#'
#' @param input name of the input dataset, currently only "magpie"
#' @param target name of the target dataset, currently only "luh2"
#' @param harmonizeYear start of the harmonization period, years before
#' this are taken from the target dataset
#' @param finalYear end of the harmonization period, years after this are
#' taken from the input dataset
#' @param method harmonization method, see \code{\link{toolGetHarmonizer}} for available methods
#' @return harmonized nonland data
#' @author Pascal Sauer
#' @importFrom mstools toolExpectTrue
calcNonlandHarmonized <- function(input = "magpie", target = "luh2mod",
                                  harmonizeYear = 2015, finalYear = 2050,
                                  method = "extrapolateFade") {
  xInput <- calcOutput("NonlandHarmonizedCategories", input = input, aggregate = FALSE)
  geometry <- attr(xInput, "geometry")
  crs <- attr(xInput, "crs")

  # get target data in spatial resolution of input data
  xTarget <- calcOutput("NonlandTarget", target = target, aggregate = FALSE)
  ref <- as.SpatVector(xInput[, 1, 1])[, c(".region", ".id")]
  xTarget <- terra::extract(xTarget, ref, sum, na.rm = TRUE, bind = TRUE)
  xTarget <- as.magpie(xTarget)

  stopifnot(setequal(getItems(xInput, 3), getItems(xTarget, 3)))
  xTarget <- xTarget[, , getItems(xInput, 3)] # harmonize order of dim 3

  if (method == "extrapolateFade") {
    # growthAveragePeriod = 10 would lead to insane growth rate due to
    # edge case (CHA.12 has 0 in 2005 but 8*10^7 in 2015), workaround by setting it to 15
    out <- toolHarmonizeExtrapolateFade(xInput, xTarget, harmonizeYear = harmonizeYear, finalYear = finalYear,
                                        constantSum = FALSE, growthAveragePeriod = 15)
  } else {
    harmonizer <- toolGetHarmonizer(method)
    out <- harmonizer(xInput, xTarget, harmonizeYear = harmonizeYear, finalYear = finalYear)
  }

  attr(out, "geometry") <- geometry
  attr(out, "crs")      <- crs

  # checks
  toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
  toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")),
                 "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(xTarget, dim = 3)),
                 "Nonland categories remain unchanged")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")
  # SpatRaster can hold values up to ~10^40 before replacing with Inf, so check we are well below that
  toolExpectTrue(max(out) < 10^30, "All values are < 10^30")

  return(list(x = out,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg yr-1",
              min = 0,
              description = "Harmonized nonland data"))
}
