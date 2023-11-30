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
calcNonlandHarmonized <- function(input = "magpie", target = "luh2mod",
                                  harmonizeYear = 2015, finalYear = 2050,
                                  method = "extrapolateFade") {
  xInput <- calcOutput("NonlandHarmonizedCategories", input = input, aggregate = FALSE)
  geometry <- attr(xInput, "geometry")
  crs <- attr(xInput, "crs")

  xTarget <- calcOutput("NonlandTarget", target = target, aggregate = FALSE)
  # bring target data to spatial resolution of input data
  ref <- as.SpatVector(xInput[, 1, 1])[, c(".region", ".id")]
  xTarget <- terra::extract(xTarget, ref, sum, na.rm = TRUE, bind = TRUE)
  xTarget <- as.magpie(xTarget)

  stopifnot(setequal(getItems(xInput, 3), getItems(xTarget, 3)))
  xTarget <- xTarget[, , getItems(xInput, 3)] # harmonize order of dim 3

  if (method == "extrapolateFade") {
    out <- toolHarmonizeExtrapolateFade(xInput, xTarget, harmonizeYear = harmonizeYear,
                                        finalYear = finalYear, constantSum = FALSE)
  } else {
    harmonizer <- toolGetHarmonizer(method)
    out <- harmonizer(xInput, xTarget, harmonizeYear = harmonizeYear, finalYear = finalYear)
  }

  attr(out, "geometry") <- geometry
  attr(out, "crs")      <- crs

  # checks
  toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
  toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(xTarget, dim = 3)), "Nonland categories remain unchanged")
  toolExpectTrue(all(out >= 0), "All values are >= 0")

  return(list(x = out,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha; fertilizer: kg yr-1",
              min = 0,
              description = "Harmonized nonland data"))
}
