#' calcNonlandHarmonized
#'
#' Harmonize nonland input data to target data using the specified method, checking
#' data for consistency before returning.
#'
#' @param input name of the input dataset, currently only "magpie"
#' @param target name of the target dataset, currently only "luh2"
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param method harmonization method, see \code{\link{toolGetHarmonizer}} for available methods
#' @return harmonized nonland data
#' @author Pascal Sauer
calcNonlandHarmonized <- function(input = "magpie", target = "luh2mod",
                                  harmonizationPeriod = c(2015, 2050),
                                  method = "extrapolateFade") {
  xInput <- calcOutput("NonlandHarmonizedCategories", input = input, aggregate = FALSE)
  geometry <- attr(xInput, "geometry")
  crs <- attr(xInput, "crs")

  # get target data in spatial resolution of input data
  xTarget <- calcOutput("NonlandTarget", target = target, aggregate = FALSE)
  xTarget <- as.magpie(xTarget)
  resolutionMapping <- calcOutput("ResolutionMapping", aggregate = FALSE)
  xTarget <- toolAggregate(xTarget, resolutionMapping, from = "cell", to = "lowRes")
  names(dimnames(xTarget))[1] <- "region.id"
  stopifnot(setequal(getItems(xInput, 1), getItems(xTarget, 1)),
            setequal(getItems(xInput, 3), getItems(xTarget, 3)))
  xTarget <- xTarget[getItems(xInput, 1), , ] # harmonize order of dim 1
  xTarget <- xTarget[, , getItems(xInput, 3)] # harmonize order of dim 3

  if (method == "extrapolateFade") {
    # growthAveragePeriod = 10 would lead to insane growth rate due to
    # edge case (CHA.12 has 0 in 2005 but 8*10^7 in 2015), workaround by setting it to 15
    out <- toolHarmonizeExtrapolateFade(xInput, xTarget, harmonizationPeriod = harmonizationPeriod,
                                        constantSum = FALSE, growthAveragePeriod = 15)
  } else {
    harmonizer <- toolGetHarmonizer(method)
    out <- harmonizer(xInput, xTarget, harmonizationPeriod = harmonizationPeriod)
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
