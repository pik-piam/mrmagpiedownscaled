#' calcLandTargetExtrapolated
#'
#' First aggregate to low resolution, then extrapolate to the given years
#' using toolExtrapolate.
#'
#' @param target name of the target dataset, options are: luh2, luh2mod
#' luh2mod will split secdf into forestry and secdf
#' @return extrapolated land target data
#' @author Pascal Sauer
calcLandTargetExtrapolated <- function(input = "magpie", target = "luh2mod",
                                       harmonizationPeriod = c(2015, 2050)) {
  xInput <- calcOutput("LandHarmonizedCategories", input = input,
                       target = target, aggregate = FALSE)
  xTarget <- calcOutput("LandTarget", target = target, aggregate = FALSE)

  # bring target data to spatial resolution of input data
  ref    <- as.SpatVector(xInput[, 1, 1])[, c(".region", ".id")]
  xTarget <- terra::extract(xTarget, ref, sum, na.rm = TRUE, bind = TRUE)
  xTarget <- as.magpie(xTarget)
  stopifnot(setequal(getItems(xInput, 3), getItems(xTarget, 3)))
  xTarget <- xTarget[, , getItems(xInput, 3)] # harmonize order of dim 3

  # extrapolate
  inputYears <- getYears(xInput, as.integer = TRUE)
  transitionYears <- inputYears[inputYears > harmonizationPeriod[1] & inputYears < harmonizationPeriod[2]]
  exTarget <- toolExtrapolate(xTarget, transitionYears)
  exTarget[exTarget < 0] <- 0

  # normalize exTarget so that its total sum over all layers agrees for all time steps
  # with the sum over all layers in target in the harmonization year (e.g. makes sure
  # that the land changes in a land data set do not alter the total sum of land.)
  targetArea <- dimSums(setYears(xTarget[, harmonizationPeriod[1], ], NULL), dim = 3)
  exTarget <- exTarget * targetArea / dimSums(exTarget, dim = 3)
  exTarget[is.na(exTarget)] <- 0

  out <- mbind(xTarget, exTarget)

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Extrapolated land target data for harmonization"))
}
