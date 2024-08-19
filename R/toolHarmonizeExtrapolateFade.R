#' toolHarmonizeExtrapolateFade
#'
#' Tool function for creating a harmonized data set with a smooth transition
#' from historic target data to simulated input data.
#'
#' A smooth transition is achieved in 2 steps: 1) extrapolating the target data
#' into the future so that it covers the whole transition period and 2) fade
#' over from the extrapolated target data to the input data.
#'
#' Extrapolation is done by \code{\link{toolExtrapolate}}. After extrapolation
#' negative values are replaced by zeros and if \code{constantSum} is
#' set to TRUE, the result is normalized to the total sum of each spatial entity.
#'
#' Please note that it is crucial to set the switch \code{constantSum} correctly
#' for the specific application as otherwise the results will be inconsistent.
#'
#' @param input input data
#' @param target target data
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param constantSum boolean indicating whether the total sum over all layers
#' is suppossed to stay contstant (e.g. sum over all land types) or not.
#' @param growthAveragePeriod when projecting into the future, how many years
#' to look back from last year to determine growth rate
#' @author Jan Philipp Dietrich, Pascal Sauer
toolHarmonizeExtrapolateFade <- function(input, target, harmonizationPeriod, constantSum) {
  # extrapolate target data till the end of the harmonization period, then
  # fade from one dataset to the other
  a <- harmonizationPeriod[1]
  b <- harmonizationPeriod[2]
  inputYears <- getYears(input, as.integer = TRUE)
  targetYears <- getYears(target, as.integer = TRUE)
  transitionYears <- inputYears[inputYears > a & inputYears < b]
  stopifnot(length(harmonizationPeriod) == 2,
            round(harmonizationPeriod) == harmonizationPeriod,
            b > a,
            a %in% inputYears,
            a %in% targetYears,
            b %in% inputYears,
            all.equal(getItems(input, dim = 1), getItems(target, dim = 1)),
            all.equal(getItems(input, dim = 3), getItems(target, dim = 3)))

  exTarget <- toolExtrapolate(target, transitionYears)
  exTarget[exTarget < 0] <- 0

  if (constantSum) {
    # scale exTarget so that its total sum over all layers agrees for all time steps with the sum over all layers
    # in target in the harmonization year (e.g. makes sure that the land changes in a land data set do not alter
    # the total sum of land.)
    exTarget <- exTarget * dimSums(setYears(target[, a, ], NULL), dim = 3) / dimSums(exTarget, dim = 3)
    exTarget[is.na(exTarget)] <- 0
  }

  # fade over from extrapolated target data to input data
  out <- convergence(exTarget, input[, transitionYears, ],
                     start_year = a, end_year = b, type = "s")

  out <- mbind(target[, getYears(target, as.integer = TRUE) < min(getYears(out, as.integer = TRUE)), ],
               out,
               input[, getYears(input, as.integer = TRUE) > max(getYears(out, as.integer = TRUE)), ])
  return(out)
}
