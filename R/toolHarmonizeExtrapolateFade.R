#' toolHarmonizeExtrapolateFade
#'
#' Tool function for creating a harmonized data set with a smooth transition
#' from historic target data to simulated input data.
#'
#' A smooth transition is achieved in 2 steps: 1) extrapolating the target data
#' into the future so that it covers the whole transition period and 2) fade
#' over from the extrapolated target data to the input data.
#'
#' Extrapolation of target data is achieved by taking the historically observed
#' growth rates of the different categories, dampen them (to avoid overestimation
#' of trends), apply them into the future and finally, if \code{constantSum} is
#' set to TRUE, normalize the result to the total sum of each spatial entity.
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
#' @author Jan Philipp Dietrich

toolHarmonizeExtrapolateFade <- function(input, target, harmonizationPeriod,
                                         constantSum, growthAveragePeriod = 10) {
  # extrapolate target data till the end of the harmonization period, then
  # fade from one dataset to the other
  a <- harmonizationPeriod[1]
  b <- harmonizationPeriod[2]
  inputYears <- getYears(input, as.integer = TRUE)
  targetYears <- getYears(target, as.integer = TRUE)
  stopifnot(length(harmonizationPeriod) == 2,
            round(harmonizationPeriod) == harmonizationPeriod,
            b > a,
            a %in% inputYears,
            a %in% targetYears,
            b %in% inputYears,
            min(targetYears) <= a - growthAveragePeriod,
            all.equal(getItems(input, dim = 1), getItems(target, dim = 1)),
            all.equal(getItems(input, dim = 3), getItems(target, dim = 3)))

  # calculate average growth over growth period in target data
  .growth <- function(target, growthPeriodEnd, growthAveragePeriod, constantSum) {
    growthPeriod <- time_interpolate(target, c(growthPeriodEnd - growthAveragePeriod, growthPeriodEnd))
    growth <- setYears(growthPeriod[, 2, ] / growthPeriod[, 1, ], NULL)^(1 / growthAveragePeriod)
    growth[is.na(growth) | is.infinite(growth)] <- 1
    if (constantSum) {
      # set growth rates to 0 for land types with an area share of less than 0.1% to avoid distortions
      # due to potentially unrealistic growth rates (values to be resetted are first set to negative values
      # via multiplication with -1 and afterwards set to a growth rate of 1)
      growth <- growth * (2 * setYears(growthPeriod[, 1, ] / dimSums(growthPeriod[, 1, ], dim = 3) > 0.001, NULL) - 1)
      growth[growth < 0 | is.na(growth)] <- 1
    }
    return(growth)
  }
  growth <- .growth(target, growthPeriodEnd = a, growthAveragePeriod, constantSum)
  transitionYears <- inputYears[inputYears >= a & inputYears <= b]
  extrapolationYears <- transitionYears[transitionYears > max(targetYears)]
  lastAvailableTransitionYear <-  max(transitionYears[transitionYears <= max(targetYears)])
  extrapolationRange <- (extrapolationYears - lastAvailableTransitionYear)
  names(extrapolationRange) <- paste0("y", extrapolationYears)
  extrapolationRange <- as.magpie(extrapolationRange)

  exTarget  <- time_interpolate(target, transitionYears, extrapolation_type = "constant",
                                integrate_interpolated_years = TRUE)
  # dampen growth rates by taking the square root of it to reduce too extreme behavior
  exTarget[, extrapolationYears, ] <- exTarget[, extrapolationYears, ] * growth^(extrapolationRange / 2)
  if (constantSum) {
    # scale exTarget so that its total sum over all layers agrees for all time steps with the sum over all layers
    # in target in the harmonization year (e.g. makes sure that the land changes in a land data set do not alter
    # the total sum of land.)
    exTarget[, extrapolationYears, ] <- exTarget[, extrapolationYears, ] *
      dimSums(setYears(target[, a, ], NULL), dim = 3) / dimSums(exTarget[, extrapolationYears, ], dim = 3)
  }
  out <- convergence(exTarget[, transitionYears, ], input[, transitionYears, ],
                     start_year = a, end_year = b)
  out <- mbind(target[, (getYears(target, as.integer = TRUE) < min(getYears(out, as.integer = TRUE))), ], out,
               input[, (getYears(input, as.integer = TRUE) > max(getYears(out, as.integer = TRUE))), ])
  return(out)
}
