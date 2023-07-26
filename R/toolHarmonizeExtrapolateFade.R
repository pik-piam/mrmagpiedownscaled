toolHarmonizeExtrapolateFade <- function(input, target, harmonizeYear, finalYear) {
  # extrapolate target data till finalYear and afterwards fade from one dataset to the other
  inputYears <- getYears(input, as.integer = TRUE)
  targetYears <- getYears(target, as.integer = TRUE)
  growthAveragePeriod <- 10
  stopifnot(round(harmonizeYear) == harmonizeYear,
            round(finalYear) == finalYear,
            finalYear > harmonizeYear,
            harmonizeYear %in% inputYears,
            harmonizeYear %in% targetYears,
            finalYear %in% inputYears,
            min(targetYears) <= harmonizeYear - growthAveragePeriod,
            all.equal(getItems(input, dim = 1), getItems(target, dim = 1)),
            all.equal(getItems(input, dim = 3), getItems(target, dim = 3)))

  # calculate average growth over growth period in target data
  .growth <- function(target, harmonizeYear, growthAveragePeriod) {
    growthPeriod <- time_interpolate(target, c(harmonizeYear - growthAveragePeriod, harmonizeYear))
    growth <- setYears(growthPeriod[, 2, ] / growthPeriod[, 1, ], NULL)^(1 / growthAveragePeriod)
    growth[is.na(growth) | is.infinite(growth)] <- 1
    # set growth rates to 0 for land types with an area share of less than 0.1% to avoid distortions
    # due to potentially unrealistic growth rates
    growth <- growth * (2 * setYears(growthPeriod[, 1, ] / dimSums(growthPeriod[, 1, ], dim = 3) > 0.001, NULL) - 1)
    growth[growth < 0] <- 1
    return(growth)
  }
  growth <- .growth(target, harmonizeYear, growthAveragePeriod)
  transitionYears <- inputYears[inputYears >= harmonizeYear & inputYears <= finalYear]
  extrapolationYears <- transitionYears[transitionYears > max(targetYears)]
  lastAvailableTransitionYear <-  max(transitionYears[transitionYears <= max(targetYears)])
  extrapolationRange <- (extrapolationYears - lastAvailableTransitionYear)
  names(extrapolationRange) <- paste0("y", extrapolationYears)
  extrapolationRange <- as.magpie(extrapolationRange)

  exTarget  <- time_interpolate(target, transitionYears, extrapolation_type = "constant",
                                integrate_interpolated_years = TRUE)
  # dampen growth rates by taking the square root of it to reduce too extreme behavior
  exTarget[, extrapolationYears, ] <- exTarget[, extrapolationYears, ] * growth^(extrapolationRange / 2)
  exTarget[, extrapolationYears, ] <- exTarget[, extrapolationYears, ] *
    dimSums(setYears(target[, harmonizeYear, ], NULL), dim = 3) / dimSums(exTarget[, extrapolationYears, ], dim = 3)
  out <- convergence(exTarget[, transitionYears, ], input[, transitionYears, ],
                     start_year = harmonizeYear, end_year = finalYear)
  out <- mbind(target[, (getYears(target, as.integer = TRUE) < min(getYears(out, as.integer = TRUE))), ], out,
               input[, (getYears(input, as.integer = TRUE) > max(getYears(out, as.integer = TRUE))), ])
  return(out)

}
