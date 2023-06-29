toolHarmonizeExtrapolateFade <- function(input, target, harmonizeYear, finalYear) {
  # extrapolate target data till finalYear and afterwards fade from one dataset to the other
  inputYears <- getYears(input, as.integer = TRUE)
  targetYears <- getYears(target, as.integer = TRUE)
  stopifnot(round(harmonizeYear) == harmonizeYear,
            round(finalYear) == finalYear,
            finalYear > harmonizeYear,
            harmonizeYear %in% inputYears,
            harmonizeYear %in% targetYears,
            finalYear %in% inputYears,
            all.equal(getItems(input, dim = 1), getItems(target, dim = 1)),
            all.equal(getItems(input, dim = 3), getItems(target, dim = 3)))

  transitionYears <- inputYears[inputYears >= harmonizeYear & inputYears <= finalYear]
  extrapolationYears <- transitionYears[transitionYears > max(targetYears)]
  inputGrowthRate <- input[,transitionYears,]/(setYears(input[,harmonizeYear,], NULL))
  inputGrowthRate[is.na(inputGrowthRate) | is.infinite(inputGrowthRate)] <- 0
  target  <- time_interpolate(target, transitionYears, extrapolation_type = "constant", integrate_interpolated_years = TRUE)
  target[,extrapolationYears,] <- target[,extrapolationYears,] * inputGrowthRate[,extrapolationYears,]
  target[,extrapolationYears,] <- target[,extrapolationYears,] * dimSums(setYears(target[,harmonizeYear,], NULL), dim = 3) / dimSums(target[,extrapolationYears,], dim = 3)
  return(toolHarmonizeFade(input, target, harmonizeYear, finalYear))

}
