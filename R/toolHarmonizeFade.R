# Pascal

toolHarmonizeFade <- function(input, target, harmonizeYear, finalYear) {
  inputYears <- getYears(input, as.integer = TRUE)
  targetYears <- getYears(target, as.integer = TRUE)
  stopifnot(round(harmonizeYear) == harmonizeYear,
            round(finalYear) == finalYear,
            finalYear > harmonizeYear,
            harmonizeYear %in% inputYears,
            harmonizeYear %in% targetYears,
            finalYear %in% inputYears,
            inputYears[inputYears < finalYear] %in% targetYears,
            all.equal(getItems(input, dim = 1), getItems(target, dim = 1)),
            all.equal(getItems(input, dim = 3), getItems(target, dim = 3)))
  years <- sort(union(inputYears, targetYears))
  stopifnot(years[years < finalYear] %in% targetYears,
            years[years > harmonizeYear] %in% inputYears)
  .inputFactor <- function(year) {
    fac <- (year - harmonizeYear) / (finalYear - harmonizeYear)
    stopifnot(0 < fac, fac < 1)
    return(fac)
  }
  out <- target
  for (year in years[years > harmonizeYear & years < finalYear]) {
    out[, year, ] <- .inputFactor(year) * input[, year, ] + (1 - .inputFactor(year)) * target[, year, ]
  }
  yearsAfterHarmonization <- years[years >= finalYear]
  out <- out[, yearsAfterHarmonization, , invert = TRUE]
  out <- mbind(out, input[, yearsAfterHarmonization, ])
  return(out)
}
