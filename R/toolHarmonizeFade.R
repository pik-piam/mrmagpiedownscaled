#' toolHarmonizeFade
#'
#' Harmonize two datasets by fading from one to the other, using the following formula:
#' .inputFactor(year) * input[, year, ] + (1 - .inputFactor(year)) * target[, year, ]
#' where .inputFactor is (year - harmonizeYear) / (finalYear - harmonizeYear).
#' Requires both datasets to have all years including and between harmonizeYear and finalYear,
#' so it is not suitable for harmonization periods in the future if historical data is used.
#'
#' @param input magpie object to fade to, usually model projections
#' @param target magpie object to fade from, usually historical data, this argument is called "target"
#' for consistency with argument names of other functions, it is somewhat misleading here,
#' because data after the finalYear will not be taken from target, but from input
#' @param harmonizeYear year to start fading, data at and before this year will be taken from target
#' @param finalYear year to stop fading, data at and after this year will be taken from input
#' @return magpie object with harmonized data
#' @author Pascal Führlich
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
