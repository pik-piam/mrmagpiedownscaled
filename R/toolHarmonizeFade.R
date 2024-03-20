#' toolHarmonizeFade
#'
#' Harmonize two datasets by fading from one to the other, using the following formula:
#' .inputFactor(year) * input[, year, ] + (1 - .inputFactor(year)) * target[, year, ]
#' where .inputFactor is (year - harmonizationStart) / (harmonizationEnd - harmonizationStart).
#' Requires both datasets to have all years including and between harmonizationStart and harmonizationEnd,
#' so it is not suitable for harmonization periods in the future if historical data is used.
#'
#' @param input magpie object to fade to, usually model projections
#' @param target magpie object to fade from, usually historical data, this argument is called "target"
#' for consistency with argument names of other functions, it is somewhat misleading here,
#' because data after the harmonizationEnd will not be taken from target, but from input
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @return magpie object with harmonized data
#' @author Pascal Sauer
toolHarmonizeFade <- function(input, target, harmonizationPeriod) {
  harmonizationStart <- harmonizationPeriod[1]
  harmonizationEnd <- harmonizationPeriod[2]
  inputYears <- getYears(input, as.integer = TRUE)
  targetYears <- getYears(target, as.integer = TRUE)
  stopifnot(length(harmonizationPeriod) == 2,
            round(harmonizationPeriod) == harmonizationPeriod,
            harmonizationEnd > harmonizationStart,
            harmonizationStart %in% inputYears,
            harmonizationStart %in% targetYears,
            harmonizationEnd %in% inputYears,
            inputYears[inputYears < harmonizationEnd] %in% targetYears,
            all.equal(getItems(input, dim = 1), getItems(target, dim = 1)),
            all.equal(getItems(input, dim = 3), getItems(target, dim = 3)))
  years <- sort(union(inputYears, targetYears))
  stopifnot(years[years < harmonizationEnd] %in% targetYears,
            years[years > harmonizationStart] %in% inputYears)
  .inputFactor <- function(year) {
    fac <- (year - harmonizationStart) / (harmonizationEnd - harmonizationStart)
    stopifnot(0 < fac, fac < 1)
    return(fac)
  }
  out <- target
  for (year in years[years > harmonizationStart & years < harmonizationEnd]) {
    out[, year, ] <- .inputFactor(year) * input[, year, ] + (1 - .inputFactor(year)) * target[, year, ]
  }
  yearsAfterHarmonization <- years[years >= harmonizationEnd]
  out <- out[, yearsAfterHarmonization, , invert = TRUE]
  out <- mbind(out, input[, yearsAfterHarmonization, ])
  return(out)
}
