#' calcESMStates
#'
#' Create ESM compatible state information
#'
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @return ESM compatible states information
#' @author Pascal Sauer, Jan Philipp Dietrich
calcESMStates <- function(harmonizationPeriod = c(2015, 2050)) {

  x <- calcOutput("LandReport", outputFormat = "ESM",
                  harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)

  statesVariables <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr",
                       "primf", "primn", "range", "secdf", "secdn", "urban")

  x <- x[, getYears(x, as.integer = TRUE) %in% 2020:2100, statesVariables]
  # account for unit "years since 1970-01-01 0:0:0"
  x <- setYears(x, getYears(x, as.integer = TRUE) - 1970)

  return(list(x = x,
              isocountries = FALSE,
              unit = "1",
              min = 0,
              max = 1.0001,
              cache = FALSE,
              description = "ESM compatible states information"))
}
