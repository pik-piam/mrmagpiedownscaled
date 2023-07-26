#' toolHarmonizeOffset
#'
#' Harmonize datasets using the offset method as implemented in mip::harmonize,
#' originally implemented in Python as part of aneris.
#'
#' @param input magpie object to fade to, usually model projections
#' @param target magpie object to fade from, usually historical data, this argument is called "target"
#' for consistency with argument names of other functions, it is somewhat misleading here,
#' because data after the finalYear will not be taken from target, but from input
#' @param harmonizeYear year to start fading, data at and before this year will be taken from target
#' @param finalYear year to stop fading, data at and after this year will be taken from input
#' @return magpie object with harmonized data
#' @author Pascal Führlich
toolHarmonizeOffset <- function(input, target, harmonizeYear, finalYear) {
  "!# @monitor mip::harmonize"

  .df <- function(input) {
    df <- magclass::as.data.frame(input, rev = 3)
    stopifnot(identical(names(df), c("region", "id", "year", "data", ".value")))
    df <- df[, -1]
    names(df) <- c("region", "period", "variable", "value")
    df$region <- as.character(df$region)
    df$scenario <- "scenario"
    df$model <- "model"
    df$unit <- "Mha"
    return(df)
  }
  dInput  <- .df(input)
  dTarget <- .df(target)

  out <- mip::harmonize(dInput, dTarget,
                        harmonizeYear = as.character(harmonizeYear), finalYear = as.character(finalYear),
                        method = "offset")

  out <- as.magpie(out, spatial = "region", temporal = "period")
  getSets(out)[1] <- "id"
  spatElems <- getItems(input, dim = 1.1, full = TRUE)
  names(spatElems) <- getItems(input, dim = "id", full = TRUE)
  getItems(out, dim = "region", maindim = 1) <- unname(spatElems[getItems(out, dim = 1)])
  out <- collapseDim(magpiesort(dimOrder(out, 2:1, dim = 1)))
  getSets(out) <- c("region", "id", "year", "data")
  return(out)
}
