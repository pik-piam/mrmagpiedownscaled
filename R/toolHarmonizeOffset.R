toolHarmonizeOffset <- function(input, target, harmonizeYear, finalYear) {
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
