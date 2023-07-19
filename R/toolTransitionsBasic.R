toolTransitionsBasic <- function(x) {

  diff <- x[, 2:dim(x)[2], ] - setYears(x[, 1:(dim(x)[2] - 1), ], getYears(x)[2:dim(x)[2]])
  reduce <- expand <- diff
  reduce[reduce > 0] <- 0
  reduce <- -reduce
  expand[expand < 0] <- 0


  split <- expand / dimSums(expand, dim = 3)
  rm(expand, x, diff)
  gc()
  split[is.na(split)] <- 0

  getSets(reduce, fulldim = FALSE)[3] <- "from"
  getSets(split, fulldim = FALSE)[3] <- "to"

  withr::with_options(list(magclass_setMatching = TRUE, magclass_sizeLimit = 10e+10), {
    out <- reduce * split
  })

  out <- out[, , paste0(getItems(reduce, dim = 3), ".", getItems(reduce, dim = 3)), invert = TRUE]

  return(out)
}
