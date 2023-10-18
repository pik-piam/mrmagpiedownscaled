#' toolTransitionsExtractGrossNetDiff
#'
#' tool function to extract from a transistions data set the difference between
#' gross and net transistions (the transistions which do not alter the
#' total land distribution).
#'
#' @param x magpie dataset containing land transistions steps
#' to extract net transitions from
#' @author Jan Philipp Dietrich

toolTransitionsExtractGrossNetDiff <- function(x) {

  diff <- x[, 2:dim(x)[2], ] - setItems(x[, 1:(dim(x)[2] - 1), ], getItems(x, dim = 2)[2:dim(x)[2]], dim = 2)
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
