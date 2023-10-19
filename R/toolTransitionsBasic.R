#' toolTransitionsBasic
#'
#' tool function to extract net transitions between categories from a land
#' dataset with at least 2 time steps. The approach is rather simplistic by
#' assuming that expansion happens proportionally in all affected classes (equal
#' preference of transistions across all categories).
#'
#' @param x magpie dataset containing land data with at least two time steps
#' to extract net transitions from
#' @param gross a magpie object containing bidirectional transition shares relative
#' to the smaller land pool the transistions happen between (bidirectional means
#' here that a transition of the same size happens in both directions so that
#' the net transistion is zero)
#' @author Jan Philipp Dietrich

toolTransitionsBasic <- function(x, gross = NULL) {

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

  if (!is.null(gross)) {
    if (!is.magpie(gross)) stop('"gross" must be a MAgPIE object containing bidirectional transistion shares!')
    smallerArea <- toolGetSmallerArea(x)
    out <- out + gross * smallerArea
  }

  return(out)
}
