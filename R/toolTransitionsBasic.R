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
#' @export

toolTransitionsBasic <- function(x, gross = NULL) {

  x <- x[, , grep("(_|manaf)", getItems(x, dim = 3), invert = TRUE, value = TRUE)]
  diff <- x[, 2:dim(x)[2], ] - setItems(x[, 1:(dim(x)[2] - 1), ], getItems(x, dim = 2)[2:dim(x)[2]], dim = 2)
  reduce <- expand <- diff
  reduce[reduce > 0] <- 0
  reduce <- -reduce
  expand[expand < 0] <- 0

  split <- expand / dimSums(expand, dim = 3)
  rm(expand, diff)
  gc()
  split[is.na(split)] <- 0

  getSets(reduce, fulldim = FALSE)[3] <- "from"
  getSets(split, fulldim = FALSE)[3] <- "to"

  withr::with_options(list(magclass_setMatching = TRUE, magclass_sizeLimit = 10e+10), {
    out <- reduce * split
  })

  # remove entries for transitions to itself (1) as well as transitions to primn
  # or primf (2) as they are not allowed
  remove <- union(paste0(getItems(reduce, dim = 3), ".", getItems(reduce, dim = 3)),
                  grep("\\.(primn|primf)$", getItems(out, dim = 3), value = TRUE))

  out <- out[, , remove, invert = TRUE]

  if (!is.null(gross)) {
    if (!is.magpie(gross)) stop('"gross" must be a MAgPIE object containing bidirectional transistion shares!')
    # boost gross data set to all transistions
    invertSet <- sub("^(.*)\\.(.*)$", "\\2.\\1", getItems(gross, dim = 3))
    if (any(invertSet %in% getItems(gross, dim = 3))) {
      stop("The gross transistion data set should only contain a single value for any bidirectional connection!")
    }
    gross <- mbind(gross, setItems(gross, invertSet, dim = 3, raw = TRUE))
    smallerArea <- toolGetSmallerArea(x)[, 2:dim(x)[2], getItems(out, dim = 3)]
    missing <- setdiff(getItems(smallerArea, dim = 3), getItems(gross, dim = 3))
    dummy <- gross[, , 1]
    dummy[, , ] <- 0
    dummy <- setItems(dummy[, , rep(1, length(missing))], missing, dim = 3, raw = TRUE)
    gross <- mbind(gross, dummy)
    gross <- gross[getItems(smallerArea, dim = 1), , ] * smallerArea
    # ToDo: 1. annual transistions (net + gross, but also gross standalone)
    #          can currently be bigger than the area available for transformation
    #       2. bidirectional transistions needs to be multiplied by number of years
    #          (after that the transitions between two time steps could be bigger
    #          than the available area!)
    out <- out + gross[getItems(smallerArea, dim = 1), , ] * smallerArea
  }

  return(out)
}
