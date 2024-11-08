#' toolPrimFix
#'
#' Replace primary land expansion with secondary land expansion.
#'
#' @param x a magpie object
#' @param prim name of primary land category, e.g. "primf"
#' @param secd name of secondary land category, e.g. "secdf"
#' @param ... not used, will throw an error if supplied
#' @param noteThreshold prim expansion greater than this will trigger a note
#' that primf/primn expansion was replaced by secdf/secdn expansion
#' @param warnThreshold prim expansion greater than this will trigger a warning
#' that primf/primn are expanding considerably
#' @return a magpie object with primf/primn expansion replaced by secdf/secdn expansion
#' @author Pascal Sauer
toolPrimFix <- function(x, prim, secd, ..., noteThreshold = 10^-10, warnThreshold = 10^-5) {
  stopifnot(nyears(x) >= 2,
            prim %in% getItems(x, dim = 3),
            secd %in% getItems(x, dim = 3),
            ...length() == 0,
            noteThreshold <= warnThreshold)
  maxDiff <- 0
  for (i in 2:nyears(x)) {
    primDiff <- x[, i, prim] - setYears(x[, i - 1, prim], getYears(x)[i])
    primDiff[primDiff < 0] <- 0
    maxDiff <- max(maxDiff, max(primDiff))
    # use pmin instead of subtracting to avoid tiny expansions due to numerical imprecision
    x[, i, prim] <- pmin(x[, i, prim], x[, i - 1, prim])
    x[, i, secd] <- x[, i, secd] + magclass::setNames(primDiff, secd)
  }

  if (maxDiff > warnThreshold) {
    toolStatusMessage("warn", paste0(prim, " is expanding considerably ",
                                     "(max expansion: ", maxDiff, ")"), level = 1)
  }

  if (maxDiff > noteThreshold) {
    toolStatusMessage("note", paste("replaced", prim, "expansion with", secd, "expansion"), level = 1)
  }

  return(x)
}
