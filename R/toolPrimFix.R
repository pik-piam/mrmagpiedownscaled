#' toolPrimFix
#'
#' Replace primf and primn expansion with secdf and secdn expansion.
#'
#' @param x a magpie object with categories c("primf", "primn", "secdf", "secdn")
#' @param noteThreshold prim expansion greater than this will trigger a note
#' that primf/primn expansion was replaced by secdf/secdn expansion
#' @param warnThreshold prim expansion greater than this will trigger a warning
#' that primf/primn are expanding considerably
#' @return a magpie object with primf/primn expansion replaced by secdf/secdn expansion
#' @author Pascal Sauer
toolPrimFix <- function(x, noteThreshold = 10^-10, warnThreshold = 10^-5) {
  stopifnot(nyears(x) >= 2,
            setequal(getItems(x, dim = 3), c("primf", "primn", "secdf", "secdn")),
            noteThreshold <= warnThreshold)
  maxDiff <- 0
  for (i in 2:nyears(x)) {
    primDiff <- x[, i, c("primf", "primn")] - setYears(x[, i - 1, c("primf", "primn")], getYears(x)[i])
    primDiff[primDiff < 0] <- 0
    maxDiff <- max(maxDiff, max(primDiff))
    # use pmin instead of subtracting to avoid tiny expansions due to numerical precision
    x[, i, c("primf", "primn")] <- pmin(x[, i, c("primf", "primn")], x[, i - 1, c("primf", "primn")])
    x[, i, c("secdf", "secdn")] <- x[, i, c("secdf", "secdn")] + setNames(primDiff, c("secdf", "secdn"))
  }

  if (maxDiff > warnThreshold) {
    toolStatusMessage("warn", paste0("primf and/or primn are expanding considerably",
                                     "(max expansion: ", maxDiff, ")"), level = 1)
  }

  if (maxDiff > noteThreshold) {
    toolStatusMessage("note", "replaced primf/primn expansion with secdf/secdn expansion", level = 1)
  }

  return(x)
}
