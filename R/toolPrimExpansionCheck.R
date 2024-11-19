toolPrimExpansionCheck <- function(x) {
  if ("primn" %in% getItems(x, 3)) {
    toolExpectTrue(toolMaxExpansion(x, "primn") <= 0,
                   "primn is never expanding", falseStatus = "warn", level = 1)
  }
  toolExpectTrue(toolMaxExpansion(x, "primf") <= 0,
                 "primf is never expanding", falseStatus = "warn", level = 1)
}

toolMaxExpansion <- function(x, variable,
                             na.rm = FALSE) { # nolint: object_name_linter.
  return(max(x[, -1, variable] - setYears(x[, -nyears(x), variable],
                                          getYears(x[, -1, ])), na.rm = na.rm))
}

toolStopIfExpansion <- function(x, variable) {
  maxExpansion <- vapply(intersect(variable, getItems(x, 3)),
                         function(v) toolMaxExpansion(x, v),
                         numeric(1))
  maxExpansion <- maxExpansion[maxExpansion > 0]
  if (length(maxExpansion) >= 1) {
    stop(paste0("Expansion of ", names(maxExpansion),
                " detected, max expansion: ", signif(maxExpansion, 3),
                collapse = "\n  "))
  }
}
