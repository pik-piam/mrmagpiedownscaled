toolPrimExpansionCheck <- function(x) {
  if ("primn" %in% getItems(x, 3)) {
    toolExpectTrue(all(x[, -1, "primn"] <= setYears(x[, -nyears(x), "primn"],
                                                    getYears(x[, -1, ]))),
                   "primn is never expanding", falseStatus = "warn", level = 1)
  }
  toolExpectTrue(all(x[, -1, "primf"] <= setYears(x[, -nyears(x), "primf"],
                                                  getYears(x[, -1, ]))),
                 "primf is never expanding", falseStatus = "warn", level = 1)
}
