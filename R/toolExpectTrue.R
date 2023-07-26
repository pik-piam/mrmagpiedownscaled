toolExpectTrue <- function(check, description, level = 0) {
  status <- ifelse(isTRUE(check), "ok", "warn")
  toolStatusMessage(status, description, level = 1 + level)
}
