toolExpectTrue <- function(check, description) {
  if (isTRUE(check)) {
    toolStatusMessage("\u2713", description)
  } else {
    toolStatusMessage("-", description)
    warning("Check failed: ", description)
  }
}
