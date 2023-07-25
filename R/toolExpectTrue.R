toolExpectTrue <- function(check, description, level = 0) {
  if (isTRUE(check)) {
    toolStatusMessage("\u2713", description, level = 1 + level)
  } else {
    toolStatusMessage("!", description, level = 1 + level)
    warning("Check failed: ", description, level = 1 + level)
  }
}
