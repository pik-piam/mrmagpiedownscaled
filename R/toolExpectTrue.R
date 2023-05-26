toolExpectTrue <- function(check, description) {
  if (isTRUE(check)) {
    vcat(1, "[\u2713] ", description, show_prefix = FALSE)
  } else {
    vcat(1, "[-] ", description, show_prefix = FALSE)
    warning("Check failed: ", description)
  }
}
