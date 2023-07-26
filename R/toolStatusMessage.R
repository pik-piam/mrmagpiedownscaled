toolStatusMessage <- function(status, message, level = 0) {
  symbol <- toolSubtypeSelect(status, c(ok = "\u2713", warn = "!"))
  message <- paste0("[", symbol, "] ", message)
  vcat(1, message, show_prefix = FALSE)
  putMadratMessage("status", message, fname = -2 - level, add = TRUE)
}
