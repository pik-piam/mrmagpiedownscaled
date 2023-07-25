toolStatusMessage <- function(symbol, message, level = 0) {
  message <- paste0("[", symbol, "] ", message)
  vcat(1, message, show_prefix = FALSE)
  putMadratMessage("status", message, fname = -2 - level, add = TRUE)
}
