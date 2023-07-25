toolStatusMessage <- function(symbol, message, level = 0) {
  putMadratMessage("status", paste0("[", symbol, "] ", message), fname = -2 - level, add = TRUE)
}
