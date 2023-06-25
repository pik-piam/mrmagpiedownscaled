toolStatusMessage <- function(symbol, message) {
  options(toolStatusMessages = c(getOption("toolStatusMessages"), # nolint: undesirable_function_linter
                                 paste0("[", symbol, "] ", message)))
}
