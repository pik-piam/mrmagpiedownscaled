toolAddCheckReport <- function(x) {
  report <- attr(x, "toolCheck")
  toolCheckOption <- getOption("toolCheck")
  for (n in names(report)) {
    toolCheckOption[[n]] <- report[[n]]
  }
  options(toolCheck = toolCheckOption) # nolint: undesirable_function_linter
  return(x)
}
