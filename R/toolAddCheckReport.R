toolAddCheckReport <- function(x) {
  report <- attr(x, "toolCheck")
  toolCheckOption <- getOption("toolCheck")
  for (n in names(report)) {
    toolCheckOption[[n]] <- report[[n]]
  }
  options(toolCheck = toolCheckOption)
  return(x)
}
