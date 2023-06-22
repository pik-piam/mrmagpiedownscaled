
toolAddCheckReport <- function(x) {
  report <- attr(x, "toolCheck")
  envar <- getOption("toolCheck")
  for (n in names(report)) {
    envar[[n]] <- report[[n]]
  }
  options(toolCheck = envar)
  return(x)
}
