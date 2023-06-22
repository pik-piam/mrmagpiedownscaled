toolCheckReport <- function(filter = FALSE) {
  fname <- as.character(sys.call(-1))[1]
  x <- getOption("toolCheck")
  if (isTRUE(filter) && !is.na(fname)) {
    deps <- suppressWarnings(getDependencies(sub("^.*:::", "", fname),
                                             packages = getConfig("packages"), self = TRUE)$call)
    x <- x[names(x) %in% deps]
  }
  return(x)
}
