
toolCheckReport <- function(filter = FALSE, unlist = FALSE) {
  fname <- as.character(sys.call(-1))[1]
  x <- getOption("toolCheck")
  if (isTRUE(filter) && !is.na(fname)) {
    deps <- suppressWarnings(getDependencies(sub("^.*:::", "", fname), packages = getConfig("packages"), self = TRUE)$call)
    x <- x[names(x) %in% deps]
  }
  if (isTRUE(unlist)) {
    x <- unlist(x, use.names = FALSE)
  }
  return(x)
}
