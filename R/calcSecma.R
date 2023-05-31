calcSecma <- function() {
  x <- readSource("Magpie", "ageclasses")
  x <- x[, , "acx", invert = TRUE] # TODO how to handle acx (age class 160+)
  x <- add_dimension(x, add = "total")
  weight <- x
  ageClasses <- paste0("ac", seq(0, 155, 5))
  stopifnot(setequal(ageClasses, getItems(x, dim = 3.3)))
  for (ageClass in ageClasses) {
    age <- as.numeric(sub("ac", "", ageClass)) + 2.5
    x[, , ageClass] <- age
  }
  toolAggregate(x, weight = weight, to = "total", dim = 3) # TODO this does not work yet
}
