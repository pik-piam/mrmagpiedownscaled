calcLandHighRes <- function(input = "magpie", target = "luh2") {
  x <- calcOutput("Harmonized", input = input, target = target, aggregate = FALSE)

  # get target data
  if (target == "luh2") {
    xTarget <- madrat::readSource("LUH2v2h")
  } else {
    stop("Unsupported output type \"", target, "\"")
  }

  .getDownscaleMap <- function(x, xTarget) {
    tmp <- as.SpatVector(x[, 1, 1])[, ".id"]
    tmp[[1]]$.id <- as.integer(tmp[[1]]$.id)
    tmp <- terra::rasterize(tmp, xTarget, ".id", touches = TRUE)
    names(tmp) <- "id"
    tmp <- as.magpie(tmp)
    map <- data.frame(cell = getItems(tmp, dim = 1), cluster = as.vector(tmp))
    return(map)
  }

  map <- .getDownscaleMap(x, xTarget)
  xlength(intersect(getItems(input, dim = 1), map$cell))

  return(return(list(x = NULL,
                     class = "magpie",
                     isocountries = FALSE,
                     unit = "Mha",
                     min = 0,
                     description = "Downscaled data")))
}
