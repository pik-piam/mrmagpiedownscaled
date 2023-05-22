calcDownscaled <- function(input = "magpie", target = "luh2") {
  input <- calcOutput("Harmonized", input = "magpie", target = "luh2", aggregate = FALSE)

  # get target data
  if (target == "luh2") {
    target <- madrat::readSource("LUH2v2h")
  } else {
    stop("Unsupported output type \"", target, "\"")
  }

  .getDownscaleMap <- function(x, target) {
    tmp <- as.SpatVector(x[,1,1])[,".id"]
    tmp[[1]]$.id <- as.integer(tmp[[1]]$.id)
    tmp <- terra::rasterize(tmp, target, ".id", touches = TRUE)
    names(tmp) <- "id"
    tmp <- as.magpie(tmp)
    map <-data.frame(cell=getItems(tmp, dim = 1), cluster=as.vector(tmp))
    return(map)
  }

  map <- .getDownscaleMap(input, target)
  length(intersect(getItems(mt, dim = 1), map$cell))

  return(return(list(x = out,
                     class = "magpie",
                     isocountries = FALSE,
                     unit = "Mha",
                     min = 0,
                     description = "Downscaled data")))
}
