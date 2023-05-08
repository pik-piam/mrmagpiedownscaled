

toolRemapCategories <- function(x, input2fao, output2fao) {

  # prepare FAO reference data
  .getFaoSpatVector <- function() {
    fao <- readRDS(system.file("extdata/faoAreaHarvested2019.rds", package = "mrdownscale"))
    geometryCountries <- readRDS(system.file("extdata/geometryCountries.rds", package = "mrdownscale"))
    attr(fao, "geometry") <- geometryCountries
    return(as.SpatVector(fao))
  }
  fao  <- .getFaoSpatVector()
  luh2 <- readRDS(system.file("extdata/faoAreaHarvested2019.rds", package = "mrdownscale"))

  # project fao and luh2 data on x
  # ToDo write projectData function
  fao  <- projectData(fao, x)
  luh2 <- projectData(luh2, x)

  mluh2 <- as.magpie(luh2)
  mfao <- as.magpie(fao)
  ref <- mbind(mfao, mluh2)

  mx   <- as.magpie(x)

  xRef <- toolAggregate(mx, input2ref, weight = ref)
  xOut <- toolAggregate(xRef, output2ref)
  return(xOut)
}

projectData <- function(x, target) {
  # TODO: need to divide x values by polygon area!!
  # something like:
  # x2 <- x/terra::expanse(x, unit = "ha")
  out <- list()
  for(i in seq_len(dim(target)[1])) {
    out[[i]] <- terra::intersect(target[i,], x)
  }
  out <- do.call(rbind, out)
  # TODO: need to multiply x values by polygon area!!
  out <- terra::aggregate(out, by ="clusterId", fun = "sum")
  return(out)
}
