

toolRemapCategories <- function(x, input2fao, output2fao) {

  # prepare FAO reference data
  .getFaoSpatVector <- function() {
    fao <- readRDS(system.file("extdata/faoAreaHarvested2019.rds", package = "mrdownscale"))
    geometryCountries <- readRDS(system.file("extdata/geometryCountries.rds", package = "mrdownscale"))
    attr(fao, "geometry") <- geometryCountries[getItems(fao, dim = 1)]
    attr(fao, "crs") <- "+proj=longlat +datum=WGS84 +no_defs"
    return(as.SpatVector(fao))
  }
  fao      <- .getFaoSpatVector()
  fao[[1]] <- NULL # remove country column
  luh2 <- as.SpatRaster(read.magpie(system.file("extdata/luh2015.mz", package = "mrdownscale")))

  # project fao and luh2 data on x
  # ToDo write projectData function
  pfao  <- projectData(fao, x[,1:2])
  pluh2 <- projectData(luh2, x[,1:2])

  mluh2 <- as.magpie(pluh2,  spatial = which(terra::datatype(mluh2) != "double"))
  mfao <- as.magpie(pfao, spatial = which(terra::datatype(pfao) != "double"))
  ref <- mbind(mfao, mluh2)

  mx   <- as.magpie(x)

  xRef <- toolAggregate(mx, input2ref, weight = ref)
  xOut <- toolAggregate(xRef, output2ref)
  return(xOut)
}

projectData <- function(x, target) {
  if(inherits(x, "SpatVector")) {
      elementSize <- terra::expanse(x, unit = "ha")
      for(i in which(terra::datatype(x) == "double")) {
        x[[i]] <- x[[i]]/elementSize
      }
      out <- list()
      for(i in seq_len(dim(target)[1])) {
        out[[i]] <- terra::intersect(target[i,], x)
      }
      out <- do.call(rbind, out)
      elementSize <- terra::expanse(out, unit = "ha")
      for(i in which(terra::datatype(out) == "double")) {
        out[[i]] <- out[[i]] * elementSize
      }
      out <- terra::aggregate(out, by ="clusterId", fun = "sum", count = FALSE)
  } else if(inherits(x, "SpatRaster")) {
    out  <- terra::extract(x, target, "sum", bind = TRUE, na.rm=TRUE)
  }
  return(out)
}
