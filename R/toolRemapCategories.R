

toolRemapCategories <- function(x, input2ref, output2ref) {

  .projectData <- function(x, target) {
    if (inherits(x, "SpatVector")) {
      elementSize <- terra::expanse(x, unit = "ha")
      for (i in which(terra::datatype(x) == "double")) {
        x[[i]] <- x[[i]] / elementSize
      }
      out <- list()
      for (i in seq_len(dim(target)[1])) {
        out[[i]] <- terra::intersect(target[i, ], x)
      }
      out <- do.call(rbind, out)
      elementSize <- terra::expanse(out, unit = "ha")
      for (i in which(terra::datatype(out) == "double")) {
        out[[i]] <- out[[i]] * elementSize
      }
      out <- terra::aggregate(out, by = "clusterId", fun = "sum", count = FALSE)
    } else if (inherits(x, "SpatRaster")) {
      out  <- terra::extract(x, target, "sum", bind = TRUE, na.rm = TRUE)
    }
    return(out)
  }

  # prepare FAO reference data
  .getFaoSpatVector <- function() {
    fao <- readRDS(system.file("extdata/faoAreaHarvested.rds", package = "mrdownscale"))
    geometryCountries <- readRDS(system.file("extdata/geometryCountries.rds", package = "mrdownscale"))
    attr(fao, "geometry") <- geometryCountries[getItems(fao, dim = 1)]
    attr(fao, "crs") <- "+proj=longlat +datum=WGS84 +no_defs"
    fao <- as.SpatVector(fao)
    fao[[1]] <- NULL # remove country column
    return(fao)
  }
  .getLUH2SpatRaster <- function() {
    luh2 <- as.SpatRaster(read.magpie(system.file("extdata/luh2015.mz", package = "mrdownscale")))
    return(luh2)
  }
  fao      <- .getFaoSpatVector()
  luh2     <- .getLUH2SpatRaster()

  # project fao and luh2 data on x
  pfao  <- .projectData(fao, x[, 1:2])
  pluh2 <- .projectData(luh2, x[, 1:2])

  # convert to magclass
  mluh2 <- as.magpie(pluh2, spatial = which(terra::datatype(pluh2) != "double"))
  mfao  <- as.magpie(pfao, spatial = which(terra::datatype(pfao) != "double"))
  mx    <- as.magpie(x, spatial = which(terra::datatype(x) != "double"))
  # harmonize reference data to million ha and merge
  getItems(mfao, dim = 3) <- sub("^sum_", "", getItems(mfao, dim = 3))

  .bioenergDummy <- function(x) {
    # generate empty bioenergy dummy to
    # represent 2nd gen bioenergy production
    bioDummy <- x[,,1:2]
    bioDummy[,,] <- 0
    getItems(bioDummy, dim = 3) <- c("begr", "betr")
    return(bioDummy)
  }

  ref <- mbind(mluh2, mfao * 10^-6, .bioenergDummy(mfao))

  xRef <- toolAggregate(mx[, , "crop", invert = TRUE], input2ref, dim = 3, weight = ref + 10^-10)
  xOut <- toolAggregate(xRef, dim = 3, output2ref)
  attr(xOut, "crs") <- attr(mx, "crs")
  attr(xOut, "geometry") <- attr(mx, "geometry")
  return(xOut)
}
