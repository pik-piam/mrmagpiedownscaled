

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
      names(out) <- sub("^sum\\_", "", names(out))
    } else if (inherits(x, "SpatRaster")) {
      out  <- terra::extract(x, target, "sum", bind = TRUE, na.rm = TRUE)
    }
    return(out)
  }

  .getMap <- function(input2ref, output2ref) {
    map <- merge(input2ref, output2ref, by = "reference", suffixes = c("Input", "Output"))
    map$merge <- paste(map$dataInput,map$dataOutput, sep = "_")
    return(map)
  }
  map <- .getMap(input2ref, output2ref)

  .remap <- function(x, map) {
    # reduce categories to minimum based on supplied mappings
    map <- map[map$reference %in% getItems(x, dim = 3),]
    x <- toolAggregate(x, map, from = "reference", to = "merge", dim = 3)
    return(x)
  }

  # prepare FAO reference data
  .getFaoSpatVector <- function(map) {
    fao <- readRDS(system.file("extdata/faoAreaHarvested.rds", package = "mrdownscale"))

    # reduce categories to minimum based on supplied mappings
    fao <- .remap(fao, map)
    fao <- fao * 10^-6 # mio. ha -> ha

    geometryCountries <- readRDS(system.file("extdata/geometryCountries.rds", package = "mrdownscale"))
    attr(fao, "geometry") <- geometryCountries[getItems(fao, dim = 1)]
    attr(fao, "crs") <- "+proj=longlat +datum=WGS84 +no_defs"
    fao <- as.SpatVector(fao)
    fao[[1]] <- NULL # remove country column
    return(fao)
  }
  .getLUH2SpatRaster <- function(map) {
    luh2 <- read.magpie(system.file("extdata/luh2015.mz", package = "mrdownscale"))

    # reduce categories to minimum based on supplied mappings
    luh2 <- .remap(luh2, map)

    luh2 <- as.SpatRaster(luh2)
    return(luh2)
  }
  fao      <- .getFaoSpatVector(map)
  luh2     <- .getLUH2SpatRaster(map)

  # project fao and luh2 data on x
  pfao  <- .projectData(fao, x[, 1:2])
  pluh2 <- .projectData(luh2, x[, 1:2])

  # convert to magclass
  mluh2 <- as.magpie(pluh2, spatial = which(terra::datatype(pluh2) != "double"))
  mfao  <- as.magpie(pfao, spatial = which(terra::datatype(pfao) != "double"))
  mx    <- as.magpie(x, spatial = which(terra::datatype(x) != "double"))

  .bioenergDummy <- function(x, map) {
    # generate empty bioenergy dummy to
    # represent 2nd gen bioenergy production
    bioDummy <- x[,,rep(1,2)]
    bioDummy[,,] <- 0
    getItems(bioDummy, dim = 3) <- c("begr", "betr")
    bioDummy <- .remap(bioDummy, map)
    return(bioDummy)
  }

  ref <- mbind(mluh2, mfao, .bioenergDummy(mfao, map))

  xRef <- toolAggregate(mx[, , "crop", invert = TRUE], map, dim = 3, from = "dataInput", to = "merge",
                        weight = ref + 10^-10)
  xOut <- toolAggregate(xRef, dim = 3, map, from = "merge", to = "dataOutput")
  attr(xOut, "crs") <- attr(mx, "crs")
  attr(xOut, "geometry") <- attr(mx, "geometry")
  return(xOut)
}
