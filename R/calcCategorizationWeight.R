

calcCategorizationWeight <- function(map, geometry, crs) {

  .getTarget <- function(geometry, crs) {
    target <- new.magpie(names(geometry), sets = c("id", "temporal", "data"))
    attr(target, "geometry") <- geometry
    attr(target, "crs")      <- crs
    return(as.SpatVector(target)[, 1])
  }

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
      out <- terra::aggregate(out, by = ".id", fun = "sum", count = FALSE)
      names(out) <- sub("^sum\\_", "", names(out))
    } else if (inherits(x, "SpatRaster")) {
      out  <- terra::extract(x, target, "sum", bind = TRUE, na.rm = TRUE)
    }
    return(out)
  }

  .remap <- function(x, map) {
    # reduce categories to minimum based on supplied mappings
    map <- map[map$reference %in% getItems(x, dim = 3), ]
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
  target <- .getTarget(geometry, crs)
  pfao  <- .projectData(fao, target)
  pluh2 <- .projectData(luh2, target)

  # convert to magclass
  mluh2 <- as.magpie(pluh2, spatial = which(terra::datatype(pluh2) != "double"))
  mfao  <- as.magpie(pfao, spatial = which(terra::datatype(pfao) != "double"))

  .dummy <- function(x, map, availableItems) {
    # generate empty dummy for missing
    # categories
    missing <- setdiff(map$merge, availableItems)
    message("Adding dummy weights for following categories: ", paste(missing, collapse = ", "))
    dummy <- x[, , rep(1, length(missing))]
    dummy[, , ] <- 0
    getItems(dummy, dim = 3) <- missing
    return(dummy)
  }

  out <- mbind(mluh2, mfao, .dummy(mfao, map, c(getItems(mluh2, dim = 3), getItems(mfao, dim = 3)))) + 10^-10
  attr(out, "crs") <- crs
  attr(out, "geometry") <- geometry

  # tests
  tryCatch(testthat::test_that("data fullfills format requirement", {
    testthat::expect_identical(unname(getSets(out)[1]), "id")
    testthat::expect_true(all(out >= 10^-10))

    # check for expected land categories
    testthat::expect_setequal(getItems(out, dim = 3), map$merge)

    # check for constant total areas
    outSum <- dimSums(out, dim = 3)
    testthat::expect_lt(max(abs(outSum - outSum[, 1, ])), 10^-5)
  }), error = function(e) warning(e))

  return(list(x = out,
              isocountries = FALSE,
              unit = "ha",
              min = 10^-10,
              description = "Weights for dissagregation inputs to reference categories"))

}
