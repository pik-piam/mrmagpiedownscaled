calcLandCategorizationWeight <- function(map, geometry, crs) {
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

  .getToolboxSpatRaster <- function(map) {
    x <- read.magpie(system.file("extdata/toolbox2000.mz", package = "mrdownscale"))
    # reduce categories to minimum based on supplied mappings
    x <- .remap(x, map)
    x <- as.SpatRaster(x)
    return(x)
  }

  .getLUH2SpatRaster <- function(map) {
    luh2 <- read.magpie(system.file("extdata/luh2015.mz", package = "mrdownscale"))
    # reduce categories to minimum based on supplied mappings
    luh2 <- .remap(luh2, map)
    luh2 <- as.SpatRaster(luh2)
    return(luh2)
  }

  toolbox <- .getToolboxSpatRaster(map)
  luh2 <- .getLUH2SpatRaster(map)

  # project toolbox and luh2 data on x
  target <- .getTarget(geometry, crs)
  ptoolbox <- .projectData(toolbox, target)
  pluh2 <- .projectData(luh2, target)

  # convert to magclass
  mluh2 <- as.magpie(pluh2, spatial = which(terra::datatype(pluh2) != "double"))
  mtoolbox  <- as.magpie(ptoolbox, spatial = which(terra::datatype(ptoolbox) != "double"))

  .dummy <- function(x, map, availableItems) {
    # generate empty dummy for missing categories
    missing <- setdiff(map$merge, availableItems)
    dummy <- x[, , rep(1, length(missing))]
    dummy[, , ] <- 0
    getItems(dummy, dim = 3) <- missing
    return(dummy)
  }

  out <- mbind(mluh2, mtoolbox, .dummy(mtoolbox, map, c(getItems(mluh2, dim = 3),
                                                        getItems(mtoolbox, dim = 3)))) + 10^-10
  attr(out, "crs") <- crs
  attr(out, "geometry") <- geometry

  # check data for consistency
  toolExpectTrue(identical(unname(getSets(out)[1]), "id"), "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), map$merge), "Land categories match merged categories")
  toolExpectTrue(all(out >= 10^-10), "All values are >= 10^-10")
  .dummyCols <- function(x) {
    dummy <- magpply(x, function(x) return(all(x == 10^-10)), 3)
    dummy <- getItems(dummy, dim = 3)[dummy]
    if (length(dummy) == 0) {
      toolStatusMessage("\u2713", "No dummy weights detected")
    } else {
      toolStatusMessage("!", paste("Following categories contain 10^-10 as dummy weight:",
                                   paste(dummy, collapse = ", ")))
    }
  }
  .dummyCols(out)

  return(list(x = out,
              isocountries = FALSE,
              unit = "ha",
              min = 10^-10,
              description = "Weights for dissagregation inputs to reference categories"))
}
