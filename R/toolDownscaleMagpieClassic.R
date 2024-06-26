#' toolDownscaleMagpieClassic
#'
#' classic MAgPIE downscaling method using \code{luscale::interpolate2} as
#' downscaling function.
#'
#' @param x magpie dataset containing land to be downscaled
#' @param xTarget target land use dataset as SpatRaster for initialization year
#' @return downscaled land use dataset
#' @author Jan Philipp Dietrich
toolDownscaleMagpieClassic <- function(x, xTarget) {

  .getDownscaleMap <- function(x, xTarget) {
    tmp <- as.SpatVector(x[, 1, 1])[, ".id"]
    tmp[[1]]$.id <- as.integer(tmp[[1]]$.id)
    tmp <- terra::rasterize(tmp, xTarget, ".id", touches = FALSE)
    names(tmp) <- "id"
    tmp <- as.magpie(tmp)
    map <- data.frame(cell = getItems(tmp, dim = 1), cluster = as.vector(tmp))

    clusternames <- getItems(x, dim = 1)
    names(clusternames) <- getItems(x, dim = 1.1)
    map$cluster <- clusternames[map$cluster]

    return(map)
  }

  map <- .getDownscaleMap(x, xTarget)

  mTarget <- as.magpie(xTarget)

  rm(xTarget)
  gc()

  intersect <- intersect(getItems(mTarget, dim = 1), map$cell)
  missingInTarget <- (length(map$cell) - length(intersect)) / length(map$cell)
  if (missingInTarget > 0) {
    map <- map[map$cell %in% intersect, ]
    message <- paste0(round(missingInTarget * 100, 2),
                      "% of input cells are missing in target data and thus will be ignored!")
    toolStatusMessage("warn", message)
  }  else {
    toolStatusMessage("ok", "all input cells exist in target data")
  }

  missingInX <- setdiff(getItems(mTarget, dim = 1), map$cell)
  if (length(missingInX) > 0) {
    message <- paste0(round(length(missingInX) / length(map$cell) * 100, 2),
                      "% of target cells missing in input data. Those are matched to the nearest input cell.")
    toolStatusMessage("warn", message)

    resolution <- guessResolution(mTarget)
    stopifnot(!("NOTFOUND" %in% map$cluster))
    missingMapped <- vapply(missingInX, function(cell) {
      # search for the nearest cell in the clustermap in a spiral pattern
      # this is a crude, inefficient approach, may need to be optimized in the future
      xy <- strsplit(cell, ".", fixed = TRUE)[[1]]
      xy <- as.numeric(sub("p", ".", xy))
      maxDistance <- 10
      for (i in seq_len(maxDistance)) {
        xCoords <- seq(xy[1] - i * resolution, xy[1] + i * resolution, resolution)
        yCoords <- seq(xy[2] - i * resolution, xy[2] + i * resolution, resolution)
        coords <- c(paste(xCoords[-1], yCoords[1], sep = ":"),
                    paste(xCoords[length(xCoords)], yCoords[-1], sep = ":"),
                    paste(rev(xCoords[-length(xCoords)]), yCoords[length(yCoords)], sep = ":"),
                    paste(xCoords[1], rev(yCoords[-length(yCoords)]), sep = ":"))
        coords <- gsub("\\.", "p", coords)
        coords <- sub(":", ".", coords)

        # which cluster is most frequent in coords
        count <- table(map[map$cell %in% coords, ]$cluster)
        if (length(count) > 0) {
          return(paste0(names(which.max(count))))
        }
      }
      warning(cell, " is more than ", maxDistance,
              " cells away from cells in downscale map. It will be ignored.")
      return("NOTFOUND")
    }, character(1))
    mapExtension <- data.frame(cell = names(missingMapped), cluster = missingMapped)
    mapExtension <- mapExtension[mapExtension$cluster != "NOTFOUND", ]
    map <- rbind(map, mapExtension)
    intersect <- intersect(getItems(mTarget, dim = 1), map$cell)
    mTarget <- mTarget[intersect, , ]
    # TODO test
  } else {
    toolStatusMessage("ok", "all target cells exist in input data")
  }

  "!# @monitor luscale::interpolate2"

  # interpolate2 assumes constant total over time, but only warns if unfulfilled, convert that to error
  withr::with_options(c(warn = 2), {
    out <- luscale::interpolate2(x[, -1, ], mTarget[, 1, ], map)
  })
  getSets(out)[1:2] <- c("x", "y")

  return(out)
}
