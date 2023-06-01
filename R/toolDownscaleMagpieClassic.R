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

  toolCheck("Land High Res input", {
    mTarget <- as.magpie(xTarget)
    intersect <- intersect(getItems(mTarget, dim = 1), map$cell)
    missingInTarget <- (length(map$cell) - length(intersect)) / length(map$cell)
    if (missingInTarget > 0) {
      map <- map[map$cell %in% intersect, ]
      message <- paste0(round(missingInTarget * 100, 2),
                        "% of cells missing in target data and thus removed from input data!")
      toolStatusMessage("!", message)
    }  else {
      toolStatusMessage("\u2713", "input data area is fully covered by target data")
    }

    missingInX <- (dim(mTarget)[1] - length(intersect)) / dim(mTarget)[1]
    if (missingInX > 0) {
      mTarget <- mTarget[intersect, , ]
      message <- paste0(round(missingInX * 100, 2),
                        "% of cells missing in input data and thus removed from target data!")
      toolStatusMessage("!", message)
    } else {
      toolStatusMessage("\u2713", "target data area is fully covered by input data")
    }
  })

  "!# @monitor luscale::interpolate2"

  out <- luscale::interpolate2(x[, -1, ], mTarget[, 1, ], map)
  getSets(out)[1:2] <- c("x", "y")

  return(out)
}
