calcResolutionMapping <- function(input = "magpie", target = "luh2mod") {
  if (input == "magpie") {
    mapping <- readSource("MagpieFulldataGdx", subtype = "clustermap")
    coords <- strsplit(mapping$cell, "\\.")
    xCoords <- vapply(coords, function(x) as.double(sub("p", ".", x[1])), double(1))
    yCoords <- vapply(coords, function(x) as.double(sub("p", ".", x[2])), double(1))
    mapping <- cbind(x = xCoords, y = yCoords, mapping[, -which(colnames(mapping) == "cell")])
    colnames(mapping)[colnames(mapping) == "cluster"] <- "lowRes"
  } else {
    stop("Unsupported input type \"", input, "\"")
  }

  xTarget <- calcOutput("LandTarget", target = target, aggregate = FALSE)
  mapping <- toolResolutionMapping(mapping, xTarget)
  return(list(x = mapping,
              class = "data.frame",
              description = "mapping of high to low resolution and countrycode"))
}

# assuming target resolution is finer than what mapping already provides
# mapping must include columns x, y, lowRes
toolResolutionMapping <- function(mapping, xTarget) {
  stopifnot(c("x", "y", "lowRes") %in% colnames(mapping))
  mapping$cellId <- seq_len(nrow(mapping))
  pointsMapping <- terra::vect(mapping, geom = c("x", "y"), crs = terra::crs(xTarget))
  mappingRes <- guessResolution(mapping[, c("x", "y")])
  xTargetRes <- terra::res(xTarget)
  stopifnot(xTargetRes[1] == xTargetRes[2],
            xTargetRes[1] < mappingRes,
            mappingRes %% xTargetRes[1] == 0)
  xTargetAggregated <- terra::aggregate(xTarget, fact = mappingRes / xTargetRes)
  rasterMapping <- terra::rasterize(pointsMapping, xTargetAggregated, field = "cellId")
  names(rasterMapping) <- "cellId"
  polygonsMapping <- terra::as.polygons(rasterMapping)
  rasterMapping <- terra::rasterize(polygonsMapping, xTarget, field = "cellId")

  xyMapping <- terra::as.data.frame(rasterMapping, xy = TRUE)

  xyTarget <- terra::crds(xTarget, df = TRUE, na.all = TRUE)

  # TODO rename terrible variable names
  mapAllTarget <- merge(xyMapping, xyTarget, by = c("x", "y"), all.y = TRUE)

  missingInMapping <- mapAllTarget[is.na(mapAllTarget$cellId), c("x", "y")]
  # TODO report % input cells missing in target which are discarded
  if (nrow(missingInMapping) > 0) {
    toolStatusMessage("warn", paste0(round(nrow(missingInMapping) / nrow(xyTarget) * 100, 2),
                                     "% of target cells missing in input/mapping, ",
                                     "add those to mapping (nearest neighbor)"))

    near <- terra::nearest(terra::vect(missingInMapping, geom = c("x", "y"), crs = terra::crs(xTarget)),
                           pointsMapping)
    toolStatusMessage("warn", paste0("nearest neighbor distances: ",
                                     "max = ", round(max(near$distance) / 1000, 1), "km",
                                     ", 90% quantile = ", round(quantile(near$distance, probs = 0.90) / 1000, 1), "km",
                                     ", mean = ", round(mean(near$distance) / 1000, 1), "km"))

    near <- as.data.frame(near)
    colnames(near)[colnames(near) == "to_id"] <- "cellId"
    nn <- merge(near, mapping)
    nn$x <- nn$from_x
    nn$y <- nn$from_y
    nn <- nn[, colnames(mapping)]
    stopifnot(nrow(nn) == nrow(missingInMapping))

    ma <- mapping
    colnames(ma) <- sub("^x$", "x0p5", colnames(ma))
    colnames(ma) <- sub("^y$", "y0p5", colnames(ma))

    aa <- merge(mapAllTarget[!is.na(mapAllTarget$cellId), ], ma)
    aa <- aa[, colnames(mapping)]
    result <- rbind(aa, nn)
  } else {
    toolStatusMessage("ok", "input includes all target cells")
    result <- mapAllTarget
  }

  stopifnot(setequal(paste(result$x, result$y), paste(xyTarget$x, xyTarget$y)))
  return(result)
}
