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
              unit = NA,
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
  xyTarget$source <- "target"

  mapAllInput <- merge(xyMapping, xyTarget, by = c("x", "y"), all.x = TRUE)
  missingInTarget <- mapAllInput[is.na(mapAllInput$source), ]
  if (nrow(missingInTarget) > 0) {
    toolStatusMessage("warn", paste0(round(nrow(missingInTarget) / nrow(xyMapping) * 100, 2),
                                     "% of input cells missing in target, these are discarded"))
  } else {
    toolStatusMessage("ok", "target includes all input cells")
  }

  mapAllTarget <- merge(xyMapping, xyTarget, by = c("x", "y"), all.y = TRUE)
  missingInMapping <- mapAllTarget[is.na(mapAllTarget$cellId), c("x", "y")]
  if (nrow(missingInMapping) > 0) {
    toolStatusMessage("warn", paste0(round(nrow(missingInMapping) / nrow(xyTarget) * 100, 2),
                                     "% of target cells missing in mapping, ",
                                     "adding those to mapping (nearest neighbor)"))

    near <- terra::nearest(terra::vect(missingInMapping, geom = c("x", "y"), crs = terra::crs(xTarget)),
                           pointsMapping)
    toolStatusMessage("warn", paste0("nearest neighbor distances: ",
                                     "max = ", round(max(near$distance) / 1000, 1), "km",
                                     ", 90% quantile = ",
                                     round(quantile(near$distance, probs = 0.90) / 1000, 1), "km",
                                     ", mean = ", round(mean(near$distance) / 1000, 1), "km"))

    near <- as.data.frame(near)
    colnames(near)[colnames(near) == "to_id"] <- "cellId"

    mappingAddition <- merge(near, mapping)
    mappingAddition$x <- mappingAddition$from_x
    mappingAddition$y <- mappingAddition$from_y
    mappingColumns <- colnames(mapping)
    mappingAddition <- mappingAddition[, mappingColumns]
    stopifnot(nrow(mappingAddition) == nrow(missingInMapping))

    colnames(mapping) <- sub("^x$", "xOriginal", colnames(mapping))
    colnames(mapping) <- sub("^y$", "yOriginal", colnames(mapping))

    mappingBase <- merge(mapAllTarget[!is.na(mapAllTarget$cellId), ], mapping)
    mappingBase <- mappingBase[, mappingColumns]
    result <- rbind(mappingBase, mappingAddition)
  } else {
    toolStatusMessage("ok", "input includes all target cells")

    # TODO check this

    mappingBase <- merge(mapAllTarget[!is.na(mapAllTarget$cellId), ], mapping)
    result <- mappingBase[, mappingColumns]
  }

  stopifnot(setequal(paste(result$x, result$y), paste(xyTarget$x, xyTarget$y)))
  return(result)
}
