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
  mapping$cellId <- seq_len(nrow(mapping))
  pointsMapping <- terra::vect(mapping, geom = c("x", "y"), crs = terra::crs(xTarget))
  res0p5 <- terra::aggregate(xTarget) # TODO mapping is not always half resolution of xTarget
  rasterMapping <- terra::rasterize(pointsMapping, res0p5, field = "cellId")
  names(rasterMapping) <- "cellId"
  polygonsMapping <- terra::as.polygons(rasterMapping)
  rasterMapping <- terra::rasterize(polygonsMapping, xTarget, field = "cellId")

  xyMapping <- terra::as.data.frame(rasterMapping, xy = TRUE)

  xyTarget <- terra::crds(xTarget, df = TRUE, na.all = TRUE)

  # TODO rename terrible variable names
  a <- merge(xyMapping, xyTarget, by = c("x", "y"), all.y = TRUE)

  missingInMapping <- a[is.na(a$cellId), c("x", "y")]
  nMissing <- nrow(missingInMapping)
  # TODO report nMissing

  b <- terra::vect(missingInMapping, geom = c("x", "y"), crs = terra::crs(xTarget))

  near <- terra::nearest(b, pointsMapping)
  # TODO report max(near$distance)

  n <- as.data.frame(near)
  colnames(n)[colnames(n) == "to_id"] <- "cellId"
  nn <- merge(n, mapping)
  nn$x <- nn$from_x
  nn$y <- nn$from_y
  nn <- nn[, colnames(mapping)]
  stopifnot(nrow(nn) == nMissing)

  ma <- mapping
  colnames(ma) <- sub("^x$", "x0p5", colnames(ma))
  colnames(ma) <- sub("^y$", "y0p5", colnames(ma))

  aa <- merge(a[!is.na(a$cellId), ], ma)
  aa <- aa[, colnames(mapping)]
  result <- rbind(aa, nn)

  stopifnot(setequal(paste(result$x, result$y), paste(xyTarget$x, xyTarget$y)))
  return(result)
}
