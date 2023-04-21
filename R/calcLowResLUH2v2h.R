calcLowResLUH2v2h <- function() {
  x <- madrat::readSource("LUH2v2h", supplementary = TRUE)
  unit <- x$unit
  x <- x$x

  # create cluster polygons
  clustermap <- attr(madrat::readSource("Magpie"), "clustermap")
  clustermap <- clustermap[, c("cell", "cluster")]
  clustermap$cluster <- as.integer(sub("[A-Z]{3}\\.", "", clustermap$cluster))
  clustermap <- magclass::as.magpie(clustermap, filter = FALSE)
  clustermap <- magclass::as.SpatRaster(clustermap) # this adds coordinates
  clusterPolygons <- terra::as.polygons(clustermap)

  # calc category shares for each cluster
  clusterData <- terra::extract(x, clusterPolygons, sum, na.rm = TRUE)
  names(clusterData)[1] <- "cluster"
  x <- terra::merge(clusterPolygons, clusterData)

  return(list(x = x,
              class = "SpatVector",
              unit = unit,
              description = "LUH2v2h data on cluster resolution"))
}
