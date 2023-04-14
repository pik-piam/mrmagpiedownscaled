calcLowResLUH2v2h <- function() {
  x <- madrat::readSource("LUH2v2h")
  x <- x[[grep("1166", names(x))]] # use only data from year 2015
  names(x) <- sub("_[0-9]+$", "", names(x))
  x <- x[[setdiff(names(x), c("secma", "secmb"))]]
  x$residual <- 1 - terra::app(x, sum)

  clustermap <- attr(madrat::readSource("Magpie"), "clustermap")
  clustermap <- clustermap[, c("cell", "cluster")]
  clustermap$cluster <- as.integer(sub("[A-Z]{3}\\.", "", clustermap$cluster))
  clustermap <- magclass::as.magpie(clustermap, filter = FALSE)
  clustermap <- magclass::as.SpatRaster(clustermap) # this adds coordinates
  clusterPolygons <- terra::as.polygons(clustermap)

  clusterData <- terra::extract(x, clusterPolygons, mean, na.rm = TRUE, weight = TRUE)
  names(clusterData)[1] <- "cluster"
  x <- terra::merge(clusterPolygons, clusterData)
  return(list(x = x,
              class = "SpatVector",
              cache = FALSE,
              unit = 1,
              description = "LUH2v2h data on cluster resolution"))
}
