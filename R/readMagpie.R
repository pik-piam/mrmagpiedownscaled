readMagpie <- function() {
  stopifnot(file.exists("fulldata.gdx"),
            length(Sys.glob("clustermap_*.rds")) == 1)

  clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
  landUse    <- magpie4::land("fulldata.gdx", level = "cell")
  cropArea   <- magpie4::croparea("fulldata.gdx", level = "cell", product_aggr = FALSE)

  x <- magclass::mbind(landUse, cropArea)

  # TODO get geom without using magpie4::clusterOutputToTerraVector
  longFormat <- magpie4::clusterOutputToTerraVector(x, clustermap)
  longFormat <- longFormat[!duplicated(longFormat$clusterId), ]
  attr(x, "geometry") <- terra::geom(longFormat, wkt = TRUE)

  x <- as.SpatVector(x)
  stopifnot(identical(names(x)[1:2], c(".j", ".region")))
  x <- x[, -1]
  names(x)[1] <- "clusterId"
  terra::crs(x) <- terra::crs(longFormat)
  return(list(x = x, class = "SpatVector"))
}
