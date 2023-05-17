
readMagpie <- function() {

  "!# @monitor magpie4:::addGeometry"

  stopifnot(file.exists("fulldata.gdx"),
            length(Sys.glob("clustermap_*.rds")) == 1)

  clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
  landUse    <- magpie4::land("fulldata.gdx", level = "cell")
  cropArea   <- magpie4::croparea("fulldata.gdx", level = "cell", product_aggr = FALSE)

  x <- magclass::mbind(landUse, cropArea)
  x <- magpie4::addGeometry(x, clustermap)
  x <- as.SpatVector(x)
  stopifnot(identical(names(x)[1:2], c(".j", ".region")))
  x <- x[,c(2,1,seq_len(dim(x)[2])[-1:-2])]
  names(x)[1] <- "clusterId"
  return(list(x = x, class = "SpatVector"))
}
