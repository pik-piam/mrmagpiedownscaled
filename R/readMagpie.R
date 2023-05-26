readMagpie <- function() {
  "!# @monitor magpie4:::addGeometry"

  stopifnot(file.exists("fulldata.gdx"),
            length(Sys.glob("clustermap_*.rds")) == 1)

  clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
  landUse    <- magpie4::land("fulldata.gdx", level = "cell")
  cropArea   <- magpie4::croparea("fulldata.gdx", level = "cell", product_aggr = FALSE)

  x <- magclass::mbind(landUse, cropArea)
  x <- x[, , "crop", invert = TRUE] # remove crop to avoid double counting of areas
  x <- magpie4::addGeometry(x, clustermap)
  getSets(x) <- c("region", "id", "year", "data") # fix spatial set names

  return(list(x = x,
              class = "magpie",
              unit = "Mha",
              description = "Land cover information computed by MAgPIE"))
}
