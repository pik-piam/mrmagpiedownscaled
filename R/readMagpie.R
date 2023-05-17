readMagpie <- function() {
  "!# @monitor magpie4::clusterOutputToTerraVector"
  stopifnot(file.exists("fulldata.gdx"),
            length(Sys.glob("clustermap_*.rds")) == 1)

  clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
  landUse    <- magpie4::land("fulldata.gdx", level = "cell")
  cropArea   <- magpie4::croparea("fulldata.gdx", level = "cell", product_aggr = FALSE)

  x <- magclass::mbind(landUse, cropArea)
  x <- magpie4::clusterOutputToTerraVector(x, clustermap)
  stopifnot(names(x)[2] == "j")
  x <- x[, -2]

  return(list(x = x, class = "SpatVector"))
}
