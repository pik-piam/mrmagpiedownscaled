readMagpie <- function() {
  landUse <- magpie4::land("fulldata.gdx", level = "cell")
  cropArea <- magpie4::croparea("fulldata.gdx", level = "cell", product_aggr = FALSE)
  x <- magclass::mbind(landUse, cropArea)
  attr(x, "clustermap") <- readRDS("clustermap.rds")
  return(x)
}
