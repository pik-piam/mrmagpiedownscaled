readMagpie <- function(subtype = "default") {
  "!# @monitor magpie4:::addGeometry"

  if (subtype == "default") {
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
  } else if (subtype == "ageclasses") {
    x <- magpie4::land("fulldata.gdx", level = "cell", subcategories = c("other", "secdforest"))
    x <- x[, , "total", invert = TRUE]
    return(list(x = x,
                class = "magpie",
                unit = "Mha",
                description = "Age classes land cover information computed by MAgPIE"))
  } else {
    stop("subtype ", subtype, " not allowed in readMagpie")
  }
}
