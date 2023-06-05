readMagpie <- function(subtype = "default") {
  "!# @monitor magpie4:::addGeometry"

  gdx <- "fulldata.gdx"
  # to produce these files run MAgPIE via ./scripts/start/forestry.R
  stopifnot(file.exists(gdx),
            length(Sys.glob("clustermap_*.rds")) == 1)

  if (subtype == "default") {
    clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
    landUse    <- magpie4::land(gdx, level = "cell")
    cropArea   <- magpie4::croparea(gdx, level = "cell", product_aggr = FALSE)

    x <- magclass::mbind(landUse, cropArea)
    x <- x[, , "crop", invert = TRUE] # remove crop to avoid double counting of areas
    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "data") # fix spatial set names

    return(list(x = x,
                class = "magpie",
                unit = "Mha",
                description = "Land cover information computed by MAgPIE"))
  } else if (subtype == "management") {
    # industrial roundwood fraction of wood harvest: rndwd
    # fuelwood fraction of wood harvest: fulwd
    timber <- magpie4::TimberProductionVolumetric(gdx, level = "cell", sumSource = TRUE, sumProduct = FALSE)
    timberShares <- timber / dimSums(timber, dim = 3)
    stopifnot(identical(getNames(timberShares), c("wood", "woodfuel")))
    getNames(timberShares) <- c("rndwd", "fulwd")
    stopifnot(max(timberShares, na.rm = TRUE) < 1.001)
    stopifnot(min(timberShares, na.rm = TRUE) >= 0)

    x <- timberShares

    return(list(x = x,
                class = "magpie",
                unit = "1", # TODO all shares?
                description = "Management information computed by MAgPIE"))
  } else {
    stop("Unknown subtype '", subtype, "' in readMagpie")
  }
}
