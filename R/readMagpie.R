readMagpie <- function(subtype = "land") {
  "!# @monitor magpie4:::addGeometry"

  gdx <- "fulldata.gdx"
  # to produce these files run MAgPIE via ./scripts/start/forestry.R
  stopifnot(file.exists(gdx),
            length(Sys.glob("clustermap_*.rds")) == 1)

  clustermap <- readRDS(Sys.glob("clustermap_*.rds"))

  if (subtype == "land") {
    x <- magpie4::land(gdx, level = "cell")
    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "data") # fix spatial set names
    return(list(x = x,
                unit = "Mha",
                description = "Land cover information computed by MAgPIE"))
  } else if (subtype == "crop") {
    x <- magpie4::croparea(gdx, level = "cell", product_aggr = FALSE)
    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "data") # fix spatial set names
    return(list(x = x,
                unit = "Mha",
                description = "Crop land information computed by MAgPIE"))
  } else if (subtype == "woodHarvest") {
    # TODO check endogenous forest was active when creating fulldata.gdx
    x <- magpie4::TimberProductionVolumetric(gdx, level = "cell", sumSource = TRUE, sumProduct = FALSE)
    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "data") # fix spatial set names

    return(list(x = x,
                unit = "mio. m3 per year",
                min = 0,
                description = "roundwood and fuelwood harvest in mio. m3 per year computed by MAgPIE"))
  } else if (subtype == "irrigation") {
    x <- magpie4::croparea(gdx, level = "cell", product_aggr = FALSE, water_aggr = FALSE)

    return(list(x = x,
                unit = "Mha",
                min = 0,
                description = "rainfed and irrigated area per crop computed by MAgPIE"))
  } else if (subtype == "fertilizer") {
    x <- magpie4::NitrogenBudget(gdx, level = "cell", cropTypes = TRUE)
    x <- collapseDim(x[, , "fertilizer"])
    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "data") # fix spatial set names
    return(list(x = x,
                unit = "Tg Nr yr-1",
                min = 0,
                description = "fertilization rate per croptype computed by MAgPIE"))
  } else {
    stop("Unknown subtype '", subtype, "' in readMagpie")
  }
}
