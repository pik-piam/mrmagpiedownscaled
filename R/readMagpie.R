readMagpie <- function(subtype = "default") {
  "!# @monitor magpie4:::addGeometry"

  gdx <- "fulldata.gdx"
  # to produce these files run MAgPIE via ./scripts/start/forestry.R
  stopifnot(file.exists(gdx),
            length(Sys.glob("clustermap_*.rds")) == 1)

  clustermap <- readRDS(Sys.glob("clustermap_*.rds"))

  if (subtype == "default") {
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

  } else if (subtype == "woodHarvest") {
    # TODO check endogenous forest was active when creating fulldata.gdx
    x <- magpie4::TimberProductionVolumetric(gdx, level = "cell", sumSource = TRUE, sumProduct = FALSE)
    x <- magpie4::addGeometry(x, clustermap)

    return(list(x = x,
                unit = "mio. m3 per year",
                min = 0,
                description = "roundwood and fuelwood shares of total wood harvest computed by MAgPIE"))
  } else if (subtype == "irrigation") {
    x <- magpie4::croparea(gdx, level = "cell", product_aggr = FALSE, water_aggr = FALSE)

    return(list(x = x,
                class = "magpie",
                unit = "mio. ha",
                min = 0,
                description = "rainfed and irrigated area per crop computed by MAgPIE"))
  } else if (subtype == "management") {
    # TODO biofuel area fraction: crpbf_[c3ann,c3nfx,c3per,c4ann,c4per]
    # - dimSums begr & betr / dimSums c3per c4per
    # - 1st gen biofuel crops e.g. maize?
    # - only crpbf_[c3per,c4per]

    # TODO fertilization rate (in kg ha-1 yr-1 (crop season)): fertl_[c3ann,c3nfx,c3per,c4ann,c4per]
    # - TODO check if 0.5deg data is uniformly distributed
    # nitrogenBudget <- magpie4::NitrogenBudget(gdx,level="cell")
    # nitrogen <- magpie4::NitrogenBudgetWithdrawals(gdx,kcr="kcr",level="grid",net=TRUE)

  } else {
    stop("Unknown subtype '", subtype, "' in readMagpie")
  }
}
