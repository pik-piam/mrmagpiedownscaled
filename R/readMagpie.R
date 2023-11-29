#' readMagpie
#'
#' Read function for data coming from the MAgPIE model.
#'
#' @param subtype type of data to be read in. Available options are land,
#' crop, woodHarvestWeight, woodHarvestArea and fertilizer
#' @author Pascal Sauer, Jan Philipp Dietrich
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
    getSets(x) <- c("region", "id", "year", "data")
    return(list(x = x,
                unit = "Mha",
                description = "land use information"))
  } else if (subtype == "crop") {
    x <- magpie4::croparea(gdx, level = "cell", product_aggr = FALSE, water_aggr = FALSE)
    x <- magpie4::addGeometry(x, clustermap)
    attr(x, "gdxdata") <- NULL
    getSets(x) <- c("region", "id", "year", "data", "water")
    return(list(x = x,
                unit = "Mha",
                description = "crop land information separated by irrigated/rainfed"))
  } else if (subtype == "woodHarvestWeight") {
    timberFromHeaven <- gdx::readGDX(gdx, "ov73_prod_heaven_timber", select = list(type = "level"))
    if (any(timberFromHeaven != 0)) {
      warning("Timber production from heaven (ov73_prod_heaven_timber) is not zero.",
              "Please check the MAgPIE run.")
    }
    forestry <- gdx::readGDX(gdx, "ov_prod_forestry", select = list(type = "level"))
    if (all(forestry == 0)) {
      warning("Timber production from forestry (ov_prod_forestry) is zero. ",
              "Are you using a fulldata.gdx from a magpie run with endogenous ",
              "forestry enabled (e.g. using scripts/start/forestry.R)?")
    }
    forestry <- add_dimension(forestry, dim = 3.1, add = "land_natveg", "forestry")
    natveg <- gdx::readGDX(gdx, "ov_prod_natveg", select = list(type = "level"))
    x <- mbind(forestry, natveg)
    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "source", "woodType")

    return(list(x = x,
                unit = "mio. t DM yr-1",
                min = 0,
                description = "roundwood and fuelwood harvest weight separated by source"))
  } else if (subtype == "woodHarvestArea") {
    forestry <- gdx::readGDX(gdx, "ov32_hvarea_forestry", select = list(type = "level"))
    if (all(forestry == 0)) {
      warning("Wood harvest area from forestry (ov32_hvarea_forestry) is zero. ",
              "Are you using a fulldata.gdx from a magpie run with endogenous ",
              "forestry enabled (e.g. using scripts/start/forestry.R)?")
    }
    forestry <- add_dimension(forestry, add = "source", nm = "forestry")

    primforest <- gdx::readGDX(gdx, "ov35_hvarea_primforest", select = list(type = "level"))
    primforest <- add_dimension(primforest, add = "ac", nm = "primary")
    primforest <- add_dimension(primforest, add = "source", nm = "primforest")

    secdforest <- gdx::readGDX(gdx, "ov35_hvarea_secdforest", select = list(type = "level"))
    secdforest <- add_dimension(secdforest, add = "source", nm = "secdforest")

    other <- gdx::readGDX(gdx, "ov35_hvarea_other", select = list(type = "level"))
    other <- add_dimension(other, add = "source", nm = "other")

    x <- mbind(forestry, primforest, secdforest, other)

    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "source", "ageClass")

    return(list(x = x,
                unit = "Mha",
                min = 0,
                description = "wood harvest area separated by source and age classes"))
  } else if (subtype == "fertilizer") {
    # suppressing the warning:
    # due to non-iteration of fertilizer distribution, residual fertilizer deficit is moved to balanceflow.
    suppressWarnings({
      x <- magpie4::NitrogenBudget(gdx, level = "cell", cropTypes = TRUE)
    })
    x <- collapseDim(x[, , "fertilizer"])
    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "cropType")
    return(list(x = x,
                unit = "Tg yr-1",
                min = 0,
                description = "fertilization rate per croptype"))
  } else {
    stop("Unknown subtype '", subtype, "' in readMagpie")
  }
}
