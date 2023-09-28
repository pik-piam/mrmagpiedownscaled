#' readMagpie
#'
#' Read function for data coming from the MAgPIE model.
#'
#' @param subtype type of data to be read in. Available options are "land",
#' "crop", "woodHarvest" and "fertilizer"
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
                description = "Land cover information computed by MAgPIE"))
  } else if (subtype == "crop") {
    x <- magpie4::croparea(gdx, level = "cell", product_aggr = FALSE, water_aggr = FALSE)
    x <- magpie4::addGeometry(x, clustermap)
    attr(x, "gdxdata") <- NULL
    getSets(x) <- c("region", "id", "year", "data", "water")
    return(list(x = x,
                unit = "Mha",
                description = "Crop land information separated by irrigated/rainfed computed by MAgPIE"))
  } else if (subtype == "woodHarvest") {
    tryCatch({
      x <- magpie4::TimberProductionVolumetric(gdx, level = "cell", sumSource = TRUE, sumProduct = FALSE)
    }, message = function(msg) {
      if (grepl("Missing timber production", msg$message)) {
        stop("Cannot read wood harvest from fulldata.gdx. Try using fulldata.gdx ",
             "from a magpie run with endogenous forestry enabled (use scripts/start/forestry.R).")
      } else {
        message(message)
      }
    })
    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "data")

    return(list(x = x,
                unit = "mio. m3 per year",
                min = 0,
                description = "roundwood and fuelwood harvest in mio. m3 per year computed by MAgPIE"))
  } else if (subtype == "fertilizer") {
    x <- magpie4::NitrogenBudget(gdx, level = "cell", cropTypes = TRUE)
    x <- collapseDim(x[, , "fertilizer"])
    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "data")
    return(list(x = x,
                unit = "Tg yr-1",
                min = 0,
                description = "fertilization rate per croptype computed by MAgPIE"))
  } else {
    stop("Unknown subtype '", subtype, "' in readMagpie")
  }
}
