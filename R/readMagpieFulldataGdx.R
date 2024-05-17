#' readMagpieFulldataGdx
#'
#' Read function for data coming from the MAgPIE model.
#'
#' @param subtype type of data to be read in. Available options are
#' land, crop, woodHarvestWeight, woodHarvestArea, fertilizer, countrymapping
#' @param subset Available years (usually timestep is 5+ years) are only returned if they are in subset.
#' @author Pascal Sauer, Jan Philipp Dietrich
readMagpieFulldataGdx <- function(subtype = "land", subset = 1995:2100) {
  "!# @monitor magpie4:::addGeometry"
  "!# @monitor magpie4:::land"
  "!# @monitor magpie4:::croparea"
  "!# @monitor magpie4:::woodProduction"
  "!# @monitor magpie4:::woodHarvestArea"
  "!# @monitor magpie4:::NitrogenBudget"

  gdx <- "fulldata.gdx"
  # to produce these files run MAgPIE via ./scripts/start/forestry.R
  stopifnot(file.exists(gdx),
            length(Sys.glob("clustermap_*.rds")) == 1)

  clustermap <- readRDS(Sys.glob("clustermap_*.rds"))

  if (subtype == "land") {
    x <- magpie4::land(gdx, level = "cell")
    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "data")
    unit <- "Mha"
    description <- "land use information"
  } else if (subtype == "crop") {
    x <- magpie4::croparea(gdx, level = "cell", product_aggr = FALSE, water_aggr = FALSE)
    x <- magpie4::addGeometry(x, clustermap)
    attr(x, "gdxdata") <- NULL
    getSets(x) <- c("region", "id", "year", "data", "water")
    unit <- "Mha"
    description <- "crop land information separated by irrigated/rainfed"
  } else if (subtype == "woodHarvestWeight") {
    x <- magpie4::woodProduction(gdx)
    x <- magpie4::addGeometry(x, clustermap)
    stopifnot(identical(getComment(x), " unit: Pg DM yr-1"))
    unit <- "Pg DM yr-1"
    description <- "roundwood and fuelwood harvest weight separated by source"
  } else if (subtype == "woodHarvestArea") {
    x <- magpie4::woodHarvestArea(gdx)
    x <- magpie4::addGeometry(x, clustermap)
    stopifnot(identical(getComment(x), " unit: Mha yr-1"))
    unit <- "Mha yr-1"
    description <- "wood harvest area separated by source and age classes"
  } else if (subtype == "fertilizer") {
    # suppressing the warning:
    # due to non-iteration of fertilizer distribution, residual fertilizer deficit is moved to balanceflow.
    suppressWarnings({
      x <- magpie4::NitrogenBudget(gdx, level = "cell", cropTypes = TRUE)
    })
    x <- collapseDim(x[, , "fertilizer"])
    x <- magpie4::addGeometry(x, clustermap)
    getSets(x) <- c("region", "id", "year", "cropType")
    unit <- "Tg yr-1"
    description <- "fertilization rate per croptype"
  } else if (subtype == "countrymapping") {
    clustermap <- new.magpie(clustermap$cell, sets = c("x", "y", "country", "year", "data"))
    map <- getCoords(clustermap)
    map$x0p5 <- map$x
    map$y0p5 <- map$y
    map$country <- getItems(clustermap, "country", full = TRUE)

    map1 <- map
    map1$x <- map1$x - 0.125
    map1$y <- map1$y - 0.125

    map2 <- map
    map2$x <- map2$x + 0.125
    map2$y <- map2$y - 0.125

    map3 <- map
    map3$x <- map3$x - 0.125
    map3$y <- map3$y + 0.125

    map4 <- map
    map4$x <- map4$x + 0.125
    map4$y <- map4$y + 0.125

    mapping <- rbind(map1, map2, map3, map4)
    x <- new.magpie(paste(sub("\\.", "p", mapping$x),
                          sub("\\.", "p", mapping$y),
                          mapping$country,
                          sep = "."),
                    "y2000", "dummy",
                    sets = c("x", "y", "country", "year", "data"))
    unit <- ""
    description <- "mapping of 0.25 degree cells to countrycode"
  } else {
    stop("Unknown subtype '", subtype, "' in readMagpieFulldataGdx")
  }

  x <- x[, getYears(x, as.integer = TRUE) %in% subset, ]
  return(list(x = x, min = 0, unit = unit, description = description))
}
