#' calcNonlandTarget
#'
#' Prepare the high resolution nonland target dataset for
#' harmonization and downscaling, checking data for consistency before returning.
#' Reads in yearly data and aggregates according to the given timestep length.
#'
#' @param target name of the target dataset, currently only "luh2" and "luh2mod" are supported
#' @param years years to aggregate to/to return
#' @param timestepLength length of the timestep in years, determines the
#' first year that is read: min(years) - timestepLength + 1
#' @return nonland target data
#' @author Pascal Sauer
calcNonlandTarget <- function(target = "luh2mod", years = seq(1995, 2015, 5), timestepLength = years[2] - years[1]) {
  stopifnot(diff(years) == timestepLength)

  if (target %in% c("luh2", "luh2mod")) {
    cellAreaKm2 <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
    # convert from km2 to ha
    cellAreaHa <- cellAreaKm2 * 100
    # convert from km2 to Mha
    cellAreaMha <- cellAreaKm2 / 10000

    yearsToRead <- (min(years) - timestepLength + 1):max(years)

    management <- readSource("LUH2v2h", subtype = "management", subset = yearsToRead, convert = FALSE)

    ### fertilizer in kg yr-1
    # need absolute values for downscaling, fertl_* is in kg ha-1 yr-1, convert to kg yr-1
    fertilizer <- management["fertl"] * cellAreaHa
    names(fertilizer) <- paste0(sub("fertl_", "", names(fertilizer)), "_fertilizer")
    fertilizer <- toolAverageOverYears(fertilizer, years, timestepLength, unit = "kg yr-1")

    transitions <- readSource("LUH2v2h", subtype = "transitions", subset = yearsToRead, convert = FALSE)

    ### wood harvest area in Mha yr-1
    # convert from shares to Mha yr-1
    woodHarvestArea <- c(transitions["primf_harv"] * cellAreaMha,
                         transitions["primn_harv"] * cellAreaMha,
                         transitions["secmf_harv"] * cellAreaMha,
                         transitions["secyf_harv"] * cellAreaMha,
                         transitions["secnf_harv"] * cellAreaMha)
    names(woodHarvestArea) <- paste0(sub("_harv", "", names(woodHarvestArea)), "_wood_harvest_area")
    woodHarvestArea <- toolAverageOverYears(woodHarvestArea, years, timestepLength, unit = "Mha yr-1")

    ### wood harvest weight (bioh) in kg C yr-1
    woodHarvestWeight <- transitions["bioh"]
    minWoodHarvestWeight <- min(terra::minmax(woodHarvestWeight, compute = TRUE))
    if (minWoodHarvestWeight < 0) {
      # replace negative weight of wood harvest with 0
      toolStatusMessage("note", paste0("replacing negative wood harvest weight (bioh) with 0 (min: ",
                                       round(minWoodHarvestWeight, 3), " kg C yr-1)"))
      woodHarvestWeight <- terra::classify(woodHarvestWeight, cbind(-Inf, 0, 0))
    }
    woodHarvestWeight <- toolAverageOverYears(woodHarvestWeight, years, timestepLength, unit = "kg C yr-1")

    ### wood harvest weight type (fuelwood/roundwood) in kg C yr-1
    woodHarvestWeightType <- do.call(c, lapply(years, function(year) {
      total <- sum(woodHarvestWeight[[terra::time(woodHarvestWeight) == year]])
      roundwood <- total * management[[paste0("y", year, "..rndwd")]]
      names(roundwood) <- paste0("y", year, "..roundwood_harvest_weight_type")
      fuelwood <- total * management[[paste0("y", year, "..fulwd")]]
      names(fuelwood) <- paste0("y", year, "..fuelwood_harvest_weight_type")
      return(c(roundwood, fuelwood))
    }))
    terra::units(woodHarvestWeightType) <- "kg C yr-1"

    out <- c(woodHarvestArea, woodHarvestWeight, woodHarvestWeightType, fertilizer)
    terra::time(out, tstep = "years") <- as.integer(sub("^y([0-9]+).+", "\\1", names(out)))

    toolExpectTrue(min(terra::minmax(out)) >= 0, "All values are >= 0")

    return(list(x = out,
                class = "SpatRaster",
                unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg yr-1",
                description = "Nonland target data for data harmonization"))
  } else {
    stop("Unsupported output type \"", target, "\"")
  }
}

toolAverageOverYears <- function(x, years, timestepLength, unit) {
  yearCategory <- expand.grid(years, unique(sub("^.+\\.\\.", "", names(x))), stringsAsFactors = FALSE)
  return(do.call(c, Map(yearCategory[[1]], yearCategory[[2]], f = function(year, category) {
    layer <- terra::mean(x[[paste0("y", (year - timestepLength + 1):year, "..", category)]])
    names(layer) <- paste0("y", year, "..", category)
    terra::time(layer, tstep = "years") <- year
    terra::units(layer) <- unit
    return(layer)
  })))
}
