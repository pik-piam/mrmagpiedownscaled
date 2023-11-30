#' calcNonlandReport
#'
#' Convert the downscaled nonland data to the format required by the given project.
#'
#' @param project name of the project, currently only "RESCUE"
#' @return nonland data
#' @author Pascal Sauer
calcNonlandReport <- function(project = "RESCUE") {
  if (project == "RESCUE") {
    x <- calcOutput("NonlandHighRes", input = "magpie", target = "luh2mod", aggregate = FALSE)

    cellAreaKm2 <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
    # convert from km2 to ha
    cellAreaHa <- cellAreaKm2 * 100
    cellAreaMha <- cellAreaKm2 / 10000

    # convert from kg yr-1 to kg ha-1 yr-1
    fertl <- x["fertilizer"] / cellAreaHa
    names(fertl) <- sub("\\.\\.(.+)_fertilizer$", "..fertl_\\1", names(fertl))

    # convert from Mha to shares
    harv <- x["wood_harvest_area"] / cellAreaMha
    names(harv) <- sub("wood_harvest_area$", "harv", names(harv))

    # calculate wood harvest type shares
    years <- unique(terra::time(x))
    woodTypeShares <- do.call(c, lapply(years, function(year) {
      total <- sum(x[[terra::time(x) == year]]["(roundwood|fuelwood)_harvest_weight_type"])
      rndwd <- x[[paste0("y", year, "..roundwood_harvest_weight_type")]] / total
      names(rndwd) <- paste0("y", year, "..rndwd")
      fulwd <- x[[paste0("y", year, "..fuelwood_harvest_weight_type")]] / total
      names(fulwd) <- paste0("y", year, "..fulwd")
      return(c(rndwd, fulwd))
    }))

    x <- c(fertl, harv, woodTypeShares, x["_bioh$"])
    x <- terra::writeRaster(x, file = tempfile(fileext = ".tif"))

    return(list(x = x,
                class = "SpatRaster",
                unit = "rndwd & fulwd: 1; bioh: kg C yr-1; harv: 1; fertl: kg ha-1 yr-1",
                description = "Downscaled nonland data report for RESCUE"))
  } else {
    stop("Can only report for project = 'RESCUE'")
  }
}
