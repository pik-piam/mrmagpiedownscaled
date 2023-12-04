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

    cellArea <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
    cellArea <- as.magpie(cellArea)
    stopifnot(getItems(x, 1) %in% getItems(cellArea, 1))
    cellArea <- collapseDim(cellArea[getItems(x, 1), , ], 3)
    # convert from km2 to ha
    cellAreaHa <- cellArea * 100
    cellAreaMha <- cellArea / 10000

    # convert from kg yr-1 to kg ha-1 yr-1
    fertl <- x[, , grep("fertilizer$", getNames(x))] / cellAreaHa
    getNames(fertl) <- sub("(.+)_fertilizer$", "fertl_\\1", getNames(fertl))

    # convert from Mha to shares
    harv <- x[, , grep("wood_harvest_area$", getNames(x))] / cellAreaMha
    getNames(harv) <- sub("wood_harvest_area$", "harv", getNames(harv))

    woodTypeShares <- x[, , c("roundwood_harvest_weight_type", "fuelwood_harvest_weight_type")]
    total <- dimSums(woodTypeShares, 3) # 1269815 cells with total == 0
    woodTypeShares <- woodTypeShares / total # NAs introduced by cells with total == 0
    getNames(woodTypeShares) <- c("rndwd", "fulwd")

    out <- mbind(fertl, harv, woodTypeShares, x[, , grep("bioh$", getNames(x))])

    stopifnot(all(out[, , grep("(harv|rndwd|fulwd)$", getNames(x))] < 1.0001, na.rm = TRUE),
              !is.na(out[, , c("rndwd", "fulwd"), invert = TRUE]))

    return(list(x = out,
                isocountries = FALSE,
                min = 0,
                unit = "rndwd & fulwd: 1; bioh: kg C yr-1; harv: 1; fertl: kg ha-1 yr-1",
                description = "Downscaled nonland data report for RESCUE"))
  } else {
    stop("Can only report for project = 'RESCUE'")
  }
}
