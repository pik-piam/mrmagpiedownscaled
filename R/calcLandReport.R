#' calcLandReport
#'
#' Convert the downscaled land use data to the format required by the given project.
#'
#' @param project name of the project, currently only "RESCUE"
#' @return land use data
#' @author Pascal Führlich
calcLandReport <- function(project = "RESCUE") {
  if (project == "RESCUE") {
    native <- calcOutput("LandHighRes", input = "magpie", target = "luh2", aggregate = FALSE)
    cellArea <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
    cellArea <- collapseDim(as.magpie(cellArea), 3)

    # calc irrigation shares
    cropTypes <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per")
    out <- do.call(mbind, lapply(cropTypes, function(cropType) {
      x <- native[, , grep(cropType, getItems(native, 3))]
      total <- dimSums(x, dim = 3)

      irrigationShare <- dimSums(x[, , grep("irrigated", getItems(x, 3))], dim = 3) / total
      getNames(irrigationShare) <- paste0("irrig_", cropType)
      irrigationShare[total == 0] <- 0 # replace NAs introduced by 0 / 0

      biofuel1stGenShare <- dimSums(x[, , grep("biofuel_1st_gen", getItems(x, 3))], dim = 3) / total
      getNames(biofuel1stGenShare) <- paste0("crpbf_", cropType)
      biofuel1stGenShare[total == 0] <- 0 # replace NAs introduced by 0 / 0

      # multiply by 10000 to convert from Mha to km2, divide by cellArea to get shares
      cellAreaShare <- total * 10000 / cellArea[getItems(cellArea, 1) %in% getItems(x, 1), , ]
      getNames(cellAreaShare) <- cropType

      combined <- mbind(irrigationShare, biofuel1stGenShare, cellAreaShare)
      if (any(grepl("biofuel_2nd_gen", getItems(x, 3)))) {
        biofuel2ndGenShare <- dimSums(x[, , grep("biofuel_2nd_gen", getItems(x, 3))], dim = 3) / total
        getNames(biofuel2ndGenShare) <- paste0("crpbf2_", cropType)
        biofuel2ndGenShare[total == 0] <- 0 # replace NAs introduced by 0 / 0
        combined <- mbind(combined, biofuel2ndGenShare)
      }
      return(combined)
    }))

    stopifnot(!any(is.na(out)))

    return(list(x = out,
                isocountries = FALSE,
                unit = "1",
                min = 0,
                max = 1.0001,
                description = "MAgPIE land use data downscaled to LUH2 resolution"))
  } else {
    stop("Can only report for project = 'RESCUE'")
  }
}
