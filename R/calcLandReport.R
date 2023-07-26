#' calcLandReport
#'
#' Convert the downscaled land use data to the format required by the given project.
#'
#' @param project name of the project, currently only "RESCUE"
#' @return land use data
#' @author Pascal Führlich
calcLandReport <- function(project = "RESCUE") {
  if (project == "RESCUE") {
    stop("The RESCUE reporting does not work at the moment. We are waiting for the RESCUE check tool.")
    x <- calcOutput("LandHighRes", input = "magpie", target = "luh2", aggregate = FALSE)

    # combine c[34]per and c[34]per_biofuel
    per <- c("c3per", "c4per")
    nonPer <- c("c3ann", "c4ann", "c3nfx")
    biofuel1stGen <- magclass::setNames(x[, , paste0(c(per, nonPer), "_biofuel_1st_gen")], c(per, nonPer))
    biofuel2ndGen <- magclass::setNames(x[, , paste0(per, "_biofuel_2nd_gen")], per)
    x[, , per] <- x[, , per] + biofuel1stGen[, , per] + biofuel2ndGen
    x[, , nonPer] <- x[, , nonPer] + biofuel1stGen[, , nonPer]

    # crpbf_c[34]per = biofuel share of c[34]per
    biofuel <- x[, , grep("biofuel", getItems(x, 3))]
    stopifnot(!is.na(biofuel))
    biofuel <- biofuel / magclass::setNames(x[, , sub("_.+$", "", getItems(biofuel, 3))], getItems(biofuel, 3))
    biofuel[is.na(biofuel)] <- 0 # replace NAs introduced by 0 / 0
    biofuel <- magclass::setNames(biofuel, paste0("crpbf_", sub("biofuel_", "", getItems(biofuel, 3))))

    x <- x[, , getItems(biofuel, 3), invert = TRUE]

    # convert to share of cell area
    stopifnot(" unit: Mha" %in% comment(x))
    cellArea <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
    cellArea <- as.magpie(cellArea)
    cellArea <- cellArea[getItems(cellArea, 1) %in% getItems(x, 1), , ]
    cellArea <- collapseDim(cellArea, 3)
    # multiply by 10000 to convert from Mha to km2, divide by cellArea to get shares
    x <- x * 10000 / cellArea

    x <- mbind(x, biofuel)

    return(list(x = x,
                isocountries = FALSE,
                unit = "1",
                min = 0,
                max = 1.0001,
                description = paste("MAgPIE land use data downscaled to LUH2 resolution,",
                                    "unit: share of cell area")))
  } else {
    stop("Can only report for project = 'RESCUE'")
  }
}
