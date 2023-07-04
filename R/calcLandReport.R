calcLandReport <- function(project = "RESCUE") {
  if (project == "RESCUE") {
    x <- toolAddCheckReport(calcOutput("LandHighRes", input = "magpie", target = "luh2", aggregate = FALSE))

    # combine c[34]per and c[34]per_biofuel
    per <- c("c3per", "c4per")
    biofuel <- magclass::setNames(x[, , paste0(per, "_biofuel")], per)
    x[, , per] <- x[, , per] + biofuel

    # crpbf_c[34]per = biofuel share of c[34]per
    management <- biofuel / x[, , per]
    management[biofuel == 0] <- 0 # replace NAs introduced by 0 / 0
    management <- magclass::setNames(management, paste0("crpbf_", per))
    x <- x[, , paste0(per, "_biofuel"), invert = TRUE]

    # convert to share of cell area
    stopifnot(" unit: Mha" %in% comment(x))
    cellArea <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
    cellArea <- cellArea[getItems(cellArea, 1) %in% getItems(x, 1), , ]
    cellArea <- collapseDim(cellArea, 3)
    # multiply by 10000 to convert from Mha to km2, divide by cellArea to get shares
    x <- x * 10000 / cellArea

    x <- mbind(x, management)

    attr(x, "toolCheck") <- toolCheckReport(filter = TRUE)
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
