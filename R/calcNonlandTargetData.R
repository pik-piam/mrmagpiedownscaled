calcNonlandTargetData <- function(target = "luh2") {
  if (target == "luh2") {
    man <- readSource("LUH2v2h", subtype = "management", convert = FALSE)

    # need absolute values for downscaling, fertl_* is in kg ha-1 yr-1, convert to kg yr-1
    cellArea <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
    fertl <- man["fertl"] * cellArea
    terra::units(fertl) <- "kg yr-1"
    names(fertl) <- paste0(sub("fertl_", "", names(fertl)), "_fertilizer")

    out <- c(man["rndwd|fulwd"], fertl)
    attr(out, "toolCheck") <- toolCheckReport(filter = TRUE)
    return(list(x = out,
                class = "SpatRaster",
                cache = FALSE,
                unit = "rndwd, fulwd: 1, fertl_*: kg yr-1",
                description = "Nonland target data for data harmonization"))
  } else {
    stop("Unsupported output type \"", target, "\"")
  }
}
