#' calcNonlandReport
#'
#' Convert the downscaled nonland data to the format required by the given project.
#'
#' @param project name of the project, currently only "RESCUE"
#' @return nonland data
#' @author Pascal Sauer
calcNonlandReport <- function(project = "RESCUE") {
  if (project == "RESCUE") {
    x <- calcOutput("NonlandHighRes", input = "magpie", target = "luh2", aggregate = FALSE)

    # *_fertilizer is in kg yr-1, need to report in kg ha-1 yr-1, so divide by cell area
    cellArea <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
    cellArea <- collapseDim(as.magpie(cellArea), 3)
    stopifnot(getItems(x, 1) %in% getItems(cellArea, 1))
    x <- x / (cellArea[getItems(x, 1), , ] * 100) # divide by cell area in ha (*100 to convert from km2 to ha)
    getNames(x) <- sub("^(.+)_fertilizer$", "fertl_\\1", getNames(x))

    return(list(x = x,
                isocountries = FALSE,
                unit = "fertl_*: kg ha-1 yr-1",
                min = 0,
                description = "Downscaled nonland data report for RESCUE"))
  } else {
    stop("Can only report for project = 'RESCUE'")
  }
}
