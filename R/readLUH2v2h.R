readLUH2v2h <- function(subtype = "LUH", subset = "1995|2000|2005|2010|2015") {
  if (subtype == "LUH") {
    x <- terra::rast("states.nc")

    # remove secma & secmb
    x <- x[[grep("secm[ab]", names(x), invert = TRUE, value = TRUE)]]

    names(x) <- paste0("y", terra::time(x), "..", sub("_.+", "", names(x)))

    if (!isFALSE(subset)) {
      x <- x[as.character(subset)]
    }

    return(list(x = x,
                class = "SpatRaster",
                cache = FALSE,
                unit = "1"))
  } else if (subtype == "cellArea") {
    cellArea <- read.magpie("staticData_quarterdeg.nc")[, , "carea"]
    cellArea <- collapseDim(cellArea, 2)
    return(list(x = cellArea, unit = "km2"))
  } else {
    stop("subtype must be LUH or cellArea")
  }
}
