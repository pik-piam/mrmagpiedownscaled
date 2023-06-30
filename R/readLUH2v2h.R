readLUH2v2h <- function(subtype = "states", subset = seq(1995, 2015, 5)) {
  if (subtype == "cellArea") {
    cellArea <- read.magpie("staticData_quarterdeg.nc")[, , "carea"]
    cellArea <- collapseDim(cellArea, 2)
    return(list(x = cellArea, unit = "km2"))
  }

  if (subtype == "states") {
    x <- terra::rast("states.nc")
    # remove secma & secmb
    x <- x[[grep("secm[ab]", names(x), invert = TRUE)]]
  } else if (subtype == "management") {
    x <- terra::rast("management.nc")
    x <- x["crpbf|rndwd|fulwd"]
  } else {
    stop("subtype must be states, management or cellArea")
  }

  names(x) <- paste0("y", terra::time(x), "..", sub("_[0-9]+$", "", names(x)))

  if (!isFALSE(subset)) {
    x <- x[[terra::time(x) %in% subset]]
  }

  return(list(x = x,
              class = "SpatRaster",
              cache = FALSE,
              unit = "1"))
}
