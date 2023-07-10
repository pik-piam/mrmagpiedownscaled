readLUH2v2h <- function(subtype = "states", subset = seq(1995, 2015, 5)) {
  if (subtype == "cellArea") {
    cellArea <- terra::rast("staticData_quarterdeg.nc", "carea")
    return(list(x = cellArea, class = "SpatRaster", cache = FALSE, unit = "km2"))
  }

  if (subtype == "states") {
    x <- terra::rast("states.nc")
    # remove secma & secmb
    x <- x[[grep("secm[ab]", names(x), invert = TRUE)]]
    unit <- "1"
  } else if (subtype == "management") {
    x <- terra::rast("management.nc")
    x <- x["crpbf|rndwd|fulwd|fertl"]
    unit <- "1, except fertl: kg ha-1 yr-1"
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
              unit = unit))
}
