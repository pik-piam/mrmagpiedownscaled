#' readLUH2v2h
#'
#' Read LUH2v2h data. For the states subtype, the secma and secmb categories are removed.
#' For the management subtype, only the categories crpbf, rndwd, fulwd, fertl and irrig are read.
#' For the transitions subtype, only the wood harvest categories bioh and harv are read. To
#' match magpie semantics years are shifted by 1 when reading transitions.
#'
#' @param subtype one of states, management, transitions, cellArea
#' @param subset subset of years to read
readLUH2v2h <- function(subtype = "states", subset = seq(1995, 2015, 5)) {
  if (subtype == "cellArea") {
    cellArea <- terra::rast("staticData_quarterdeg.nc", "carea")
    return(list(x = cellArea, class = "SpatRaster", cache = FALSE, unit = "km2"))
  }

  if (subtype == "states") {
    x <- terra::rast("states.nc")
    if (!isFALSE(subset)) {
      x <- x[[terra::time(x) %in% subset]]
    }
    # remove secma & secmb
    x <- x[[grep("secm[ab]", names(x), invert = TRUE)]]
    unit <- "1"
  } else if (subtype == "management") {
    x <- terra::rast("management.nc")
    if (!isFALSE(subset)) {
      x <- x[[terra::time(x) %in% subset]]
    }

    # combf is a share of wood harvest like rndwd and fulwd, but we can ignore it as long as it is 0 everywhere
    stopifnot(identical(max(terra::values(max(x["combf"])), na.rm = TRUE), 0))

    x <- x["crpbf|rndwd|fulwd|fertl|irrig"]
    unit <- "1, except fertl: kg ha-1 yr-1"
  } else if (subtype == "transitions") {
    x <- terra::rast("transitions.nc")

    # LUH uses from-semantics for transitions (value for 1994 describes what happens from 1994 to 1995)
    # by adding 1 to time we get to-semantics (value for 1994 describes what happens from 1993 to 1994)
    terra::time(x, tstep = "years") <- terra::time(x) + 1

    if (!isFALSE(subset)) {
      x <- x[[terra::time(x) %in% subset]]
    }
    x <- x["bioh|harv"]
    unit <- "*_bioh: kg C yr-1, *_harv: 1"
  } else {
    stop("subtype must be states, management, transitions or cellArea")
  }

  names(x) <- paste0("y", terra::time(x), "..", sub("_[0-9]+$", "", names(x)))

  return(list(x = x,
              class = "SpatRaster",
              cache = FALSE,
              unit = unit))
}
