fullMAGPIELUH <- function() {
  terra::terraOptions(memfrac = 0.2, todisk = TRUE)
  x <- calcOutput("MagpieStatesLUH", aggregate = FALSE)
  # write.magpie(x, "magpie_luh_states.nc")
  x <- as.SpatRaster(x)
  terra::time(x, tstep = "years") <- as.integer(substr(names(x), 2, 5))
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(x)))
  categories <- unique(sub("^y[0-9]{4}\\.\\.", "", names(x)))
  lapply(categories, function(category) {
    message("filling years for ", category, "... ", appendLF = FALSE)
    filled <- fillYears(x[category])
    terra::writeCDF(filled, paste0(category, ".nc"), category)
    gc()
    message("done.")
    TRUE
  })

  # combine into one single .nc file
  states <- terra::sds(paste0(categories, ".nc"))
  terra::writeCDF(states, "magpie_luh_states.nc")
  unlink(paste0(categories, ".nc"))

  # history <- readSource("LUH2v2h", subset = paste(850:1994, collapse = "|")) # TODO Error: ! std::bad_alloc

  management <- calcOutput("MagpieManagementLUH", aggregate = FALSE)
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(management)))
  varnames <- unique(sub("^y[0-9]{4}\\.\\.", "", names(management)))
  datasets <- lapply(varnames, function(varname) management[paste0("\\.\\.", varname, "$")])
  management <- terra::sds(datasets)
  names(management) <- varnames
  terra::writeCDF(management, "magpie_luh_management.nc")
}

# TODO integrate into toolFillYears or turn into separate tool function
fillYears <- function(x, years = NULL) {
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(x)[[1]]))
  category <- sub("^y[0-9]{4}\\.\\.", "", names(x)[[1]])
  xYears <- terra::time(x)
  if (any(is.na(xYears))) {
    stop("terra::time must be set for fillYears to work")
  }
  if (is.unsorted(xYears)) {
    stop("xYears must be sorted")
  }
  years <- if (is.null(years)) min(xYears):max(xYears) else sort(years)
  out <- lapply(years, function(year) {
    if (year %in% xYears) {
      return(x[as.character(year)])
    }
    # find the last xYear bigger than `year`
    a <- Find(function(y) y < year, xYears, right = TRUE)
    # find the first xYear bigger than `year`
    b <- Find(function(y) y > year, xYears)
    p <- (year - a) / (b - a)
    stopifnot(p >= 0 && p <= 1)
    # interpolate between anchor years a and b
    out <- (1 - p) * x[as.character(a)] + p * x[as.character(b)]
    stopifnot(terra::nlyr(out) == 1)
    names(out) <- paste0("y", year, "..", category)
    return(out)
  })
  return(do.call(c, out))
}
