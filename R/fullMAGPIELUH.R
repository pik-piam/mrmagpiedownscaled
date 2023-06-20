fullMAGPIELUH <- function() {
  x <- calcOutput("MagpieStatesLUH", aggregate = FALSE)

  # TODO move this into calcMagpieStatesLUH when netcdf & SpatRasterDataset can be cached
  # fill years, write one .nc file for each category to prevent memory issues
  todisk <- terra::terraOptions(print = FALSE)$todisk
  terra::terraOptions(todisk = TRUE)
  x <- as.SpatRaster(x)
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(x)))
  terra::time(x, tstep = "years") <- as.integer(substr(names(x), 2, 5))
  categories <- unique(sub("^y[0-9]{4}\\.\\.", "", names(x)))
  for (category in categories) {
    message("filling years for ", category, "...")
    filled <- toolFillYearsSpatRaster(x[category])
    terra::writeCDF(filled, paste0(category, ".nc"), category, overwrite = TRUE)
  }

  # combine into one single .nc file
  states <- terra::sds(paste0(categories, ".nc"))
  terra::writeCDF(states, "magpie_luh_states.nc")
  unlink(paste0(categories, ".nc"))
  terra::terraOptions(todisk = todisk)

  # history <- readSource("LUH2v2h", subset = paste(850:1994, collapse = "|")) # TODO Error: ! std::bad_alloc

  management <- calcOutput("MagpieManagementLUH", aggregate = FALSE)
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(management)))
  varnames <- unique(sub("^y[0-9]{4}\\.\\.", "", names(management)))
  datasets <- lapply(varnames, function(varname) management[paste0("\\.\\.", varname, "$")])
  management <- terra::sds(datasets)
  names(management) <- varnames
  terra::writeCDF(management, "magpie_luh_management.nc")
}
