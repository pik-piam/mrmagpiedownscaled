fullMAGPIELUH <- function() {
  options(toolCheck = NULL)
  x <- toolAddCheckReport(calcOutput("LandReport", aggregate = FALSE))

  # TODO move this into calcLandReport when netcdf & SpatRasterDataset can be cached
  history <- readSource("LUH2v2h", subset = FALSE, convert = FALSE)
  history <- history[[terra::time(history) < 1995]]

  # fill years, write one .nc file for each category to prevent memory issues
  todisk <- terra::terraOptions(print = FALSE)$todisk
  withr::defer({
    terra::terraOptions(todisk = todisk)
  })
  terra::terraOptions(todisk = TRUE)
  x <- as.SpatRaster(x)
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(x)))
  terra::time(x, tstep = "years") <- as.integer(substr(names(x), 2, 5))
  categories <- unique(sub("^y[0-9]{4}\\.\\.", "", names(x)))
  managementCategories <- grep("crpbf", categories, value = TRUE)
  statesCategories <- setdiff(categories, managementCategories)
  lapply(categories, function(category) {
    message(category)
    filled <- toolFillYearsSpatRaster(x[paste0("\\.\\.", category, "$")])
    extended <- terra::extend(filled, history)
    # combined <- c(history[category], extended)
    terra::writeCDF(extended, paste0(category, ".nc"), category, overwrite = TRUE)
    TRUE
  })

  # combine into one single .nc file
  terra::writeCDF(terra::sds(paste0(statesCategories, ".nc")), "magpie_luh_states.nc")

  management <- toolAddCheckReport(calcOutput("MagpieManagementLUH", aggregate = FALSE))
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(management)))
  varnames <- unique(sub("^y[0-9]{4}\\.\\.", "", names(management)))
  datasets <- lapply(varnames, function(varname) toolFillYearsSpatRaster(management[paste0("\\.\\.", varname, "$")]))
  management <- terra::sds(datasets)
  names(management) <- varnames
  management <- c(management, terra::sds(paste0(managementCategories, ".nc")))

  terra::writeCDF(management, "magpie_luh_management.nc")
  # unlink(paste0(categories, ".nc")) # TODO comment in

  report <- toolCheckReport(unlist = TRUE)
  cat(report, sep = "\n")
  writeLines(report, "report.log")
}
