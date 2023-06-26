fullRESCUE <- function() {
  options(toolCheck = NULL) # nolint: undesirable_function_linter

  historyStates <- readSource("LUH2v2h", subtype = "states", subset = 850:1994, convert = FALSE)
  historyStates <- toolSpatRasterToDataset(historyStates)
  terra::writeCDF(historyStates, "history_states.nc", compress = 4)
  gc()

  historyManagement <- readSource("LUH2v2h", subtype = "management", subset = 850:1994, convert = FALSE)
  historyManagement <- toolSpatRasterToDataset(historyManagement)
  terra::writeCDF(historyManagement, "history_management.nc", compress = 4)
  gc()

  x <- toolAddCheckReport(calcOutput("LandReport", project = "RESCUE", aggregate = FALSE))
  # TODO move this into calcLandReport when netcdf & SpatRasterDataset can be cached
  # fill years, write one .nc file for each category to prevent memory issues
  todisk <- terra::terraOptions(print = FALSE)$todisk
  withr::defer({
    terra::terraOptions(todisk = todisk)
  })
  terra::terraOptions(todisk = TRUE)
  x <- as.SpatRaster(x)
  gc()
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(x)))
  terra::time(x, tstep = "years") <- as.integer(substr(names(x), 2, 5)) # TODO make as.SpatRaster set time
  varnames <- unique(sub("^y[0-9]+\\.\\.", "", names(x)))

  for (category in varnames) {
    message("filling years for ", category)
    terra::writeCDF(toolFillYearsSpatRaster(x[paste0("\\.\\.", category, "$")]),
                    paste0(category, ".nc"), overwrite = TRUE)
    gc()
  }

  statesCategories <- c("primf", "primn", "secdf", "secdn", "urban", "c3ann",
                        "c4ann", "c3per", "c4per", "c3nfx", "pastr", "range")
  # combine into one single .nc file
  states <- terra::sds(paste0(statesCategories, ".nc"))
  terra::writeCDF(states, "magpie_luh_states.nc", compress = 4)
  gc()


  management <- toolAddCheckReport(calcOutput("MagpieManagementLUH", aggregate = FALSE))
  management <- toolSpatRasterToDataset(management)
  for (category in names(management)) {
    message("filling years for ", category)
    terra::writeCDF(toolFillYearsSpatRaster(management[category]), paste0(category, ".nc"), overwrite = TRUE)
    gc()
  }

  managementCategories <- c("crpbf_c3per", "crpbf_c4per", "rndwd", "fulwd")
  crpbf <- terra::rast(c("crpbf_c3per.nc", "crpbf_c4per.nc"))
  wood <- terra::rast(c("rndwd.nc", "fulwd.nc"))
  crpbf <- terra::extend(crpbf, wood)
  gc()
  management <- c(crpbf, wood)
  names(management) <- paste0("y", terra::time(management), "..", sub("_[0-9]+$", "", names(management)))
  management <- toolSpatRasterToDataset(management)
  stopifnot(all.equal(names(management), managementCategories))
  terra::writeCDF(management, "magpie_luh_management.nc", compress = 4)
  gc()

  unlink(paste0(c(statesCategories, managementCategories), ".nc"))

  report <- unlist(toolCheckReport(), use.names = FALSE)
  cat(report, sep = "\n")
  writeLines(report, "report.log")
}
