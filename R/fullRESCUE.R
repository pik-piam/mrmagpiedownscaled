# Pascal

fullRESCUE <- function() {

  x <- calcOutput("LandReport", project = "RESCUE", aggregate = FALSE)
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

  # TODO test this management reporting
  managementCategories <- c("crpbf_c3per", "crpbf_c4per")
  management <- terra::rast(paste0(managementCategories, ".nc"))
  names(management) <- paste0("y", terra::time(management), "..", sub("_[0-9]+$", "", names(management)))
  management <- toolSpatRasterToDataset(management)
  stopifnot(all.equal(names(management), managementCategories))
  terra::writeCDF(management, "magpie_luh_management.nc", compress = 4)
  gc()

  unlink(paste0(c(statesCategories, managementCategories), ".nc"))

  report <- yaml::as.yaml(getMadratMessage("status"))
  cat(report)
  writeLines(report, "report.log")
}
