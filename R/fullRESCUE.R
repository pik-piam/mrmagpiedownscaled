#' fullRESCUE
#'
#' Run the pipeline to generate harmonized and downscaled data to report for the RESCUE project.
#' Write .nc files, print full report on consistency checks and write it to report.log.
#'
#' @param rev revision number of the data
#'
#' @author Pascal Sauer
fullRESCUE <- function(rev = 2) {
  missingValue <- 1e20
  extent <- terra::ext(-180, 180, -55.5, 83.25)

  .writeNC <- function(x, fileName) {
    # setting chunksizes might speed up nc access, but
    # terra::writeCDF only allows setting one global set of chunksizes, but e.g. "time" needs a different one
    terra::writeCDF(x, fileName, overwrite = TRUE, missval = missingValue)
  }

  .convertExtendUnitWrite <- function(x, fileName) {
    xRaster <- toolSpatRasterToDataset(terra::extend(as.SpatRaster(x), extent))
    terra::units(xRaster) <- sub(" unit: ", "", grep(" unit: ", getComment(x), value = TRUE))
    .writeNC(xRaster, fileName)
  }

  # set terra::units on a SpatRasterDataset using the units specified in individual SpatRasters
  .setUnits <- function(x) {
    terra::units(x) <- vapply(x, function(spatRaster) {
      stopifnot(length(unique(terra::units(spatRaster))) == 1) # assert each year-layer has the same unit
      return(terra::units(spatRaster)[1])
    }, character(1))
    return(x)
  }

  .addMetadata <- function(ncFile, comment) {
    nc <- ncdf4::nc_open(ncFile, write = TRUE)
    withr::defer({
      ncdf4::nc_close(nc)
    })
    # global
    ncdf4::ncatt_put(nc, 0, "activity_id", "RESCUE")
    ncdf4::ncatt_put(nc, 0, "comment", comment)
    ncdf4::ncatt_put(nc, 0, "contact", "pascal.fuehrlich@pik-potsdam.de, dietrich@pik-potsdam.de")
    ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.6")
    now <- strftime(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ncdf4::ncatt_put(nc, 0, "creation_date", now)
    ncdf4::ncatt_put(nc, 0, "date", now)
    ncdf4::ncatt_put(nc, 0, "data_structure", "grid")
    ncdf4::ncatt_put(nc, 0, "frequency", "yr")
    ncdf4::ncatt_put(nc, 0, "further_info_url", "https://github.com/pik-piam/mrdownscale")
    ncdf4::ncatt_put(nc, 0, "institution_id", "PIK")
    ncdf4::ncatt_put(nc, 0, "institution", "Potsdam Institute for Climate Impact Research")
    ncdf4::ncatt_put(nc, 0, "license", "CC BY 4.0")
    ncdf4::ncatt_put(nc, 0, "realm", "land")
    ncdf4::ncatt_put(nc, 0, "title", "MAgPIE Land-Use Data Harmonized and Downscaled using LUH2 v2h as reference")
    # time
    ncdf4::ncatt_put(nc, "time", "units", "years since 1970-01-01 0:0:0")
    ncdf4::ncatt_put(nc, "time", "calendar", "365_day")
    ncdf4::ncatt_put(nc, "time", "long_name", "time")
    ncdf4::ncatt_put(nc, "time", "standard_name", "time")
    ncdf4::ncatt_put(nc, "time", "axis", "T")
    # longitude and latitude
    # renaming to lon/lat is not possible with ncdf4::ncvar_rename
    ncdf4::ncatt_put(nc, "longitude", "standard_name", "longitude")
    ncdf4::ncatt_put(nc, "longitude", "axis", "Y")
    ncdf4::ncatt_put(nc, "latitude", "standard_name", "latitude")
    ncdf4::ncatt_put(nc, "latitude", "axis", "X")
    # others
    luhNames <- toolGetMapping("luhNames.csv", where = "mrdownscale")
    for (varname in setdiff(names(nc$var), "crs")) {
      ncdf4::ncatt_put(nc, varname, "_fillvalue", missingValue, prec = "float")
      ncdf4::ncatt_put(nc, varname, "missing_value", missingValue, prec = "float")
      ncdf4::ncatt_put(nc, varname, "cell_methods", "time:mean")
      stopifnot(varname %in% luhNames$name)
      ncdf4::ncatt_put(nc, varname, "long_name", luhNames$long_name[luhNames$name == varname])
      ncdf4::ncatt_put(nc, varname, "standard_name", luhNames$standard_name[luhNames$name == varname])
    }
  }

  land <- calcOutput("LandReport", project = "RESCUE", aggregate = FALSE)

  statesVariables <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr",
                       "primf", "primn", "range", "secdf", "secdn", "urban")
  stopifnot(statesVariables %in% getNames(land))
  for (i in seq_along(statesVariables)) {
    statesVariable <- statesVariables[i]
    message(i, "/", length(statesVariables), " writing ", statesVariable, ".nc")
    .convertExtendUnitWrite(land[, , statesVariable], paste0(statesVariable, ".nc"))
  }
  states <- terra::sds(paste0(statesVariables, ".nc"))
  states <- .setUnits(states)
  statesFile <- paste0("multiple-states_input4MIPs_landState_RESCUE_PIK-MAgPIE4.6.11-", rev,
                       "_gn_1995-2100.nc")
  .writeNC(states, statesFile)
  unlink(paste0(statesVariables, ".nc"))
  .addMetadata(statesFile, "states")

  # missing: combf, rndwd, fulwd, crpbf_total, fharv_c3per, fharv_c4per, flood, lat_bounds, lon_bounds,
  managementVariables <- c("fertl_c3ann", "fertl_c3nfx", "fertl_c3per", "fertl_c4ann", "fertl_c4per",
                           "irrig_c3ann", "irrig_c3nfx", "irrig_c3per", "irrig_c4ann", "irrig_c4per",
                           "crpbf_c3ann", "crpbf_c3nfx", "crpbf_c3per", "crpbf_c4ann", "crpbf_c4per",
                           "crpbf2_c3per", "crpbf2_c4per", "manaf")
  nonland <- calcOutput("NonlandReport", project = "RESCUE", aggregate = FALSE)
  for (i in seq_along(managementVariables)) {
    managementVariable <- managementVariables[i]
    message(i, "/", length(managementVariables), " writing ", managementVariable, ".nc")
    if (managementVariable %in% getNames(land)) {
      x <- land[, , managementVariable]
    } else {
      x <- nonland[, , managementVariable]
    }
    .convertExtendUnitWrite(x, paste0(managementVariable, ".nc"))
  }
  management <- terra::sds(paste0(managementVariables, ".nc"))
  management <- .setUnits(management)
  managementFile <- paste0("multiple-management_input4MIPs_landState_RESCUE_PIK-MAgPIE4.6.11-", rev,
                           "_gn_1995-2100.nc")
  .writeNC(management, managementFile)
  unlink(paste0(managementVariables, ".nc"))
  .addMetadata(managementFile, "management")
}
