#' fullRESCUE
#'
#' Run the pipeline to generate harmonized and downscaled data to report for the RESCUE project.
#' Write .nc files, print full report on consistency checks and write it to report.log.
#'
#' @param rev revision number of the data. If not provided the current date will be used instead
#' @param ... reserved for future use
#' @param scenario scenario name to be included in filenames
#' @param compression compression level of the resulting .nc files, possible values are integers from 1-9,
#' 1 = fastest, 9 = best compression
#' @param interpolate boolean defining whether the data should be interpolated to annual values or not
#'
#' @author Pascal Sauer, Jan Philipp Dietrich
fullRESCUE <- function(rev = NULL, ..., scenario = "", years = 2015:2100,
                       compression = 2, interpolate = FALSE) {
  stopifnot(...length() == 0)
  missingValue <- 1e20
  resolution <- 0.25

  now <- Sys.time()
  version <- if (is.null(rev)) format(now, "%Y-%m-%d") else rev
  fileSuffix <- paste0("_input4MIPs_landState_RESCUE_PIK-MAgPIE-4-7-",
                       scenario, if (scenario == "") "" else "-",
                       version, "_gn_", min(years), "-", max(years))



  land <- calcOutput("LandReport", project = "RESCUE", aggregate = FALSE)
  # land <- readRDS("landSample.rds") # TODO remove, comment in line above
  land <- land[, getYears(land, as.integer = TRUE) %in% years, ]
  # account for unit "years since 2015-01-01 0:0:0" which addMetadataRESCUE sets
  land <- setYears(land, getYears(land, as.integer = TRUE) - 2015)
  land <- magclass::extend(land,
                           xRange = c(-180 + resolution / 2, 180 - resolution / 2),
                           yRange = c(90 - resolution / 2, -90 + resolution / 2),
                           res = resolution)

  statesFile <- paste0("multiple-states", fileSuffix, ".nc")
  statesVariables <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr",
                       "primf", "primn", "range", "secdf", "secdn", "urban")
  write.magpie(land[, , statesVariables], statesFile, missval = missingValue, compression = compression)
  addMetadataRESCUE(statesFile, now = now, missingValue = missingValue, variableId = "multiple-states",
                    resolution = resolution)

  stop("not implemented yet") # TODO


  nonland <- calcOutput("NonlandReport", project = "RESCUE", aggregate = FALSE,
                        warnNA = FALSE) # rndwd & fulwd include NAs
  toolWriteManagement(land, nonland, fileSuffix = fileSuffix, now = now, compression = compression,
                      interpolate = interpolate)
  rm(land)



  transitions <- calcOutput("LandTransitions", project = "RESCUE", aggregate = FALSE)
  toolWriteTransitions(transitions, nonland, fileSuffix = fileSuffix, now = now,
                       compression = compression, interpolate = interpolate)
}

addMetadataRESCUE <- function(ncFile, now, missingValue, variableId, resolution) {
  stopifnot(variableId %in% c("multiple-states", "multiple-management", "multiple-transitions"))
  nc <- ncdf4::nc_open(ncFile, write = TRUE)
  withr::defer({
    ncdf4::nc_close(nc)
  })
  # global
  dateTime <- strftime(now, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ncdf4::ncatt_put(nc, 0, "activity_id", "RESCUE")
  ncdf4::ncatt_put(nc, 0, "contact", "pascal.sauer@pik-potsdam.de, dietrich@pik-potsdam.de")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.6")
  ncdf4::ncatt_put(nc, 0, "creation_date", dateTime)
  ncdf4::ncatt_put(nc, 0, "data_structure", "grid")
  ncdf4::ncatt_put(nc, 0, "dataset_category", "landState")
  ncdf4::ncatt_put(nc, 0, "date", dateTime)
  ncdf4::ncatt_put(nc, 0, "frequency", "yr")
  ncdf4::ncatt_put(nc, 0, "further_info_url", "https://github.com/pik-piam/mrdownscale")
  ncdf4::ncatt_put(nc, 0, "grid_label", "gn")
  ncdf4::ncatt_put(nc, 0, "institution_id", "PIK")
  ncdf4::ncatt_put(nc, 0, "institution", "Potsdam Institute for Climate Impact Research")
  ncdf4::ncatt_put(nc, 0, "license", "CC BY 4.0")
  ncdf4::ncatt_put(nc, 0, "nominal_resolution", "50 km")
  ncdf4::ncatt_put(nc, 0, "realm", "land")
  ncdf4::ncatt_put(nc, 0, "source_id", sub(paste0("^", variableId, "_"), "",
                                           sub("\\.nc$", "",
                                               basename(ncFile))))
  ncdf4::ncatt_put(nc, 0, "target_mip", "input4MIPs")
  ncdf4::ncatt_put(nc, 0, "title", "MAgPIE Land-Use Data Harmonized and Downscaled using LUH2 v2h as reference")
  ncdf4::ncatt_put(nc, 0, "variable_id", variableId)
  # time
  ncdf4::ncatt_put(nc, "time", "axis", "T")
  ncdf4::ncatt_put(nc, "time", "bounds", "bounds_time")
  ncdf4::ncatt_put(nc, "time", "calendar", "365_day")
  ncdf4::ncatt_put(nc, "time", "long_name", "time")
  ncdf4::ncatt_put(nc, "time", "realtopology", "linear")
  ncdf4::ncatt_put(nc, "time", "standard_name", "time")
  ncdf4::ncatt_put(nc, "time", "units", "years since 2015-01-01 0:0:0")
  # lon
  ncdf4::ncatt_put(nc, "lon", "realtopology", "circular")
  ncdf4::ncatt_put(nc, "lon", "topology", "circular")
  ncdf4::ncatt_put(nc, "lon", "modulo", 360, prec = "double")
  ncdf4::ncatt_put(nc, "lon", "long_name", "longitude")
  ncdf4::ncatt_put(nc, "lon", "standard_name", "longitude")
  ncdf4::ncatt_put(nc, "lon", "axis", "X")
  ncdf4::ncatt_put(nc, "lon", "bounds", "bounds_lon")
  # lat
  ncdf4::ncatt_put(nc, "lat", "realtopology", "linear")
  ncdf4::ncatt_put(nc, "lat", "long_name", "longitude")
  ncdf4::ncatt_put(nc, "lat", "standard_name", "longitude")
  ncdf4::ncatt_put(nc, "lat", "axis", "Y")
  ncdf4::ncatt_put(nc, "lat", "bounds", "bounds_lat")
  # others
  luhNames <- toolGetMapping("luhNames.csv", where = "mrdownscale")
  for (varname in names(nc$var)) {
    ncdf4::ncatt_put(nc, varname, "_Fillvalue", missingValue, prec = "float")
    ncdf4::ncatt_put(nc, varname, "missing_value", missingValue, prec = "float")
    ncdf4::ncatt_put(nc, varname, "cell_methods", "time:mean")
    if (varname %in% luhNames$name) {
      ncdf4::ncatt_put(nc, varname, "long_name", luhNames$long_name[luhNames$name == varname])
      ncdf4::ncatt_put(nc, varname, "standard_name", luhNames$standard_name[luhNames$name == varname])
    } else {
      warning(varname, " not found in luhNames.csv")
    }
  }

  # add bounds
  boundsDim <- ncdf4::ncdim_def("bounds", "", 1:2, create_dimvar = FALSE)

  # TODO pass compression to ncvard_def
  nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("bounds_lon", units = "",
                                              dim = list(boundsDim, nc$dim$lon), prec = "double"))
  nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("bounds_lat", units = "",
                                              dim = list(boundsDim, nc$dim$lat), prec = "double"))
  nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("bounds_time", units = "",
                                              dim = list(boundsDim, nc$dim$time), prec = "integer"))

  ncdf4::ncvar_put(nc, "bounds_lon", rbind(nc$dim$lon$vals - resolution / 2,
                                           nc$dim$lon$vals + resolution / 2))
  ncdf4::ncvar_put(nc, "bounds_lat", rbind(nc$dim$lat$vals + resolution / 2,
                                           nc$dim$lat$vals - resolution / 2))
  # these time bounds were taken from the reference file
  # multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc
  ncdf4::ncvar_put(nc, "bounds_time", rbind(rep(1, nc$dim$time$len),
                                            rep(365, nc$dim$time$len)))
}
