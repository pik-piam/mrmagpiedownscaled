#' fullRESCUE
#'
#' Run the pipeline to generate harmonized and downscaled data to report for the RESCUE project.
#' Write .nc files, print full report on consistency checks and write it to report.log.
#'
#' @param rev revision number of the data. If not provided the current date will be used instead
#' @param ... reserved for future use
#' @param scenario scenario name to be included in filenames
#' @param years Only years from this vector will be included in the output files. If interpolate is TRUE,
#' all years given here will be included
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param compression compression level of the resulting .nc files, possible values are integers from 1-9,
#' 1 = fastest, 9 = best compression
#' @param interpolate boolean defining whether the data should be interpolated
#' to include all years given in the years argument
#'
#' @author Pascal Sauer, Jan Philipp Dietrich
fullRESCUE <- function(rev = NULL, ..., scenario = "", years = 1995:2100,
                       harmonizationPeriod = c(2015, 2050),
                       compression = 2, interpolate = FALSE) {
  stopifnot(...length() == 0, isFALSE(interpolate))
  missingValue <- 1e20
  gridDefinition <- c(-179.875, 179.875, -89.875, 89.875, 0.25)
  resolution <- gridDefinition[5]

  now <- Sys.time()
  version <- if (is.null(rev)) format(now, "%Y-%m-%d") else rev
  fileSuffix <- paste0("_input4MIPs_landState_RESCUE_PIK-MAgPIE-4-7-",
                       scenario, if (scenario == "") "" else "-",
                       version, "_gn_", min(years), "-", max(years))

  land <- calcOutput("LandReport", project = "RESCUE",
                     harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)
  land <- adaptYearsRESCUE(land, years)

  statesFile <- paste0("multiple-states", fileSuffix, ".nc")
  statesVariables <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr",
                       "primf", "primn", "range", "secdf", "secdn", "urban")
  write.magpie(land[, , statesVariables], statesFile, compression = compression,
               missval = missingValue, gridDefinition = gridDefinition, progress = TRUE)
  addMetadataRESCUE(statesFile, now, missingValue, resolution, compression, harmonizationPeriod)

  landManagementVariables <- c("irrig_c3ann", "crpbf_c3ann", "irrig_c3nfx", "crpbf_c3nfx",
                               "irrig_c3per", "crpbf_c3per", "crpbf2_c3per", "irrig_c4ann",
                               "crpbf_c4ann", "irrig_c4per", "crpbf_c4per", "crpbf2_c4per", "manaf")
  land <- land[, , landManagementVariables]
  nonland <- calcOutput("NonlandReport", project = "RESCUE",
                        harmonizationPeriod = harmonizationPeriod, aggregate = FALSE,
                        warnNA = FALSE) # rndwd & fulwd include NAs
  nonland <- adaptYearsRESCUE(nonland, years)
  nonlandManagementVariables <- c("fertl_c3nfx", "fertl_c3per", "fertl_c3ann", "fertl_c4ann",
                                  "fertl_c4per", "rndwd", "fulwd")
  management <- mbind(land, nonland[, , nonlandManagementVariables])
  rm(land)
  managementFile <- paste0("multiple-management", fileSuffix, ".nc")
  write.magpie(management, managementFile, compression = compression,
               missval = missingValue, gridDefinition = gridDefinition, progress = TRUE)
  rm(management)
  addMetadataRESCUE(managementFile, now, missingValue, resolution, compression, harmonizationPeriod)

  woodSources <- c("primf", "secyf", "secmf", "primn", "secnf")
  woodHarvestVariables <- c(paste0(woodSources, "_bioh"), paste0(woodSources, "_harv"))
  nonland <- nonland[, , woodHarvestVariables]

  transitions <- calcOutput("LandTransitions", project = "RESCUE",
                            harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)
  getItems(transitions, raw = TRUE, dim = 3) <- sub("\\.", "_to_", getItems(transitions, dim = 3))
  getSets(transitions, fulldim = FALSE)[3] <- "transitions"
  getYears(transitions) <- getYears(transitions, as.integer = TRUE) - 1
  transitions <- adaptYearsRESCUE(transitions, years)
  transitions <- mbind(transitions, nonland)
  rm(nonland)
  transitionsFile <- paste0("multiple-transitions", fileSuffix, ".nc")
  write.magpie(transitions, transitionsFile, compression = compression,
               missval = missingValue, gridDefinition = gridDefinition, progress = TRUE)
  rm(transitions)
  addMetadataRESCUE(transitionsFile, now, missingValue, resolution, compression, harmonizationPeriod)
}

adaptYearsRESCUE <- function(x, years) {
  x <- x[, getYears(x, as.integer = TRUE) %in% years, ]
  # account for unit "years since 1970-01-01 0:0:0" which addMetadataRESCUE sets
  x <- setYears(x, getYears(x, as.integer = TRUE) - 1970)
  return(x)
}

addMetadataRESCUE <- function(ncFile, now, missingValue, resolution, compression, harmonizationPeriod) {
  variableId <- sub("^(multiple-[^_]+).+$", "\\1", basename(ncFile))
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
  ncdf4::ncatt_put(nc, 0, "further_info_url",
                   "https://github.com/pik-piam/mrdownscale/blob/main/inst/extdata/runner/changelog-rescue.md")
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

  # added by us
  ncdf4::ncatt_put(nc, 0, "harmonization_period", paste(harmonizationPeriod, collapse = "-"))
  ncdf4::ncatt_put(nc, 0, "reference_dataset", "LUH2 v2h Release (10/14/16) from https://luh.umd.edu/data.shtml")
  ncdf4::ncatt_put(nc, 0, "harmonization_downscaling_tool", "https://github.com/pik-piam/mrdownscale")

  # time
  ncdf4::ncatt_put(nc, "time", "axis", "T")
  ncdf4::ncatt_put(nc, "time", "bounds", "bounds_time")
  ncdf4::ncatt_put(nc, "time", "calendar", "365_day")
  ncdf4::ncatt_put(nc, "time", "long_name", "time")
  ncdf4::ncatt_put(nc, "time", "realtopology", "linear")
  ncdf4::ncatt_put(nc, "time", "standard_name", "time")
  ncdf4::ncatt_put(nc, "time", "units", "years since 1970-01-01 0:0:0")

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

  # variable attributes
  luhNames <- toolGetMapping("luhNames.csv", where = "mrdownscale")
  for (varname in names(nc$var)) {
    ncdf4::ncatt_put(nc, varname, "_Fillvalue", missingValue, prec = "float")
    ncdf4::ncatt_put(nc, varname, "missing_value", missingValue, prec = "float")
    ncdf4::ncatt_put(nc, varname, "cell_methods", "time:mean")

    varnameLuhNames <- as.vector(luhNames[luhNames$name == varname, ])
    ncdf4::ncatt_put(nc, varname, "long_name", varnameLuhNames$long_name)
    ncdf4::ncatt_put(nc, varname, "standard_name", varnameLuhNames$standard_name)
    if (varnameLuhNames$standard_name_description != "") {
      ncdf4::ncatt_put(nc, varname, "standard_name_description", varnameLuhNames$standard_name_description)
    }
    if (varnameLuhNames$long_name_description != "") {
      ncdf4::ncatt_put(nc, varname, "long_name_description", varnameLuhNames$long_name_description)
    }
  }

  # add bounds
  boundsDim <- ncdf4::ncdim_def("bounds", "", 1:2, create_dimvar = FALSE)

  withr::with_options(list(warnPartialMatchDollar = FALSE), {
    nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("bounds_lon", units = "",
                                                dim = list(boundsDim, nc$dim$lon),
                                                prec = "double", compression = compression))
    nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("bounds_lat", units = "",
                                                dim = list(boundsDim, nc$dim$lat),
                                                prec = "double", compression = compression))
    nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("bounds_time", units = "",
                                                dim = list(boundsDim, nc$dim$time),
                                                prec = "integer", compression = compression))
  })

  ncdf4::ncvar_put(nc, "bounds_lon", rbind(nc$dim$lon$vals - resolution / 2,
                                           nc$dim$lon$vals + resolution / 2))
  ncdf4::ncvar_put(nc, "bounds_lat", rbind(nc$dim$lat$vals + resolution / 2,
                                           nc$dim$lat$vals - resolution / 2))
  # these time bounds were taken from the reference file
  # multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc
  ncdf4::ncvar_put(nc, "bounds_time", rbind(rep(1, nc$dim$time$len),
                                            rep(365, nc$dim$time$len)))
}
