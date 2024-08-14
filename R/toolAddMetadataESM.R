#' toolAddMetadataESM
#'
#' Add metdata to ESM compatible nc output files
#'
#' @param ncFile file name of the respective nc file
#' @param cfg list of settings containing the elements "revision", "missingValue",
#' "resolution", "compression" and "harmonizationPeriod"
#' @author Pascal Sauer, Jan Philipp Dietrich

toolAddMetadataESM <- function(ncFile, cfg) {
  variableId <- sub("^(multiple-[^_]+).+$", "\\1", basename(ncFile))
  stopifnot(variableId %in% c("multiple-states", "multiple-management", "multiple-transitions"))
  nc <- ncdf4::nc_open(ncFile, write = TRUE)
  withr::defer({
    ncdf4::nc_close(nc)
  })
  # global
  dateTime <- strftime(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ncdf4::ncatt_put(nc, 0, "activity_id", "RESCUE/OptimESM")
  ncdf4::ncatt_put(nc, 0, "contact", "pascal.sauer@pik-potsdam.de, dietrich@pik-potsdam.de")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.6")
  ncdf4::ncatt_put(nc, 0, "creation_date", dateTime)
  ncdf4::ncatt_put(nc, 0, "data_structure", "grid")
  ncdf4::ncatt_put(nc, 0, "dataset_category", "landState")
  ncdf4::ncatt_put(nc, 0, "dataset_version_number", as.character(cfg$revision))
  ncdf4::ncatt_put(nc, 0, "date", dateTime)
  ncdf4::ncatt_put(nc, 0, "frequency", "yr")
  ncdf4::ncatt_put(nc, 0, "further_info_url", "https://github.com/pik-piam/mrdownscale/blob/main/runner/changelog.md")
  ncdf4::ncatt_put(nc, 0, "grid_label", "gn")
  ncdf4::ncatt_put(nc, 0, "host", "Potsdam Institute for Climate Impact Research")
  ncdf4::ncatt_put(nc, 0, "institution_id", "PIK")
  ncdf4::ncatt_put(nc, 0, "institution", "Potsdam Institute for Climate Impact Research")
  ncdf4::ncatt_put(nc, 0, "license", "CC BY 4.0")
  ncdf4::ncatt_put(nc, 0, "nominal_resolution", "50 km")
  ncdf4::ncatt_put(nc, 0, "realm", "land")
  ncdf4::ncatt_put(nc, 0, "references",
                   "https://github.com/pik-piam/mrdownscale and https://rescue-climate.eu/ and https://optimesm-he.eu/")
  ncdf4::ncatt_put(nc, 0, "source_id", sub(paste0("^", variableId, "_"), "",
                                           sub("\\.nc$", "",
                                               basename(ncFile))))
  ncdf4::ncatt_put(nc, 0, "target_mip", "RESCUE/OptimESM")
  ncdf4::ncatt_put(nc, 0, "title", "MAgPIE Land-Use Data Harmonized and Downscaled using LUH2 v2h as reference")
  ncdf4::ncatt_put(nc, 0, "variable_id", variableId)

  # added by us
  ncdf4::ncatt_put(nc, 0, "harmonization_period", paste(cfg$harmonizationPeriod, collapse = "-"))
  ncdf4::ncatt_put(nc, 0, "harmonization_downscaling_tool", "https://github.com/pik-piam/mrdownscale")
  ncdf4::ncatt_put(nc, 0, "reference_dataset", "LUH2 v2h Release (10/14/16) from https://luh.umd.edu/data.shtml")
  ncdf4::ncatt_put(nc, 0, "source_version", as.character(cfg$revision))

  # time
  ncdf4::ncatt_put(nc, "time", "axis", "T")
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
  ncdf4::ncatt_put(nc, "lat", "long_name", "latitude")
  ncdf4::ncatt_put(nc, "lat", "standard_name", "latitude")
  ncdf4::ncatt_put(nc, "lat", "axis", "Y")
  ncdf4::ncatt_put(nc, "lat", "bounds", "bounds_lat")

  # variable attributes
  luhNames <- toolGetMapping("luhNames.csv", where = "mrdownscale")
  for (varname in names(nc$var)) {
    ncdf4::ncatt_put(nc, varname, "_Fillvalue", cfg$missingValue, prec = "float")
    ncdf4::ncatt_put(nc, varname, "missing_value", cfg$missingValue, prec = "float")
    ncdf4::ncatt_put(nc, varname, "cell_methods", "time:mean")

    varnameLuhNames <- as.vector(luhNames[luhNames$name == varname, ])
    ncdf4::ncatt_put(nc, varname, "units", varnameLuhNames$unit)
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
                                                prec = "double", compression = cfg$compression))
    nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("bounds_lat", units = "",
                                                dim = list(boundsDim, nc$dim$lat),
                                                prec = "double", compression = cfg$compression))
  })

  ncdf4::ncvar_put(nc, "bounds_lon", rbind(nc$dim$lon$vals - cfg$resolution / 2,
                                           nc$dim$lon$vals + cfg$resolution / 2))
  ncdf4::ncvar_put(nc, "bounds_lat", rbind(nc$dim$lat$vals + cfg$resolution / 2,
                                           nc$dim$lat$vals - cfg$resolution / 2))
}
