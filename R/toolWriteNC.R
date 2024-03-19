#' toolWriteNC
#'
#' Tool function to write LUH2-style NetCDF files
#'
#' @param x magpie object to be written to file
#' @param variables layers that should be selected from \code{x}
#' @param fileName file name that should be used for the resulting nc file
#' @param now time that should be used as time stamp in metadata
#' @param compression compression level of the resulting .nc files, possible values are integers from 1-9,
#' 1 = fastest, 9 = best compression
#' @param interpolationType Either NULL (no interpolation), "linear" (linear interpolation), or "constant"
#' (last available year is being repeated)
#' @param years years to be returned. Will only be considered if a interpolation type is selected
#'
#' @author Pascal Sauer, Jan Philipp Dietrich

toolWriteNC <- function(x, variables, fileName, now = Sys.time(), compression = 2,
                        interpolationType = NULL, years = 1995:2100) {
  if (!grepl("\\.nc$", fileName)) fileName <- paste0(fileName, ".nc")

  missingValue <- 1e20

  .convertExtendUnitWrite <- function(x, fileName, compression, missingValue, interpolationType, years) {
    if (!is.null(interpolationType)) {
      if (interpolationType == "linear") {
        x <- time_interpolate(x, interpolated_year = years)
      } else if (interpolationType == "constant") {
        .constInterpolate <- function(x, years) {
          .vec <- function(years, y) {
            i <- match(sort(years), y)
            k <- NULL
            for (j in seq_along(i)) {
              if (!is.na(i[j])) {
                k <- i[j]
              } else {
                i[j] <- k
              }
            }
            return(y[i])
          }
          t <- .vec(years, getYears(x, as.integer = TRUE))
          return(setYears(x[, t, ], years))
        }
        x <- .constInterpolate(x, years)
      } else {
        stop("Unsupported interpolation type")
      }
    }

    browser() # TODO check comment is not lost after extend
    x <- magclass::extend(x,
                          xRange = c(-179.875, 179.875),
                          yRange = c(89.875, -89.875),
                          res = 0.25)
    write.magpie(x, fileName, missval = missingValue, compression = compression)
  }

  # set terra::units on a SpatRasterDataset using the units specified in individual SpatRasters
  .setUnits <- function(x) {
    terra::units(x) <- vapply(x, function(spatRaster) {
      stopifnot(length(unique(terra::units(spatRaster))) == 1) # assert each year-layer has the same unit
      return(terra::units(spatRaster)[1])
    }, character(1))
    return(x)
  }

  .addMetadata <- function(ncFile, now, missingValue) {
    # try to remove crs variable, which is not needed by ESMs
    if (Sys.which("ncks") != "") {
      system2("ncks", c("-C", "-O", "-x", "-v", "crs", ncFile, paste0(ncFile, "-no-crs")))
      file.rename(paste0(ncFile, "-no-crs"), ncFile)
    } else {
      message("Cannot remove crs variable, ncks not found")
    }

    nc <- ncdf4::nc_open(ncFile, write = TRUE)
    withr::defer({
      ncdf4::nc_close(nc)
    })
    # global
    ncdf4::ncatt_put(nc, 0, "activity_id", "RESCUE")
    ncdf4::ncatt_put(nc, 0, "contact", "pascal.sauer@pik-potsdam.de, dietrich@pik-potsdam.de")
    ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.6")
    dateTime <- strftime(now, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ncdf4::ncatt_put(nc, 0, "creation_date", dateTime)
    ncdf4::ncatt_put(nc, 0, "date", dateTime)
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
    # TODO need to rename to lon/lat -> probably need to switch to ncdf4
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
      if (varname %in% luhNames$name) {
        ncdf4::ncatt_put(nc, varname, "long_name", luhNames$long_name[luhNames$name == varname])
        ncdf4::ncatt_put(nc, varname, "standard_name", luhNames$standard_name[luhNames$name == varname])
      } else {
        warning(varname, " not found in luhNames.csv")
      }
    }
  }

  stopifnot(variables %in% getNames(x))
  for (i in seq_along(variables)) {
    variable <- variables[i]
    message(i, "/", length(variables), " writing ", variable, ".nc")
    .convertExtendUnitWrite(x[, , variable], paste0(variable, ".nc"), compression = NA, missingValue = missingValue,
                            interpolationType = interpolationType, years = years)
  }
  y <- terra::sds(paste0(variables, ".nc"))
  y <- .setUnits(y)
  terra::writeCDF(y, fileName, overwrite = TRUE, missval = missingValue, compression = compression)
  unlink(paste0(variables, ".nc"))
  .addMetadata(fileName, now = now, missingValue = missingValue)
}
