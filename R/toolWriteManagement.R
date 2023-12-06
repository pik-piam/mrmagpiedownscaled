#' toolWriteManagement
#'
#' Tool function to write LUH2-style management data in NetCDF format
#'
#' @param land land data the management information should be extracted from
#' @param nonland non-land data the management information should be extracted from
#' @param fileSuffix second half of the resulting file name for the output NetCDF file
#' @param now time that should be used as time stamp in metadata
#' @param compression compression level of the resulting .nc files, possible values are integers from 1-9,
#' 1 = fastest, 9 = best compression
#' @param interpolate boolean defining whether the data should be interpolated to annual values or not
#'
#' @author Pascal Sauer, Jan Philipp Dietrich
toolWriteManagement <- function(land, nonland, fileSuffix, now = Sys.time(), compression = 2, interpolate = FALSE) {
  if (inherits(nonland, "try-error") || inherits(land, "try-error")) {
    warning("Management data incomplete, management file not created!")
    return(NULL)
  }

  # missing: combf, crpbf_total, fharv_c3per, fharv_c4per, flood, lat_bounds, lon_bounds
  managementVariables <- c("fertl_c3ann", "fertl_c3nfx", "fertl_c3per", "fertl_c4ann", "fertl_c4per",
                           "irrig_c3ann", "irrig_c3nfx", "irrig_c3per", "irrig_c4ann", "irrig_c4per",
                           "crpbf_c3ann", "crpbf_c3nfx", "crpbf_c3per", "crpbf_c4ann", "crpbf_c4per",
                           "crpbf2_c3per", "crpbf2_c4per", "manaf", "rndwd", "fulwd")

  landVariables    <- intersect(getItems(land, dim = 3), managementVariables)
  nonlandVariables <- intersect(getItems(nonland, dim = 3), managementVariables)
  x <- mbind(land[, , landVariables], nonland[, , nonlandVariables])

  interpolationType <- if (interpolate) "linear" else NULL
  toolWriteNC(x, managementVariables, paste0("multiple-management", fileSuffix), now, compression,
              interpolationType = interpolationType, years = 1995:2100)
}
