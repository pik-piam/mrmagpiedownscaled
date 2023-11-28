#' toolWriteStates
#'
#' Tool function to write LUH2-style states data in NetCDF format
#'
#' @param land land data the information should be extracted from
#' @param fileSuffix second half of the resulting file name for the output NetCDF file
#' @param now time that should be used as time stamp in metadata
#' @param compression compression level of the resulting .nc files, possible values are integers from 1-9,
#' 1 = fastest, 9 = best compression
#' @param interpolate boolean defining whether the data should be interpolated to annual values or not
#'
#' @author Pascal Sauer, Jan Philipp Dietrich
toolWriteStates <- function(land, fileSuffix, now = Sys.time(), compression = 2, interpolate = FALSE) {
  statesVariables <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr",
                       "primf", "primn", "range", "secdf", "secdn", "urban")
  if (interpolate) {
    interpolationType <- "linear"
  } else {
    interpolationType <- NULL
  }
  toolWriteNC(land, statesVariables, paste0("multiple-states", fileSuffix), now, compression,
              interpolationType = interpolationType, years = 1995:2100)
}
