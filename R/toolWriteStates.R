#' toolWriteStates
#'
#' Tool function to write LUH2-style states data in NetCDF format
#'
#' @param land land data the information should be extracted from
#' @param fileSuffix second half of the resulting file name for the output NetCDF file
#' @param now time that should be used as time stamp in metadata
#' @param compression compression level of the resulting .nc files, possible values are integers from 1-9,
#' 1 = fastest, 9 = best compression
#'
#' @author Pascal Sauer, Jan Philipp Dietrich
toolWriteStates <- function(land, fileSuffix, now = Sys.time(), compression = 2) {
  statesVariables <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr",
                       "primf", "primn", "range", "secdf", "secdn", "urban")
  toolWriteNC(land, statesVariables, paste0("multiple-states", fileSuffix), now, compression)
}
