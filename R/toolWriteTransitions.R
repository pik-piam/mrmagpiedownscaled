#' toolWriteTransitions
#'
#' Tool function to write LUH2-style land transition data in NetCDF format
#'
#' @param trans land transition data the information should be extracted from
#' @param fileSuffix second half of the resulting file name for the output NetCDF file
#' @param now time that should be used as time stamp in metadata
#' @param compression compression level of the resulting .nc files, possible values are integers from 1-9,
#' 1 = fastest, 9 = best compression
#'
#' @author Jan Philipp Dietrich
toolWriteTransitions <- function(trans, fileSuffix, now = Sys.time(), compression = 2) {
  getItems(trans, raw = TRUE, dim = 3) <- sub("\\.", "_to_", getItems(trans, dim = 3))
  getSets(trans, fulldim = FALSE)[3] <- "transitions"
  toolWriteNC(trans, getItems(trans, dim = 3), paste0("multiple-transitions", fileSuffix), now, compression)
}
