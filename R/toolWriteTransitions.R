#' toolWriteTransitions
#'
#' Tool function to write LUH2-style land transition data in NetCDF format
#'
#' @param transitions land transition data the information should be extracted from
#' @param fileSuffix second half of the resulting file name for the output NetCDF file
#' @param now time that should be used as time stamp in metadata
#' @param compression compression level of the resulting .nc files, possible values are integers from 1-9,
#' 1 = fastest, 9 = best compression
#' @param interpolate boolean defining whether the data should be interpolated to annual values or not
#'
#' @author Jan Philipp Dietrich
toolWriteTransitions <- function(transitions, nonland, fileSuffix, now = Sys.time(),
                                 compression = 2, interpolate = FALSE) {
  getItems(transitions, raw = TRUE, dim = 3) <- sub("\\.", "_to_", getItems(transitions, dim = 3))
  getSets(transitions, fulldim = FALSE)[3] <- "transitions"
  getYears(transitions) <- getYears(transitions, as.integer = TRUE) - 1

  woodSources <- c("primf", "secyf", "secmf", "primn", "secnf")
  woodHarvestVariables <- c(paste0(woodSources, "_bioh"), paste0(woodSources, "_harv"))
  woodHarvest <- nonland[, , woodHarvestVariables]

  out <- mbind(transitions, woodHarvest)

  interpolationType <- if (interpolate) "constant" else NULL
  toolWriteNC(out, getItems(out, dim = 3),
              paste0("multiple-transitions", fileSuffix), now, compression,
              interpolationType = interpolationType, years = 1995:2100)
}
