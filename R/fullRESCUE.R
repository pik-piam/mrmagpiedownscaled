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
fullRESCUE <- function(rev = NULL, ..., scenario = "", compression = 2, interpolate = FALSE) {
  stopifnot(...length() == 0)

  now <- Sys.time()
  version <- if (is.null(rev)) format(now, "%Y-%m-%d") else rev
  fileSuffix <- paste0("_input4MIPs_landState_RESCUE_PIK-MAgPIE-4-7-",
                       scenario, if (scenario == "") "" else "-", version, "_gn_1995-2100")

  land <- calcOutput("LandReport", project = "RESCUE", aggregate = FALSE)
  nonland <- calcOutput("NonlandReport", project = "RESCUE",
                        warnNA = FALSE, # rndwd & fulwd include NAs
                        aggregate = FALSE, try = TRUE)

  toolWriteStates(land, fileSuffix = fileSuffix, now = now, compression = compression, interpolate = interpolate)
  toolWriteManagement(land, nonland, fileSuffix = fileSuffix, now = now, compression = compression,
                      interpolate = interpolate)

  transitions <- calcOutput("LandTransitions", project = "RESCUE", aggregate = FALSE, file = "transitions.mz")
  toolWriteTransitions(transitions, nonland, fileSuffix = fileSuffix, now = now,
                       compression = compression, interpolate = interpolate)
}
