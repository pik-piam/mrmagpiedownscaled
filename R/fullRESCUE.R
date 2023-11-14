#' fullRESCUE
#'
#' Run the pipeline to generate harmonized and downscaled data to report for the RESCUE project.
#' Write .nc files, print full report on consistency checks and write it to report.log.
#'
#' @param rev revision number of the data. If not provided the current date will be used instead
#' @param ... reserved for future use
#' @param compression compression level of the resulting .nc files, possible values are integers from 1-9,
#' 1 = fastest, 9 = best compression
#'
#' @author Pascal Sauer, Jan Philipp Dietrich
fullRESCUE <- function(rev = NULL, ..., compression = 2) {
  stopifnot(...length() == 0)

  now <- Sys.time()
  version <- ifelse(is.null(rev), format(now, "%Y-%m-%d"), rev)
  fileSuffix <- paste0("_input4MIPs_landState_RESCUE_PIK-MAgPIE67k-", version, "_gn_1995-2100.nc")

  land <- calcOutput("LandReport", project = "RESCUE", aggregate = FALSE)
  nonland <- calcOutput("NonlandReport", project = "RESCUE", aggregate = FALSE, try = TRUE)

  toolWriteStates(land, fileSuffix = fileSuffix, now = now, compression = compression)
  toolWriteManagement(land, nonland, fileSuffix = fileSuffix, now = now, compression = compression)
}
