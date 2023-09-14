#' fullRESCUE
#'
#' Run the pipeline to generate harmonized and downscaled data to report for the RESCUE project.
#' Data is calculated for every 5th year, fill years inbetween with linear interpolation.
#' Write .nc files, print full report on consistency checks and write it to report.log.
#'
#' @author Pascal Führlich
fullRESCUE <- function(rev = 1) {
  x <- calcOutput("LandReport", project = "RESCUE", aggregate = FALSE)

  statesVariables <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr",
                       "primf", "primn", "range", "secdf", "secdn", "urban")
  stopifnot(statesVariables %in% getNames(x))
  for (i in seq_along(statesVariables)) {
    statesVariable <- statesVariables[i]
    message(i, "/", length(statesVariables), " writing ", statesVariable, ".nc")
    terra::writeCDF(toolSpatRasterToDataset(as.SpatRaster(x[, , statesVariable])),
                    paste0(statesVariable, ".nc"), overwrite = TRUE)
  }
  states <- terra::sds(paste0(statesVariables, ".nc"))
  terra::writeCDF(states, paste0("multiple-states_input4MIPs_landState_ScenarioMIP_PIK-", rev,
                                 "_gn_1995-2100.nc"), overwrite = TRUE)
  unlink(paste0(statesVariables, ".nc"))

  # TODO write management.nc
  # TODO add metadata to nc files
}
