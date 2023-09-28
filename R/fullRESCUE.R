#' fullRESCUE
#'
#' Run the pipeline to generate harmonized and downscaled data to report for the RESCUE project.
#' Data is calculated for every 5th year, fill years inbetween with linear interpolation.
#' Write .nc files, print full report on consistency checks and write it to report.log.
#'
#' @param rev revision number of the data
#'
#' @author Pascal Sauer
fullRESCUE <- function(rev = 2) {
  land <- calcOutput("LandReport", project = "RESCUE", aggregate = FALSE)

  statesVariables <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr",
                       "primf", "primn", "range", "secdf", "secdn", "urban")
  stopifnot(statesVariables %in% getNames(land))
  for (i in seq_along(statesVariables)) {
    statesVariable <- statesVariables[i]
    message(i, "/", length(statesVariables), " writing ", statesVariable, ".nc")
    terra::writeCDF(toolSpatRasterToDataset(as.SpatRaster(land[, , statesVariable])),
                    paste0(statesVariable, ".nc"), overwrite = TRUE)
  }
  states <- terra::sds(paste0(statesVariables, ".nc"))
  statesFile <- paste0("multiple-states_input4MIPs_landState_ScenarioMIP_PIK-", rev,
                       "_gn_1995-2100.nc")
  terra::writeCDF(states, statesFile, overwrite = TRUE)
  unlink(paste0(statesVariables, ".nc"))

  # missing: combf, rndwd, fulwd, crpbf_total, fharv_c3per, fharv_c4per, flood, lat_bounds, lon_bounds,
  managementVariables <- c("fertl_c3ann", "fertl_c3nfx", "fertl_c3per", "fertl_c4ann", "fertl_c4per",
                           "irrig_c3ann", "irrig_c3nfx", "irrig_c3per", "irrig_c4ann", "irrig_c4per",
                           "crpbf_c3ann", "crpbf_c3nfx", "crpbf_c3per", "crpbf_c4ann", "crpbf_c4per",
                           "crpbf2_c3per", "crpbf2_c4per", "manaf")
  nonland <- calcOutput("NonlandReport", project = "RESCUE", aggregate = FALSE)
  for (i in seq_along(managementVariables)) {
    managementVariable <- managementVariables[i]
    message(i, "/", length(managementVariables), " writing ", managementVariable, ".nc")
    if (managementVariable %in% getNames(land)) {
      x <- land[, , managementVariable]
    } else {
      x <- nonland[, , managementVariable]
    }
    xRaster <- terra::extend(as.SpatRaster(x), terra::ext(-180, 180, -55.5, 83.25))
    terra::writeCDF(toolSpatRasterToDataset(xRaster), paste0(managementVariable, ".nc"), overwrite = TRUE)
  }
  management <- terra::sds(paste0(managementVariables, ".nc"))
  managementFile <- paste0("multiple-management_input4MIPs_landState_ScenarioMIP_PIK-", rev,
                           "_gn_1995-2100.nc")
  terra::writeCDF(management, managementFile, overwrite = TRUE)
  unlink(paste0(managementVariables, ".nc"))
}
