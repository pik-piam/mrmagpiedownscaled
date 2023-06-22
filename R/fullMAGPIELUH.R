fullMAGPIELUH <- function() {
  options(toolCheck = NULL)
  states <- toolAddCheckReport(calcOutput("MagpieStatesLUH", aggregate = FALSE))
  write.magpie(states, "magpie_luh_states.nc")

  management <- toolAddCheckReport(calcOutput("MagpieManagementLUH", aggregate = FALSE))
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(management)))
  varnames <- unique(sub("^y[0-9]{4}\\.\\.", "", names(management)))
  datasets <- lapply(varnames, function(varname) management[paste0("\\.\\.", varname, "$")])
  management <- terra::sds(datasets)
  names(management) <- varnames
  terra::writeCDF(management, "magpie_luh_management.nc")
  report <- toolCheckReport(unlist = TRUE)
  cat(report, sep="\n")
  writeLines(report, "report.log")
}
