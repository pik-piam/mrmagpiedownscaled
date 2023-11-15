#' calcLandTransitions
#'
#' Ex-Post estimation of land use transitions based on land use state
#' information
#'
#' @param project name of the project, currently only "RESCUE"
#' @param gross either boolean or a magpie object containing bidirectional
#' transition shares relative to the area of the involved land pools (transition
#' divided by the area of the land pool in the "from" sub dimension). If set to
#' FALSE only net transitions will be returned. If set to TRUE an internal
#' gross transition estimate based on average gross transitions in LUH2 in the
#' period from 1995 to 2015 will be used.
#' @return land use transition data
#' @author Jan Philipp Dietrich

calcLandTransitions <- function(project = "RESCUE", gross = TRUE) {
  if (project != "RESCUE") stop("Can only report for project = 'RESCUE'")

  land <- calcOutput("LandReport", project = "RESCUE", aggregate = FALSE)
  out <- NULL
  l <- 2
  for (i in seq(1, nyears(land), l)) {
    out <- mbind(out, toolTransitionsBasic(land[, i:min(nyears(land), i + l), ], gross = gross))
  }

  return(list(x = out,
              isocountries = FALSE,
              unit = "1",
              min = 0,
              max = 1.0001,
              description = paste("MAgPIE land use transition data estimated from downscaled land use ",
                                  "state information. Unit is share of cell area.")))
}
