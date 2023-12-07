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
#' @author Jan Philipp Dietrich, Pascal Sauer
calcLandTransitions <- function(project = "RESCUE", gross = TRUE) {
  if (project != "RESCUE") stop("Can only report for project = 'RESCUE'")

  land <- calcOutput("LandReport", project = "RESCUE", aggregate = FALSE)
  land <- land[, , grep("(_|manaf)", getItems(land, dim = 3), invert = TRUE, value = TRUE)]

  # add extra year as copy of last year to get gross transitions (net zero) for 2100 and after
  lastYear <- max(getYears(land, as.integer = TRUE))
  land <- mbind(land, setYears(land[, lastYear, ], lastYear + 1))

  l <- 2
  sequence <- seq(1, nyears(land) - 1, l)
  tempfolder <- withr::local_tempdir()
  for (i in sequence) {
    message("Compute ", getYears(land)[i], " to ", getYears(land)[min(nyears(land), i + l)])
    write.magpie(toolTransitionsBasic(land[, i:min(nyears(land), i + l), ], gross = gross),
                 paste0(tempfolder, "/", i, ".mz"))
  }

  # allocate enough memory for everything instead of mbinding to avoid memory problems
  message("allocating memory...")
  coords <- getItems(land, dim = 1)
  years <- utils::head(getYears(land, as.integer = TRUE), -1) + 1
  variables <- getItems(read.magpie(paste0(tempfolder, "/", sequence[1], ".mz")), dim = 3)
  out <- new.magpie(coords, years, variables, fill = NA_real_)

  for (j in seq_along(sequence)) {
    i <- sequence[j]
    message(j, "/", length(sequence), " combining...")
    y <- years[i:min(length(years), i + l - 1)]
    out[, y, ] <- read.magpie(paste0(tempfolder, "/", i, ".mz"))
  }
  message("combining done, returning")

  return(list(x = out,
              isocountries = FALSE,
              unit = "1",
              min = 0,
              max = 1.0001,
              description = paste("MAgPIE land use transition data estimated from downscaled land use ",
                                  "state information. Unit is share of cell area.")))
}
