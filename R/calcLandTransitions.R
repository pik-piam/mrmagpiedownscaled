#' calcLandTransitions
#'
#' Ex-Post estimation of land use transitions based on land use state
#' information
#'
#' @param project name of the project, currently only "RESCUE"
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param gross either boolean or a magpie object containing bidirectional
#' transition shares relative to the area of the involved land pools (transition
#' divided by the area of the land pool in the "from" sub dimension). If set to
#' FALSE only net transitions will be returned. If set to TRUE an internal
#' gross transition estimate based on average gross transitions in LUH2 in the
#' period from 1995 to 2015 will be used.
#' @return land use transition data
#' @author Jan Philipp Dietrich, Pascal Sauer
calcLandTransitions <- function(project = "RESCUE", harmonizationPeriod = c(2015, 2050), gross = TRUE) {
  if (project != "RESCUE") stop("Can only report for project = 'RESCUE'")

  land <- calcOutput("LandReport", project = "RESCUE",
                     harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)
  land <- land[, , grep("(_|manaf)", getItems(land, dim = 3), invert = TRUE, value = TRUE)]

  # add extra year as copy of last year to get gross transitions (net zero) for 2100 and after
  lastYear <- max(getYears(land, as.integer = TRUE))
  land <- mbind(land, setYears(land[, lastYear, ], lastYear + 1))
  yearsLand <- getYears(land, as.integer = TRUE)

  l <- 2
  sequence <- seq(1, nyears(land) - 1, l)
  tempfolder <- withr::local_tempdir()
  for (i in sequence) {
    message("Compute ", getYears(land)[i], " to ", getYears(land)[min(nyears(land), i + l)])
    transition <- toolTransitionsBasic(land[, i:min(nyears(land), i + l), ], gross = gross)

    # check if the calculated transition is consistent with the states (land)
    for (year in getYears(transition, as.integer = TRUE)) {
      yearA <- year - 1
      yearB <- yearsLand[yearsLand > yearA][1]
      for (category in getItems(land, 3)) {
        dif <- land[, yearB, category] - land[, yearA, category]
        totalAdd <- dimSums(transition[, year, endsWith(getItems(transition, 3), category)], 3)
        totalSubt <- dimSums(transition[, year, startsWith(getItems(transition, 3), category)], 3)

        deviation <- (yearB - yearA) * (totalAdd - totalSubt) - dif[, , category]
        if (any(abs(deviation) > 1e-6)) {
          warning(yearA, "-", yearB, " ", category)
          print(summary(deviation))
        }
      }
    }

    write.magpie(transition, paste0(tempfolder, "/", i, ".mz"))
  }

  # allocate enough memory for everything instead of mbinding to reduce memory requirement
  message("allocating memory...")
  coords <- getItems(land, dim = 1)
  years <- utils::head(getYears(land, as.integer = TRUE), -1) + 1
  rm(land)
  variables <- getItems(read.magpie(paste0(tempfolder, "/", sequence[1], ".mz")), dim = 3)
  out <- list(x = new.magpie(coords, years, variables, fill = NA_real_,
                             sets = c("x", "y", "year", "from", "to")),
              isocountries = FALSE,
              unit = "1",
              min = 0,
              max = 1.0001,
              description = paste("MAgPIE land use transition data estimated from downscaled land use ",
                                  "state information. Unit is share of cell area."))

  for (j in seq_along(sequence)) {
    i <- sequence[j]
    message(j, "/", length(sequence), " combining...")
    y <- years[i:min(length(years), i + l - 1)]
    out$x[, y, ] <- read.magpie(paste0(tempfolder, "/", i, ".mz"))
  }
  message("combining done, returning")

  return(out)
}
