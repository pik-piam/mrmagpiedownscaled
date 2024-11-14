#' calcLandHighRes
#'
#' This function performs the downscaling: It calculates a high resolution dataset
#' from the low resolution input dataset and the high resolution target dataset
#' using the given downscaling method.
#'
#' @param input name of an input dataset
#' @param target name of a target dataset
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param downscaling name of downscaling method, currently only "magpieClassic"
#' @return downscaled land use data
#' @author Jan Philipp Dietrich
calcLandHighRes <- function(input, target, harmonizationPeriod, downscaling = "magpieClassic") {
  x <- calcOutput("LandHarmonized", input = input, target = target,
                  harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)

  xTarget <- calcOutput("LandTarget", target = target, aggregate = FALSE)
  stopifnot(harmonizationPeriod[1] %in% terra::time(xTarget))
  xTarget <- as.magpie(xTarget[[terra::time(xTarget) == harmonizationPeriod[1]]])

  mapping <- calcOutput("ResolutionMapping", input = input, target = target, aggregate = FALSE)

  if (downscaling == "magpieClassic") {
    out <- toolDownscaleMagpieClassic(x[, getYears(x, as.integer = TRUE) >= harmonizationPeriod[1], ],
                                      xTarget, mapping)
  } else {
    stop("Unsupported downscaling method \"", downscaling, "\"")
  }

  out <- toolPrimFix(out, "primf", "secdf", warnThreshold = 100)
  if ("primn" %in% getItems(out, 3)) {
    out <- toolPrimFix(out, "primn", "secdn", warnThreshold = 100)
  }

  toolExpectTrue(identical(unname(getSets(out)), c("x", "y", "year", "data")),
                 "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(x, dim = 3)),
                 "Land categories remain unchanged")
  toolExpectLessDiff(out[, harmonizationPeriod[1], ], xTarget, 10^-5,
                     paste("In", harmonizationPeriod[1], "output equals target"))
  toolExpectTrue(all(out >= 0), "All values are >= 0")

  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-5,
                     "Total land area per cell in output stays constant over time")

  globalSumIn <- dimSums(x[, getYears(out), ], dim = 1)
  globalSumOut <- dimSums(out, dim = 1)
  toolExpectLessDiff(dimSums(globalSumIn, 3), dimSums(globalSumOut, 3), 10^-5,
                     "Total global land area remains unchanged")
  toolExpectLessDiff(globalSumIn, globalSumOut, 10^-5,
                     "Global area of each land type remains unchanged")
  toolPrimExpansionCheck(out)

  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Downscaled land use data"))
}
