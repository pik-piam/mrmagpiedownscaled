#' calcLandHighRes
#'
#' This function performs the downscaling: It calculates a high resolution dataset
#' from the low resolution input dataset and the high resolution target dataset
#' using the given downscaling method.
#'
#' @param input name of an input dataset, currently only "magpie"
#' @param target name of a target dataset, currently only "luh2"
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param downscaling name of downscaling method, currently only "magpieClassic"
#' @return downscaled land use data
#' @author Jan Philipp Dietrich
calcLandHighRes <- function(input = "magpie", target = "luh2mod",
                            harmonizationPeriod = c(2015, 2050), downscaling = "magpieClassic") {
  x <- calcOutput("LandHarmonized", input = input, target = target,
                  harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)
  xTarget <- calcOutput("LandTarget", target = target, aggregate = FALSE)

  if (downscaling == "magpieClassic") {
    out <- toolDownscaleMagpieClassic(x, xTarget)
  } else {
    stop("Unsupported downscaling method \"", downscaling, "\"")
  }
  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Downscaled land use data"))
}
