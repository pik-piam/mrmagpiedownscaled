#' calcNonlandHighRes
#'
#' This function performs the downscaling: It calculates a high resolution dataset
#' from the low resolution input dataset and the high resolution target dataset
#' using the given downscaling method.
#'
#' @param input name of an input dataset, currently only "magpie"
#' @param target name of a target dataset, currently only "luh2"
#' @param downscaling name of downscaling method, currently only "magpieClassic"
#' @return downscaled nonland data
#' @author Jan Philipp Dietrich
calcNonlandHighRes <- function(input = "magpie", target = "luh2", downscaling = "magpieClassic") {
  x <- calcOutput("NonlandHarmonized", input = input, target = target, aggregate = FALSE)
  xTarget <- calcOutput("NonlandTargetData", target = target, aggregate = FALSE)

  if (downscaling == "magpieClassic") {
    # suppressing warning: Total stock is not constant over time.
    # This is not a problem because we're not downscaling land use data.
    suppressWarnings({
      out <- toolDownscaleMagpieClassic(x, xTarget)
    })
  } else {
    stop("Unsupported downscaling method \"", downscaling, "\"")
  }

  return(list(x = out,
              isocountries = FALSE,
              unit = "rndwd, fulwd: 1, *_fertilizer: kg yr-1",
              min = 0,
              description = "Downscaled nonland data"))
}
