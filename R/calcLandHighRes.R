calcLandHighRes <- function(input = "magpie", target = "luh2", downscaling = "magpieClassic") {
  x <- toolAddCheckReport(calcOutput("LandHarmonized", input = input, target = target, aggregate = FALSE))
  xTarget <- toolAddCheckReport(calcOutput("LandTargetData", target = target, aggregate = FALSE))

  if (downscaling == "magpieClassic") {
    out <- toolDownscaleMagpieClassic(x, xTarget)
  } else {
    stop("Unsupported downscaling method \"", downscaling, "\"")
  }
  attr(out, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Downscaled land use data"))
}
