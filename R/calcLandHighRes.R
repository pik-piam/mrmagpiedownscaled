calcLandHighRes <- function(input = "magpie", target = "luh2", downscaling = "magpieClassic") {
  x <- calcOutput("LandHarmonized", input = input, target = target, aggregate = FALSE)
  xTarget <- calcOutput("LandTargetData", target = target, aggregate = FALSE)

  if (downscaling == "magpieClassic") {
    out <- toolDownscaleMagpieClassic(x, xTarget)
  } else {
    stop("Unsupported downscaling methods \"", downscaling, "\"")
  }

  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Downscaled and harmonized land use data"))
}
