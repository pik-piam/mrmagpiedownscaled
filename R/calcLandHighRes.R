calcLandHighRes <- function(input = "magpie", target = "luh2", downscaling = "magpieClassic") {
  x <- calcOutput("LandHarmonized", input = input, target = target, aggregate = FALSE)

  # get target data
  if (target == "luh2") {
    xTarget <- madrat::readSource("LUH2v2h")
  } else {
    stop("Unsupported output type \"", target, "\"")
  }

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
