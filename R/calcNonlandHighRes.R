calcNonlandHighRes <- function(input = "magpie", target = "luh2", downscaling = "magpieClassic") {
  x <- calcOutput("NonlandHarmonized", input = input, target = target, aggregate = FALSE)
  xTarget <- calcOutput("NonlandTargetData", target = target, aggregate = FALSE)

  if (downscaling == "magpieClassic") {
    # TODO warning: Total stock is not constant over time.
    # This is not a problem because we're not downscaling land use data.
    out <- toolDownscaleMagpieClassic(x, xTarget)
  } else {
    stop("Unsupported downscaling method \"", downscaling, "\"")
  }

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Downscaled nonland data"))
}
