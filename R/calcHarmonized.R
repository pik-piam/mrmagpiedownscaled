calcHarmonized <- function() {
  magpie <- readSource("Magpie") # TODO change to calcOutput("HarmonizedCategories")
  # TODO reformat magpie to fit harmonized interface

  luh <- calcOutput("LowResLUH2v2h", aggregate = FALSE)
  # TODO reformat luh to fit harmonized interface

  harmonized <- mip::harmonize(magpie, luh, finalYear = "2050", harmonizeYear = "1995", method = "ratio")
  # TODO turn harmonized dataframe into SpatVector
  return(harmonized)
}
