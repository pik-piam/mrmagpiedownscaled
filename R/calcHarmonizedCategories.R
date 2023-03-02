calcHarmonizedCategories <- function() {
  magpie <- madrat::readSource("Magpie")
  mapping <- madrat::toolGetMapping("TODO", where = "mrmagpiedownscaled")
  stop("TODO apply mapping to magpie")
  stop("TODO consistency checks")
  stop("TODO cache = FALSE?")
  return(list(x = magpie,
              description = "MAgPIE output with LUH2 categories.",
              unit = "TODO",
              class = "SpatRaster"))
}
# TODO rename package to mrdownscale
