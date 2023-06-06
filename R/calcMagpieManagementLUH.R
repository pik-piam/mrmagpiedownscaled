calcMagpieManagementLUH <- function() {
  # TODO flooded fraction of c3ann area: flood
  flooded <- read.magpie(system.file("extdata/floodedShareRice.mz", package = "mrdownscale"))
  floodedRaster <- magclass::as.SpatRaster(flooded)
  target <- calcOutput("LandTargetData", aggregate = FALSE)
  floodedTargetResolution <- terra::project(floodedRaster, target)

  rice <- readSource("Magpie")[, , "rice_pro"]
  commonYears <- intersect(getYears(shareFloodedRice, TRUE), getYears(rice, TRUE))

  # - project to target resolution, then disaggregate from cluster to target resolution

  # industrial roundwood fraction of wood harvest: rndwd
  # fuelwood fraction of wood harvest: fulwd
  wood <- readSource("Magpie", subtype = "woodHarvest")
}
