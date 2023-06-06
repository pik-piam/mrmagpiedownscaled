calcMagpieManagementLUH <- function() {
  # TODO flooded fraction of c3ann area: flood
  riceHist <- read.magpie(system.file("riceHistorical.mz", package = "mrdownscale"))
  # ... to get historic data as weights, then disaggregate rice_pro
  # - resolution = 67k or 59k cells
  # - project to target resolution, then disaggregate from cluster to target resolution

  # industrial roundwood fraction of wood harvest: rndwd
  # fuelwood fraction of wood harvest: fulwd
  wood <- readSource("Magpie", subtype = "woodHarvest")
}
