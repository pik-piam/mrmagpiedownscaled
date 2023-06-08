calcMagpieManagementLUH <- function() {
  # TODO flooded fraction of c3ann area: flood
  # flooded <- read.magpie(system.file("extdata/floodedShareRice.mz", package = "mrdownscale"))
  # floodedRaster <- magclass::as.SpatRaster(flooded)
  # floodedTargetResolution <- terra::project(floodedRaster, target)

  # land <- calcOutput("LandHighRes", input = "magpie", target = "luh2", aggregate = FALSE)
  # c3ann <- land[, , "c3ann"]
  # shareRiceC3ann <- # TODO get share of rice_pro / c3ann
  # rice <- shareRiceC3ann * c3ann
  # floodedRice <- floodedTargetResolution * rice
  # flood <- floodedRice / c3ann

  # industrial roundwood fraction of wood harvest: rndwd
  # fuelwood fraction of wood harvest: fulwd
  target <- calcOutput("LandTargetData", aggregate = FALSE)
  wood <- readSource("Magpie", subtype = "woodHarvest")
  geometry <- attr(wood, "geometry")
  wood <- wood / dimSums(wood, dim = 3)
  stopifnot(identical(getNames(wood), c("wood", "woodfuel")))
  getNames(wood) <- c("rndwd", "fulwd")
  attr(wood, "geometry") <- geometry
  wood <- as.SpatVector(wood)
  stopifnot(identical(names(wood)[1:2], c(".j", ".region")))
  # rasterize each field/column individually, then combine
  wood <- do.call(c, lapply(names(wood)[-1:-2],
                            function(name) terra::rasterize(wood, target, field = name)))

  # irrigated fraction of crop area: irrig_[c3ann,c3nfx,c3per,c4ann,c4per]
  # irrigation <- readSource("Magpie", "irrigation")

  return(list(x = wood,
              class = "SpatRaster",
              unit = "1", # TODO unit is not always 1
              description = "management variables calculated by MAgPIE in the format of LUH's management.nc"))

}
