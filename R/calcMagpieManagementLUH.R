calcMagpieManagementLUH <- function() {
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
