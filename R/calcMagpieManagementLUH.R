calcMagpieManagementLUH <- function() {
  mag <- readSource("Magpie")
  clusterGeometry <- attr(mag, "geometry")
  target <- calcOutput("LandTargetData", aggregate = FALSE)

  # industrial roundwood fraction of wood harvest: rndwd
  # fuelwood fraction of wood harvest: fulwd
  wood <- readSource("Magpie", subtype = "woodHarvest")
  wood <- wood / dimSums(wood, dim = 3)
  stopifnot(min(wood, na.rm = TRUE) >= 0,
            max(wood, na.rm = TRUE) < 1.0001,
            identical(getNames(wood), c("wood", "woodfuel")))
  getNames(wood) <- c("rndwd", "fulwd")
  wood <- toolRasterize(wood, clusterGeometry, target)

  # biofuel area fraction: crpbf_[c3ann,c3nfx,c3per,c4ann,c4per]
  # counting only second generation biofuel here, so reporting only crpbf_[c3per,c4per]
  # - dimSums begr & betr / dimSums c3per c4per
  mappedMag <- calcOutput("LandHarmonizedCategories", input = "magpie", target = "luh2", aggregate = FALSE)

  betr <- mag[, , "betr"]
  c3per <- mappedMag[, , "c3per"]
  crpbf_c3per <- collapseDim(betr, 3) / c3per
  getNames(crpbf_c3per) <- "crpbf_c3per"
  stopifnot(min(crpbf_c3per, na.rm = TRUE) >= 0,
            max(crpbf_c3per, na.rm = TRUE) < 1.0001)
  crpbf_c3per <- toolRasterize(crpbf_c3per, clusterGeometry, target)

  begr <- mag[, , "begr"]
  c4per <- mappedMag[, , "c4per"]
  crpbf_c4per <- collapseDim(begr, 3) / c4per
  getNames(crpbf_c4per) <- "crpbf_c4per"
  stopifnot(min(crpbf_c4per, na.rm = TRUE) >= 0,
            max(crpbf_c4per, na.rm = TRUE) < 1.0001)
  crpbf_c4per <- toolRasterize(crpbf_c4per, clusterGeometry, target)

  # irrigated fraction of crop area: irrig_[c3ann,c3nfx,c3per,c4ann,c4per]
  # irrigation <- readSource("Magpie", "irrigation")

  # TODO interpolate years or are 5 year steps ok?
  return(list(x = c(wood, crpbf_c3per, crpbf_c4per),
              class = "SpatRaster",
              unit = "1", # TODO check everything is actually shares
              description = "management variables calculated by MAgPIE in the format of LUH's management.nc"))

}

toolRasterize <- function(x, geometry, targetRaster) {
  nSpatialDims <- sum(grepl("^d1\\.", names(getSets(x))))
  attr(x, "geometry") <- geometry
  x <- magclass::as.SpatVector(x)
  # rasterize each field/column individually, then combine
  x <- do.call(c, lapply(names(x)[-1:-nSpatialDims],
                         function(name) terra::rasterize(x, targetRaster, field = name)))
  return(x)
}
