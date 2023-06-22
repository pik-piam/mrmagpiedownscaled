calcMagpieManagementLUH <- function() {
  .fillAndRasterize <- function(x, geometry, targetRaster, fillYears = FALSE) {
    # TODO need to interpolate years? this function takes about 6 times longer when interpolating
    if (fillYears) {
      years <- getYears(x, as.integer = TRUE)
      x <- toolFillYears(x, min(years):max(years))
    }

    nSpatialDims <- sum(grepl("^d1\\.", names(getSets(x))))
    attr(x, "geometry") <- geometry
    x <- magclass::as.SpatVector(x)
    # rasterize each field/column individually, then combine
    x <- do.call(c, lapply(names(x)[-1:-nSpatialDims],
                          function(name) terra::rasterize(x, targetRaster, field = name)))
    return(x)
  }

  mag <- readSource("Magpie")
  clusterGeometry <- attr(mag, "geometry")
  target <- toolAddCheckReport(calcOutput("LandTargetData", aggregate = FALSE))

  # industrial roundwood fraction of wood harvest: rndwd
  # fuelwood fraction of wood harvest: fulwd
  wood <- readSource("Magpie", subtype = "woodHarvest")
  wood <- wood / dimSums(wood, dim = 3)
  stopifnot(min(wood, na.rm = TRUE) >= 0,
            max(wood, na.rm = TRUE) < 1.0001,
            identical(getNames(wood), c("wood", "woodfuel")))
  getNames(wood) <- c("rndwd", "fulwd")
  wood <- .fillAndRasterize(wood, clusterGeometry, target)

  # biofuel area fraction: crpbf_[c3ann,c3nfx,c3per,c4ann,c4per]
  # counting only second generation biofuel here, so reporting only crpbf_[c3per,c4per]
  mappedMag <- (calcOutput("LandHarmonizedCategories", input = "magpie", target = "luh2", aggregate = FALSE))

  betr <- mag[, , "betr"]
  c3per <- mappedMag[, , "c3per"]
  crpbf_c3per <- collapseDim(betr, 3) / c3per
  getNames(crpbf_c3per) <- "crpbf_c3per"
  stopifnot(min(crpbf_c3per, na.rm = TRUE) >= 0,
            max(crpbf_c3per, na.rm = TRUE) < 1.0001)
  crpbf_c3per <- .fillAndRasterize(crpbf_c3per, clusterGeometry, target)

  begr <- mag[, , "begr"]
  c4per <- mappedMag[, , "c4per"]
  crpbf_c4per <- collapseDim(begr, 3) / c4per
  getNames(crpbf_c4per) <- "crpbf_c4per"
  stopifnot(min(crpbf_c4per, na.rm = TRUE) >= 0,
            max(crpbf_c4per, na.rm = TRUE) < 1.0001)
  crpbf_c4per <- .fillAndRasterize(crpbf_c4per, clusterGeometry, target)
  # TODO crpbf_[c3per,c4per] are not harmonized and not properly downscaled

  # irrigated fraction of crop area: irrig_[c3ann,c3nfx,c3per,c4ann,c4per]
  # irrigation <- readSource("Magpie", "irrigation")

  x <- c(wood, crpbf_c3per, crpbf_c4per)
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(x)))
  terra::time(x, tstep = "years") <- as.integer(substr(names(x), 2, 5))

  attr(x, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = x,
              class = "SpatRaster",
              unit = "1", # TODO check everything is actually shares
              description = "management variables calculated by MAgPIE in the format of LUH's management.nc"))

}
