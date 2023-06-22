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

  # irrigated fraction of crop area: irrig_[c3ann,c3nfx,c3per,c4ann,c4per]
  # irrigation <- readSource("Magpie", "irrigation")

  x <- c(wood)
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(x)))
  terra::time(x, tstep = "years") <- as.integer(substr(names(x), 2, 5))

  attr(x, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = x,
              class = "SpatRaster",
              unit = "1", # TODO check everything is actually shares
              description = "management variables calculated by MAgPIE in the format of LUH's management.nc"))

}
