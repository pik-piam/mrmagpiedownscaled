
calcNonlandHarmonized <- function(input = "magpie", target = "luh2",
                                  harmonizeYear = 2015, finalYear = 2050,
                                  method = "extrapolateFade") {
  # could try to harmonize fertilizer data using relative values (kg ha-1 yr-1) instead of absolute values (kg yr-1)

  input <- calcOutput("NonlandHarmonizedCategories", input = input, aggregate = FALSE)
  geometry <- attr(input, "geometry")
  crs <- attr(input, "crs")

  target <- calcOutput("NonlandTargetData", target = target, aggregate = FALSE)
  # bring target data to spatial resolution of input data
  ref    <- as.SpatVector(input[, 1, 1])[, c(".region", ".id")]
  shareCategories <- c("rndwd", "fulwd")
  nonShareCategories <- "fertilizer"
  stopifnot(grepl(paste(c(shareCategories, nonShareCategories), collapse = "|"), names(target)))
  targetNonShare <- terra::extract(target[nonShareCategories], ref, sum, na.rm = TRUE, bind = TRUE)

  land <- readSource("LUH2v2h")
  stopifnot(terra::units(land) == "Mha")
  forest <- land["primf"] + land["secdf"]
  forestCluster <- terra::extract(forest, ref, sum, na.rm = TRUE, bind = TRUE)
  forestWoodHarvest <- terra::extract(target[paste(shareCategories, collapse = "|")] * forest,
                                      ref, sum, na.rm = TRUE, bind = TRUE)
  forestWoodHarvest <- as.magpie(forestWoodHarvest)
  woodHarvest <- forestWoodHarvest / collapseDim(as.magpie(forestCluster))
  woodHarvest[forestWoodHarvest == 0] <- 0 # avoid NaNs

  # use weighted average, weight = primf + secdf area
  target <- mbind(woodHarvest, as.magpie(targetNonShare))
  stopifnot(setequal(getItems(input, 3), getItems(target, 3)))
  target <- target[, , getItems(input, 3)] # harmonize order of dim 3

  harmonizer <- toolGetHarmonizer(method)
  out <- harmonizer(input, target, harmonizeYear = harmonizeYear, finalYear = finalYear)

  attr(out, "geometry") <- geometry
  attr(out, "crs")      <- crs

  # checks
  toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
  toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(input, dim = 3)), "Nonland categories remain unchanged")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  toolExpectTrue(all(out[, , shareCategories] <= 1.0001), "All shares are <= 1")

  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "rndwd, fulwd: 1, *_fertilizer: kg yr-1",
              min = 0,
              description = "Harmonized nonland data"))
}
