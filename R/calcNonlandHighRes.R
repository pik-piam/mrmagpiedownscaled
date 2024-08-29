#' calcNonlandHighRes
#'
#' This function performs the downscaling: It calculates a high resolution dataset
#' from the low resolution input dataset and the high resolution target dataset.
#'
#' @param input name of an input dataset, currently only "magpie"
#' @param target name of a target dataset, currently only "luh2mod"
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @return downscaled nonland data
#' @author Pascal Sauer
calcNonlandHighRes <- function(input = "magpie", target = "luh2mod", harmonizationPeriod = c(2015, 2050)) {
  x <- calcOutput("NonlandHarmonized", input = input, target = target,
                  harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)

  futureYears <- getYears(x, as.integer = TRUE)
  futureYears <- futureYears[futureYears > harmonizationPeriod[1]]

  x <- x[, futureYears, ]

  weight <- calcOutput("LandHighRes", input = input, target = target,
                       harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)
  weight <- weight[, , c("urban", "pastr", "range"), invert = TRUE]
  map <- as.data.frame(rbind(c("primf", "primf"),
                             c("primn", "primn"),
                             c("secdn", "secdn"),
                             c("c3ann_irrigated", "c3ann"),
                             c("c3ann_rainfed", "c3ann"),
                             c("c3ann_irrigated_biofuel_1st_gen", "c3ann"),
                             c("c3ann_rainfed_biofuel_1st_gen", "c3ann"),
                             c("c3nfx_irrigated", "c3nfx"),
                             c("c3nfx_rainfed", "c3nfx"),
                             c("c3nfx_irrigated_biofuel_1st_gen", "c3nfx"),
                             c("c3nfx_rainfed_biofuel_1st_gen", "c3nfx"),
                             c("c3per_irrigated", "c3per"),
                             c("c3per_rainfed", "c3per"),
                             c("c3per_irrigated_biofuel_1st_gen", "c3per"),
                             c("c3per_rainfed_biofuel_1st_gen", "c3per"),
                             c("c3per_irrigated_biofuel_2nd_gen", "c3per"),
                             c("c3per_rainfed_biofuel_2nd_gen", "c3per"),
                             c("c4ann_irrigated", "c4ann"),
                             c("c4ann_rainfed", "c4ann"),
                             c("c4ann_irrigated_biofuel_1st_gen", "c4ann"),
                             c("c4ann_rainfed_biofuel_1st_gen", "c4ann"),
                             c("c4per_irrigated", "c4per"),
                             c("c4per_rainfed", "c4per"),
                             c("c4per_irrigated_biofuel_1st_gen", "c4per"),
                             c("c4per_rainfed_biofuel_1st_gen", "c4per"),
                             c("c4per_irrigated_biofuel_2nd_gen", "c4per"),
                             c("c4per_rainfed_biofuel_2nd_gen", "c4per"),
                             c("forestry", "secdf"),
                             c("secdf", "secdf")))
  colnames(map) <- c("from", "to")
  weight <- toolAggregate(weight, map, from = "from", to = "to", dim = 3)

  # wood harvest area, use land in previous year (as that is what's harvested) as weight
  harvestArea <- x[, , woodHarvestAreaCategories()]
  attr(harvestArea, "geometry") <- NULL
  attr(harvestArea, "comment") <- NULL
  attr(harvestArea, "crs") <- NULL

  nonlandTarget <- calcOutput("NonlandTarget", target = target, aggregate = FALSE)
  nonlandTarget <- as.magpie(nonlandTarget[[terra::time(nonlandTarget) == harmonizationPeriod[1]]])
  nonlandTarget <- nonlandTarget[, , woodHarvestAreaCategories()]

  w <- weight[, , woodlandCategories()]
  w <- setYears(w[, -nyears(w), ],
                getYears(w)[-1])
  w <- toolDisaggregateWoodHarvest(w, nonlandTarget + 10^-30)
  w <- w + 10^-30

  resmap <- calcOutput("ResolutionMapping", input = input, target = target, aggregate = FALSE)
  harvestAreaDownscaled <- toolAggregate(harvestArea, resmap, weight = w, from = "lowRes", to = "cell", dim = 1)


  bioh <- x[, , sub("wood_harvest_area$", "bioh", woodHarvestAreaCategories())]
  weightBioh <- harvestAreaDownscaled + 10^-30
  getItems(weightBioh, 3) <- sub("wood_harvest_area$", "bioh", getItems(weightBioh, 3))
  stopifnot(getItems(weightBioh, 3) == getItems(bioh, 3))
  biohDownscaled <- toolAggregate(bioh, resmap, weight = weightBioh, from = "lowRes", to = "cell", dim = 1)

  # TODO fertilizer

  # TODO harvest_weight_type



  inSum <- dimSums(xInput, dim = 1)
  outSum <- dimSums(out, dim = 1)

  maxdiff <- max(abs(inSum - outSum) / inSum, na.rm = TRUE)
  toolExpectTrue(maxdiff < 10^-5,
                 paste0("Relative global sum difference per category before and after downscaling < 10^-5 ",
                        "(max relative diff: ", signif(maxdiff, 2), ")"))
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(xInput, dim = 3)),
                 "Nonland categories remain unchanged")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")
  outRaster <- as.SpatRaster(out[, harmonizationPeriod[1], ])
  deviation <- terra::extend(outRaster, xTarget) - xTarget[[names(outRaster)]]
  toolExpectLessDiff(max(abs(terra::minmax(deviation))), 0, 10^-5,
                     paste("In", harmonizationPeriod[1], "downscaled data equals target data"))

  land <- calcOutput("LandHighRes", input = input, target = target,
                     harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)
  toolCheckWoodHarvestArea(out[, getYears(land), grep("wood_harvest_area$", getItems(out, 3))], land)

  return(list(x = out,
              min = 0,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg yr-1",
              description = "Downscaled nonland data"))
}
