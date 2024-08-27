#' calcNonlandHarmonized
#'
#' Harmonize nonland input data to target data using the specified method, checking
#' data for consistency before returning.
#'
#' @param input name of the input dataset, currently only "magpie"
#' @param target name of the target dataset, currently only "luh2"
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param method harmonization method, see \code{\link{toolGetHarmonizer}} for available methods
#' @return harmonized nonland data
#' @author Pascal Sauer
calcNonlandHarmonized <- function(input = "magpie", target = "luh2mod",
                                  harmonizationPeriod = c(2015, 2050),
                                  method = "fade") {
  xInput <- calcOutput("NonlandInputRecategorized", input = input, aggregate = FALSE)
  geometry <- attr(xInput, "geometry")
  crs <- attr(xInput, "crs")

  xTarget <- calcOutput("NonlandTargetExtrapolated", input = input, target = target, aggregate = FALSE)

  harmonizer <- toolGetHarmonizer(method)
  out <- harmonizer(xInput, xTarget, harmonizationPeriod = harmonizationPeriod)

  # account for primf/primn expansion being recategorized to secdf/secdn in calcLandHarmonized
  primfixShares <- calcOutput("LandHarmonized", input = input, target = target,
                              harmonizationPeriod = harmonizationPeriod,
                              method = method, aggregate = FALSE, supplementary = TRUE)$primfixShares

  # harvest in e.g. 2000 describes yearly harvest from 1995 to 2000, so need to check against land in 1995
  # primfix in 1995 corresponds to harvest in 2000, so shift accordingly
  primfixShares[, -1, ] <- setYears(primfixShares[, -nyears(primfixShares), ], getYears(primfixShares)[-1])
  primfixShares[, 1, ] <- 1

  if (any(primfixShares < 1)) {
    toolStatusMessage("note", paste("after harmonization primf/primn expansion was replaced",
                                    "by secdf/secdn, adapting wood harvest accordingly"))

    for (category in c("bioh", "wood_harvest_area")) {
      forest <- c("primf", "secyf", "secmf")
      totalBeforeForest <- dimSums(out[, , paste0(forest, "_", category)], 3)

      primForest <- paste0("primf_", category)
      toSecForest <- out[, , primForest] * (1 - primfixShares[, , "primf"])
      out[, , primForest] <- out[, , primForest] - toSecForest
      stopifnot(out[, , primForest] >= 0)
      secYoung <- paste0("secyf_", category)
      secMature <- paste0("secmf_", category)
      youngShare <- out[, , secYoung] / (out[, , secYoung] + out[, , secMature])
      youngShare[is.na(youngShare)] <- 0.5
      stopifnot(0 <= youngShare, youngShare <= 1)
      out[, , secYoung] <- out[, , secYoung] + toSecForest * youngShare
      out[, , secMature] <- out[, , secMature] + toSecForest * (1 - youngShare)

      toolExpectLessDiff(totalBeforeForest,
                         dimSums(out[, , paste0(forest, "_", category)], 3),
                         10^-4, paste0("total wood harvest (", category, ") ",
                                       "from forests is not affected by adaptation"))

      # nonforest
      nonforest <- c("primn", "secnf")
      totalBeforeNonforest <- dimSums(out[, , paste0(nonforest, "_", category)], 3)
      primNonforest <- paste0("primn_", category)
      toSecNonforest <- out[, , primNonforest] * (1 - primfixShares[, , "primn"])
      out[, , primNonforest] <- out[, , primNonforest] - toSecNonforest
      stopifnot(out[, , primNonforest] >= 0)
      secNonforest <- paste0("secnf_", category)
      out[, , secNonforest] <- out[, , secNonforest] + toSecNonforest

      toolExpectLessDiff(totalBeforeNonforest,
                         dimSums(out[, , paste0(nonforest, "_", category)], 3),
                         10^-4, paste0("total wood harvest (", category, ") ",
                                       "from non-forests is not affected by adaptation"))
    }
  }

  categories <- grep("wood_harvest_area$", getItems(out, 3), value = TRUE)

  # check if wood harvest area is exceeding land area
  land <- calcOutput("LandHarmonized", input = input, target = target,
                     harmonizationPeriod = harmonizationPeriod,
                     method = method, aggregate = FALSE)

  histYears <- getYears(out, as.integer = TRUE)
  histYears <- histYears[histYears <= harmonizationPeriod[1]]
  toolCheckWoodHarvestArea(out[, histYears, categories], land[, histYears, ],
                           "In historical period, ")

  futureYears <- setdiff(getYears(out, as.integer = TRUE), histYears)
  toolCheckWoodHarvestArea(out[, futureYears, categories], land[, futureYears, ],
                           "After historical period, ")

  attr(out, "geometry") <- geometry
  attr(out, "crs")      <- crs

  # checks
  toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
  toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")),
                 "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(xTarget, dim = 3)),
                 "Nonland categories remain unchanged")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")
  # SpatRaster can hold values up to ~10^40 before replacing with Inf, so check we are well below that
  toolExpectTrue(max(out) < 10^30, "All values are < 10^30")

  return(list(x = out,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg yr-1",
              min = 0,
              description = "Harmonized nonland data"))
}
