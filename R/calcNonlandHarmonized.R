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
                                  harmonizationPeriod = c(2015, 2050), method = "fade") {
  xInput <- calcOutput("NonlandInputRecategorized", input = input, target = target, aggregate = FALSE)
  geometry <- attr(xInput, "geometry")
  crs <- attr(xInput, "crs")

  xTarget <- calcOutput("NonlandTargetExtrapolated", input = input, target = target,
                        transitionYears = transitionYears, aggregate = FALSE)

  harmonizer <- toolGetHarmonizer(method)
  out <- harmonizer(xInput[, , woodHarvestAreaCategories(), invert = TRUE],
                    xTarget[, , woodHarvestAreaCategories(), invert = TRUE],
                    harmonizationPeriod = harmonizationPeriod)

  harvestArea <- calcOutput("WoodHarvestAreaHarmonized", input = input, target = target,
                            harmonizationPeriod = harmonizationPeriod, method = method, aggregate = FALSE)
  # TODO! adapt bioh and harvest_weight_type to match the harmonized harvest area
  out <- mbind(out, harvestArea)

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

# toolApplyPrimfixShares <- function(harvest, primfixShares) {
#   stopifnot(setequal(getItems(harvest, 3), woodHarvestAreaCategories()),
#             identical(getYears(harvest), getYears(primfixShares)))
#   # harvest in e.g. 2000 describes yearly harvest from 1995 to 2000, so need to check against land in 1995
#   # primfix in 1995 corresponds to harvest in 2000, so shift accordingly
#   primfixShares[, -1, ] <- setYears(primfixShares[, -nyears(primfixShares), ], getYears(primfixShares)[-1])
#   primfixShares[, 1, ] <- 1
#   if (any(primfixShares < 1)) {
#     toolStatusMessage("note", paste("after harmonization primf/primn expansion was replaced",
#                                     "by secdf/secdn, adapting wood harvest accordingly"))

#     for (category in c("wood_harvest_area")) { # TODO category "bioh"?
#       forest <- c("primf", "secyf", "secmf")
#       totalBeforeForest <- dimSums(harvest[, , paste0(forest, "_", category)], 3)

#       primForest <- paste0("primf_", category)
#       toSecForest <- harvest[, , primForest] * (1 - primfixShares[, , "primf"])
#       harvest[, , primForest] <- harvest[, , primForest] - toSecForest
#       stopifnot(harvest[, , primForest] >= 0)
#       secYoung <- paste0("secyf_", category)
#       secMature <- paste0("secmf_", category)
#       youngShare <- harvest[, , secYoung] / (harvest[, , secYoung] + harvest[, , secMature])
#       youngShare[is.na(youngShare)] <- 0.5
#       stopifnot(0 <= youngShare, youngShare <= 1)
#       harvest[, , secYoung] <- harvest[, , secYoung] + toSecForest * youngShare
#       harvest[, , secMature] <- harvest[, , secMature] + toSecForest * (1 - youngShare)

#       toolExpectLessDiff(totalBeforeForest,
#                          dimSums(harvest[, , paste0(forest, "_", category)], 3),
#                          10^-4, paste0("total wood harvest (", category, ") ",
#                                        "from forests is not affected by adaptation"),
#                          level = 1)

#       # nonforest
#       nonforest <- c("primn", "secnf")
#       totalBeforeNonforest <- dimSums(harvest[, , paste0(nonforest, "_", category)], 3)
#       primNonforest <- paste0("primn_", category)
#       toSecNonforest <- harvest[, , primNonforest] * (1 - primfixShares[, , "primn"])
#       harvest[, , primNonforest] <- harvest[, , primNonforest] - toSecNonforest
#       stopifnot(harvest[, , primNonforest] >= 0)
#       secNonforest <- paste0("secnf_", category)
#       harvest[, , secNonforest] <- harvest[, , secNonforest] + toSecNonforest

#       toolExpectLessDiff(totalBeforeNonforest,
#                          dimSums(harvest[, , paste0(nonforest, "_", category)], 3),
#                          10^-4, paste0("total wood harvest (", category, ") ",
#                                        "from non-forests is not affected by adaptation"),
#                          level = 1)
#     }
#   }
#   return(harvest)
# }
