#' calcLandHarmonizedCategories
#'
#' Computes the land input data in target land categories. Splitting of land
#' categories is performed under use of internal land weights reflecting the
#' prevalence of a certain land category in the given area.
#'
#' Mapping from input to target categories is achieved via a merge of a land input
#' mapping to reference categories and a mapping between land target categories and
#' the same reference categories. Thereby a new source or new target can be supported
#' by supplying a map of that new input and/or target to the reference categories.
#'
#' @param input name of the land input source to be used
#' @param target name of the land target source to be used
#' @author Jan Philipp Dietrich
calcLandHarmonizedCategories <- function(input = "magpie", target = "luh2mod") {
  map <- toolLandCategoriesMapping(input, target)
  x   <- calcOutput("LandInput", input = input, aggregate = FALSE)

  resolutionMapping <- calcOutput("ResolutionMapping", input = input, target = target, aggregate = FALSE)
  resolutionMapping$cluster <- resolutionMapping$lowRes
  "!# @monitor magpie4:::addGeometry"
  x <- magpie4::addGeometry(x, resolutionMapping)

  # get weights for disaggregation to reference categories
  ref <- calcOutput("LandCategorizationWeight", map = map, geometry = attr(x, "geometry"),
                    crs = attr(x, "crs"), aggregate = FALSE)
  y   <- toolAggregate(x, map, dim = 3, from = "dataInput", to = "merge", weight = ref)
  out <- toolAggregate(y, map, dim = 3, from = "merge",     to = "dataOutput")

  # category remapping does not take into account that primn cannot expand, so redistribute:
  # if totaln shrinks, shrink primn and secdn according to their proportions in the previous timestep
  # if totaln expands, expand only secdn, primn stays constant
  totaln <- dimSums(out[, , c("primn", "secdn")], 3)
  for (i in seq_len(nyears(totaln) - 1)) {
    dif <- totaln[, i + 1, ] - totaln[, i, ]

    stopifnot(out[, i, c("primn", "secdn")] >= 0)
    expansion <- out[, i, c("primn", "secdn")] * 0
    expansion[, , "secdn"] <- dif
    expansion[expansion < 0] <- 0

    shrinking <- out[, i, c("primn", "secdn")] * (collapseDim(dif) / totaln[, i, ])
    shrinking[is.na(shrinking) | shrinking > 0] <- 0

    newValues <- out[, i, c("primn", "secdn")] + expansion + shrinking
    if (!all(is.finite(newValues) & newValues >= -10^-10)) {
      summ <- summary(newValues)
      toolExpectTrue(FALSE, paste("Unexpected values while replacing primn expansion with secdn expansion, summary:",
                                  paste(names(summ), summ, collapse = ", ")))
    }
    newValues[newValues < 0] <- 0
    out[, i + 1, c("primn", "secdn")] <- newValues
  }

  attr(out, "crs") <- attr(x, "crs")
  attr(out, "geometry") <- attr(x, "geometry")

  # check data for consistency
  toolExpectLessDiff(dimSums(out[, , c("primn", "secdn")], 3), totaln, 10^-5,
                     paste("No change in sum of primn and secdn when replacing",
                           "primn expansion with secdn expansion"))
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")),
                 "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), map$dataOutput), "Land categories match target definition")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-6, "Total areas stay constant over time")
  toolExpectLessDiff(outSum, dimSums(x, dim = 3), 10^-6,
                     "Total areas are not affected by recategorization")
  toolExpectTrue(all(out[, -1, c("primf", "primn")] <= setYears(out[, -nyears(out), c("primf", "primn")],
                                                                getYears(out[, -1, ]))),
                 "primf and primn are never expanding", falseStatus = "warn")

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Input data with land categories remapped to categories of target dataset"))
}
