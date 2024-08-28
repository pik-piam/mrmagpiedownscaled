#' calcLandTargetExtrapolated
#'
#' First aggregate to low resolution, then extrapolate to the given years
#' using toolExtrapolate.
#'
#' @param target name of the target dataset, options are: luh2, luh2mod
#' luh2mod will split secdf into forestry and secdf
#' @return extrapolated land target data
#' @author Pascal Sauer
calcLandTargetExtrapolated <- function(input = "magpie", target = "luh2mod",
                                       transitionYears = seq(2020, 2045, 5)) {
  stopifnot(identical(transitionYears, sort(transitionYears)))

  xTarget <- calcOutput("LandTargetLowRes", input = input, target = target, aggregate = FALSE)
  histYears <- getYears(xTarget, as.integer = TRUE)

  # ---------- extrapolate -------------
  exTarget <- toolExtrapolate(xTarget, transitionYears)
  exTarget[exTarget < 0] <- 0

  # TODO describe what happens from here in documentation

  # ---------- normalize -------------
  # normalize exTarget so that its total sum over all layers agrees for all time steps
  # with the sum over all layers in target in the harmonization year (e.g. makes sure
  # that the land changes in a land data set do not alter the total sum of land.)
  targetArea <- dimSums(setYears(xTarget[, max(histYears), ], NULL), dim = 3)
  exTarget <- exTarget * targetArea / dimSums(exTarget, dim = 3)
  exTarget[is.na(exTarget)] <- 0
  out <- mbind(xTarget, exTarget)

  # ------- calculate wood harvest shares -------
  harvestHist <- calcOutput("NonlandTargetLowRes", input = input, target = target, aggregate = FALSE)
  harvestHist <- harvestHist[, , endsWith(getItems(harvestHist, 3), "wood_harvest_area")]
  harvest <- toolAggregateWoodHarvest(harvestHist)
  timestepLength <- unique(diff(getYears(harvest, as.integer = TRUE)))
  stopifnot(length(timestepLength) == 1)
  harvest <- harvest * timestepLength # harvest is per year, need per timestep
  stopifnot(identical(dimnames(harvest)[1:2], dimnames(xTarget)[1:2]))

  # calculate share: primf|primn wood harvest area / total primf|primn area
  primShare <- dimSums(harvest[, , c("primf", "primn")], 2) / dimSums(xTarget[, , c("primf", "primn")], 2)
  primShare[is.na(primShare)] <- 0
  primShare[primShare > 1] <- 1
  stopifnot(0 <= primShare, primShare <= 1)

  # calculate share: forest (primf + secdf) wood harvest area / total forest area
  forest <- c("primf", "secdf")
  totalShareForest <- dimSums(harvest[, , forest], c(2, 3)) / dimSums(xTarget[, , forest], c(2, 3))
  totalShareForest[is.na(totalShareForest)] <- 0
  totalShareForest[totalShareForest > 1] <- 1
  stopifnot(0 <= totalShareForest, totalShareForest <= 1)

  # calculate share: nature (primn + secdn) wood harvest area / total nature area
  nature <- c("primn", "secdn")
  totalShareNature <- dimSums(harvest[, , nature], c(2, 3)) / dimSums(xTarget[, , nature], c(2, 3))
  totalShareNature[is.na(totalShareNature)] <- 0
  totalShareNature[totalShareNature > 1] <- 1
  stopifnot(0 <= totalShareNature, totalShareNature <= 1)

  # calculate wood harvest area, reduce primf and primn so they are consistent with harvest
  harvest <- add_columns(harvest, paste0("y", transitionYears), dim = 2)
  for (i in match(transitionYears, getYears(out, as.integer = TRUE))) {
    overHarvest <- new.magpie(getItems(out, 1), fill = 0)

    for (category in c("forest", "nature")) {
      if (category == "forest") {
        prim <- "primf"
        secd <- "secdf"
        totalShare <- totalShareForest
      } else if (category == "nature") {
        prim <- "primn"
        secd <- "secdn"
        totalShare <- totalShareNature
      }

      # if prim[i] > prim[i-1] - prim_harvest[i-1]: convert excess to secd
      maxPossiblePrim <- out[, i - 1, prim] - harvest[, i - 1, prim]
      maxPossiblePrim <- toolNegativeToZero(maxPossiblePrim, status = "warn")

      toSecd <- out[, i, prim] - maxPossiblePrim
      toSecd[toSecd < 0] <- 0
      out[, i, secd] <- out[, i, secd] + toSecd
      out[, i, prim] <- pmin(out[, i, prim], maxPossiblePrim)

      totalHarvest <- dimSums(out[, i, c(prim, secd)], 3) * totalShare
      harvest[, i, prim] <- min(totalHarvest, out[, i, prim] * primShare[, , prim])
      secdHarvest <- totalHarvest - harvest[, i, prim]

      stopifnot(secdHarvest >= 0)
      oh <- out[, i, secd] - secdHarvest
      oh <- -1 * oh
      oh[oh < 0] <- 0
      overHarvest <- overHarvest + collapseDim(oh)

      harvest[, i, secd]  <- pmin(secdHarvest, out[, i, secd])

      stopifnot(harvest[, i, c(prim, secd)] >= 0,
                harvest[, i, c(prim, secd)] <= out[, i, c(prim, secd)])
    }

    stopifnot(overHarvest >= 0)

    for (category in c("secdf", "secdn", "primf", "primn")) {
      unharvested <- out[, i, category] - harvest[, i, category]
      overHarvest <- overHarvest - unharvested
      overHarvest[overHarvest < 0] <- 0
      harvest[, i, category] <- pmin(out[, i, category], harvest[, i, category] + overHarvest)
    }

    if (max(overHarvest) > 0) {
      toolStatusMessage(if (max(overHarvest) > 10^-5) "warn" else "note",
                        paste0("Excess wood harvest area in ", getYears(out)[i],
                               ": ", max(overHarvest), "Mha"))
    }
  }

  exHarvest <- harvest[, transitionYears, ]
  stopifnot(exHarvest <= out[, transitionYears, getItems(exHarvest, 3)])
  # dividing and later multiplying by the timestepLength might not be exactly the same (numerical limits)
  exHarvest <- pmin(exHarvest / timestepLength,
                    out[, transitionYears, getItems(exHarvest, 3)] / (timestepLength + 10^-10))
  # divide by timestepLength + 10^-10 to ensure the following check works
  stopifnot(timestepLength * exHarvest <= out[, transitionYears, getItems(exHarvest, 3)])

  youngMatureShare <- dimSums(harvestHist[, , paste0(c("secyf", "secmf"), "_wood_harvest_area")], dim = 2)
  youngMatureShare <- youngMatureShare / dimSums(youngMatureShare, 3)
  youngMatureShare[is.na(youngMatureShare)] <- 0.5
  stopifnot(abs(dimSums(youngMatureShare, 3) - 1) < 10^-5,
            0 <= youngMatureShare, youngMatureShare <= 1)
  weight <- add_dimension(youngMatureShare, dim = 2, add = "year", nm = getItems(exHarvest, 2))
  weight <- add_columns(weight, setdiff(getItems(harvestHist, 3), getItems(weight, 3)),
                        dim = 3, fill = 1)
  exHarvest <- toolDisaggregateWoodHarvest(exHarvest, weight = weight)
  outHarvest <- mbind(harvestHist, exHarvest)

  # consistency checks land
  toolExpectLessDiff(out[, histYears, ], xTarget, 0,
                     "In historical period, land was not changed")
  toolExpectLessDiff(dimSums(out, 3), targetArea, 10^-5, "Total area is constant over time")
  toolExpectTrue(all(out[, -1, c("primf", "primn")] <= setYears(out[, -nyears(out), c("primf", "primn")],
                                                                getYears(out)[-1])),
                 "primf and primn are never expanding", falseStatus = "warn")

  # consistency checks wood harvest area
  toolExpectLessDiff(outHarvest[, histYears, ], harvestHist, 0,
                     "In historical period, wood harvest area was not changed")
  toolExpectTrue(min(outHarvest) >= 0, "wood harvest area is >= 0")

  histYears <- getYears(outHarvest, as.integer = TRUE)
  histYears <- histYears[histYears < transitionYears[1]]
  toolCheckWoodHarvestArea(outHarvest[, histYears, ], out[, histYears, ],
                           "In historical period, ")

  futureYears <- setdiff(getYears(outHarvest, as.integer = TRUE), histYears)
  toolCheckWoodHarvestArea(outHarvest[, futureYears, ], out[, futureYears, ],
                           "After historical period, ")

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Extrapolated land target data for harmonization",
              woodHarvestArea = outHarvest))
}

toolNegativeToZero <- function(x, warnThreshold = -10^-5, status = "note") {
  m <- min(x, na.rm = TRUE)
  if (m < warnThreshold) {
    toolStatusMessage(status, paste("Replacing negative values with zero. Minimum value:", m), level = 1)
  }
  x[x < 0] <- 0
  return(x)
}
