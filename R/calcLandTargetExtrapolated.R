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
  xTarget <- calcOutput("LandTargetLowRes", input = input, target = target, aggregate = FALSE)

  # ---------- extrapolate -------------
  exTarget <- toolExtrapolate(xTarget, transitionYears)
  exTarget[exTarget < 0] <- 0

  # ---------- normalize -------------
  # normalize exTarget so that its total sum over all layers agrees for all time steps
  # with the sum over all layers in target in the harmonization year (e.g. makes sure
  # that the land changes in a land data set do not alter the total sum of land.)
  targetArea <- dimSums(setYears(xTarget[, harmonizationPeriod[1], ], NULL), dim = 3)
  exTarget <- exTarget * targetArea / dimSums(exTarget, dim = 3)
  exTarget[is.na(exTarget)] <- 0

  # ------- calculate wood harvest shares -------
  harvest <- calcOutput("NonlandTargetLowRes", input = input, target = target, aggregate = FALSE)
  harvest <- harvest[, , endsWith(getItems(harvest, 3), "wood_harvest_area")]
  harvest <- toolAggregateWoodHarvest(harvest)
  timestepLength <- unique(diff(getYears(harvest, as.integer = TRUE)))
  stopifnot(length(timestepLength) == 1)
  harvest <- harvest * timestepLength # harvest is per year, need per timestep
  stopifnot(identical(dimnames(harvest)[1:2], dimnames(xTarget)[1:2]))

  # calculate share: primf|primn wood harvest area / total primf|primn area
  primShare <- dimSums(harvest[, , c("primf", "primn")], 2) / dimSums(xTarget[, , c("primf", "primn")], 2)
  primShare[is.na(primShare)] <- 0
  primShare[primShare > 1] <- 1 # TODO note this in the log
  stopifnot(0 <= primShare, primShare <= 1)

  # calculate share: forest (primf + secdf) wood harvest area / total forest area
  forest <- c("primf", "secdf")
  totalShareForest <- dimSums(harvest[, , forest], c(2, 3)) / dimSums(xTarget[, , forest], c(2, 3))
  totalShareForest[is.na(totalShareForest)] <- 0
  totalShareForest[totalShareForest > 1] <- 1 # TODO note this in the log
  stopifnot(0 <= totalShareForest, totalShareForest <= 1)

  # calculate share: nature (primn + secdn) wood harvest area / total nature area
  nature <- c("primn", "secdn")
  totalShareNature <- dimSums(harvest[, , nature], c(2, 3)) / dimSums(xTarget[, , nature], c(2, 3))
  totalShareNature[is.na(totalShareNature)] <- 0
  totalShareNature[totalShareNature > 1] <- 1 # TODO note this in the log
  stopifnot(0 <= totalShareNature, totalShareNature <= 1)

  out <- mbind(xTarget, exTarget)

  # calculate wood harvest area, reduce primf and primn so they are consistent with harvest
  outHarvest <- add_columns(harvest, paste0("y", transitionYears), dim = 2)
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
      maxPossiblePrim <- out[, i - 1, prim] - outHarvest[, i - 1, prim]
      maxPossiblePrim <- toolNegativeToZero(maxPossiblePrim, status = "warn")

      toSecd <- out[, i, prim] - maxPossiblePrim
      toSecd[toSecd < 0] <- 0
      out[, i, secd] <- out[, i, secd] + toSecd
      out[, i, prim] <- pmin(out[, i, prim], maxPossiblePrim)

      totalHarvest <- dimSums(out[, i, c(prim, secd)], 3) * totalShare
      outHarvest[, i, prim] <- min(totalHarvest, out[, i, prim] * primShare[, , prim])
      secdHarvest <- totalHarvest - outHarvest[, i, prim]

      stopifnot(secdHarvest >= 0)
      oh <- out[, i, secd] - secdHarvest
      oh <- -1 * oh
      oh[oh < 0] <- 0
      overHarvest <- overHarvest + collapseDim(oh)

      outHarvest[, i, secd]  <- pmin(secdHarvest, out[, i, secd])

      stopifnot(outHarvest[, i, c(prim, secd)] >= 0,
                outHarvest[, i, c(prim, secd)] <= out[, i, c(prim, secd)])
    }

    stopifnot(overHarvest >= 0)

    for (category in c("secdf", "secdn", "primf", "primn")) {
      unharvested <- out[, i, category] - outHarvest[, i, category]
      overHarvest <- overHarvest - unharvested
      overHarvest[overHarvest < 0] <- 0
      outHarvest[, i, category] <- pmin(out[, i, category], outHarvest[, i, category] + overHarvest)
    }

    if (max(overHarvest) > 0) {
      toolStatusMessage(if (max(overHarvest) > 10^-5) "warn" else "note",
                        paste0("Excess wood harvest area in ", getYears(out)[i],
                               ": ", max(overHarvest), "Mha"))
    }
  }
  stopifnot(is.finite(outHarvest),
            outHarvest >= 0,
            outHarvest <= out[, , getItems(outHarvest, 3)])
  outHarvest <- outHarvest / timestepLength

  # TODO checks
  dif <- out[, , getItems(outHarvest, 3)] - outHarvest
  where(dif < 0)$true$indi
  # TODO split checks for history and extrapolation
  stopifnot(outHarvest >= 0,
            timestepLength * outHarvest <= out[, , getItems(outHarvest, 3)])
  pr <- c("primf", "primn")
  stopifnot(out[, -1, pr] <= out[, -nyears(out), pr] - outHarvest[, -nyears(outHarvest), pr])

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Extrapolated land target data for harmonization"))
}

toolNegativeToZero <- function(x, warnThreshold = -10^-5, status = "note") {
  m <- min(x, na.rm = TRUE)
  if (m < warnThreshold) {
    toolStatusMessage(status, paste("Replacing negative values with zero. Minimum value:", m), level = 1)
  }
  x[x < 0] <- 0
  return(x)
}
