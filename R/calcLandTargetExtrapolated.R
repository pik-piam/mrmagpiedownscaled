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
  maxHarvestHist <- toolMaxHarvestPerYear(xTarget)
  stopifnot(setequal(getItems(harvestHist, 1), getItems(maxHarvestHist, 1)),
            setequal(getItems(harvestHist[, -1, ], 2), getItems(maxHarvestHist, 2)),
            setequal(getItems(harvestHist, 3), getItems(maxHarvestHist, 3)))

  # calculate share: wood harvest area / max possible harvest
  harvestShare <- dimSums(harvestHist[, -1, ], 2) / dimSums(maxHarvestHist, 2)
  harvestShare[is.na(harvestShare)] <- 0
  harvestShare[harvestShare > 1] <- 1

  secymf <- paste0(c("secyf", "secmf"), "_wood_harvest_area")
  normalization <- dimSums(harvestShare[, , secymf], 3) + 10^-10 # + 10^-10 to ensure sum <= 1
  normalization[normalization < 1] <- 1
  harvestShare[, , secymf] <- harvestShare[, , secymf] / normalization

  stopifnot(0 <= harvestShare, harvestShare <= 1,
            dimSums(harvestShare[, , secymf], 3) <= 1)

  # calculate wood harvest area, reduce primf and primn so they are consistent with harvest
  harvest <- add_columns(harvestHist, paste0("y", transitionYears), dim = 2)
  timestepLength <- unique(diff(getYears(out, as.integer = TRUE)))
  stopifnot(identical(getYears(harvest), getYears(out)),
            length(timestepLength) == 1, timestepLength > 0)
  for (i in match(transitionYears, getYears(out, as.integer = TRUE))) {
    primfn <- c("primf", "primn")
    secdfn <- c("secdf", "secdn")

    # in toolMaxHarvestPerYear out[, i, ] is only used to determine timestepLength and then thrown away
    harvest[, i, ] <- harvestShare * toolMaxHarvestPerYear(out[, c(i - 1, i), ])

    harvestAgg <- toolAggregateWoodHarvest(harvest[, i, ])
    maxPossiblePrim <- out[, i - 1, primfn] - timestepLength * harvestAgg[, , primfn]

    # more prim than in previous timestep is not possible, convert to secd
    toSecd <- out[, i, primfn] - maxPossiblePrim
    toSecd[toSecd < 0] <- 0
    getItems(toSecd, 3) <- c("secdf", "secdn")
    out[, i, secdfn] <- out[, i, secdfn] + toSecd
    out[, i, primfn] <- pmin(out[, i, primfn], maxPossiblePrim)

    woodland <- out[, , getItems(harvestAgg, 3)]
    stopifnot(harvestAgg <= woodland[, i - 1, ] / timestepLength,
              woodland[, i, primfn] <= woodland[, i - 1, primfn] - timestepLength * harvestAgg[, , primfn])
  }

  # consistency checks land
  toolExpectLessDiff(out[, histYears, ], xTarget, 0,
                     "In historical period, land was not changed")
  toolExpectLessDiff(dimSums(out, 3), targetArea, 10^-5, "Total area is constant over time")
  toolExpectTrue(all(out[, -1, c("primf", "primn")] <= setYears(out[, -nyears(out), c("primf", "primn")],
                                                                getYears(out)[-1])),
                 "primf and primn are never expanding", falseStatus = "warn")

  # consistency checks wood harvest area
  toolExpectLessDiff(harvest[, histYears, ], harvestHist, 0,
                     "In historical period, wood harvest area was not changed")
  toolExpectTrue(min(harvest) >= 0, "wood harvest area is >= 0")

  histYears <- getYears(harvest, as.integer = TRUE)
  histYears <- histYears[histYears < transitionYears[1]]
  toolCheckWoodHarvestArea(harvest[, histYears, ], out[, histYears, ],
                           "In historical period, ")

  futureYears <- setdiff(getYears(harvest, as.integer = TRUE), histYears)
  toolCheckWoodHarvestArea(harvest[, futureYears, ], out[, futureYears, ],
                           "After historical period, ")

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Extrapolated land target data for harmonization",
              woodHarvestArea = harvest))
}

toolNegativeToZero <- function(x, warnThreshold = -10^-5, status = "note") {
  m <- min(x, na.rm = TRUE)
  if (m < warnThreshold) {
    toolStatusMessage(status, paste("Replacing negative values with zero. Minimum value:", m), level = 1)
  }
  x[x < 0] <- 0
  return(x)
}
