#' calcLandHarmonized
#'
#' This function computes a version of the chosen land input data in the resolution
#' of the land input data but harmonized to the land use information of the
#' chosen land target data set (harmonized categories as well as harmonized
#' transition from historic target data to simulated input data).
#'
#' @param input name of the land input source to be used
#' @param target name of the land target source to be used
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param method transitioning method
#' @author Pascal Sauer, Jan Philipp Dietrich
calcLandHarmonized <- function(input = "magpie", target = "luh2mod",
                               harmonizationPeriod = c(2015, 2050),
                               method = "extrapolateFade") {
  input    <- calcOutput("LandHarmonizedCategories", input = input,
                         target = target, aggregate = FALSE)
  geometry <- attr(input, "geometry")
  crs      <- attr(input, "crs")

  target <- calcOutput("LandTarget", target = target, aggregate = FALSE)
  # bring target data to spatial resolution of input data
  ref    <- as.SpatVector(input[, 1, 1])[, c(".region", ".id")]
  target <- terra::extract(target, ref, sum, na.rm = TRUE, bind = TRUE)
  target <- as.magpie(target)
  stopifnot(setequal(getItems(input, 3), getItems(target, 3)))
  target <- target[, , getItems(input, 3)] # harmonize order of dim 3

  # checks and corrections
  inSum <- dimSums(input, dim = 3)
  tSum <- dimSums(target, dim = 3)
  toolExpectLessDiff(inSum, inSum[, 1, ], 10^-5, "Total areas in input stay constant over time")
  toolExpectLessDiff(tSum, tSum[, 1, ], 10^-5, "Total areas in target stay constant over time")
  toolExpectLessDiff(inSum[, 1, ], tSum[, 1, ], 10^-5,
                     "Total areas are the same in target and input data")
  if (max(abs(inSum[, 1, ] - tSum[, 1, ])) >= 10^-5) {
    corr <- setYears(dimSums(target[, 1, ], dim = 3) / dimSums(input[, 1, ], dim = 3), NULL)
    input <- input * corr
    toolStatusMessage("warn", paste0("input data multiplied with correction factors to match target areas ",
                                     "(max ratio = ", round(max(corr), 2),
                                     ", min ratio = ", round(min(corr), 2),  ")"))
  }

  if (method == "extrapolateFade") method <- "extrapolateFadeConstantSum"
  harmonizer <- toolGetHarmonizer(method)
  out <- harmonizer(input, target, harmonizationPeriod = harmonizationPeriod)
  out[is.na(out)] <- 0 # why are NAs introduced here? input and target have no NAs

  # during harmonization primf and primn expansion might be introduced due to
  # primf or primn differences between input and target dataset
  # here we replace primf and primn expansion with secdf and secdn
  primDiffs <- NULL
  stopifnot(nyears(out) >= 2)
  for (i in 2:nyears(out)) {
    primDiff <- out[, i, c("primf", "primn")] - setYears(out[, i - 1, c("primf", "primn")], getYears(out)[i])
    primDiff[primDiff < 0] <- 0
    primDiffs <- mbind(primDiffs, primDiff)
    # use pmin instead of subtracting to avoid tiny expansions due to numerical precision
    out[, i, c("primf", "primn")] <- pmin(out[, i, c("primf", "primn")], out[, i - 1, c("primf", "primn")])
    out[, i, c("secdf", "secdn")] <- out[, i, c("secdf", "secdn")] + setNames(primDiff, c("secdf", "secdn"))
  }

  toolExpectTrue(max(primDiffs) <= 0, "harmonization does not introduce primf or primn expansion")
  if (max(primDiffs) > 0) {
    toolStatusMessage("note", "replaced primf/primn expansion with secdf/secdn expansion")
  }

  attr(out, "geometry") <- geometry
  attr(out, "crs")      <- crs

  # checks
  toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
  toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")),
                 "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(input, dim = 3)),
                 "Land categories remain unchanged")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-5, "Total areas in output stay constant over time")
  toolExpectLessDiff(outSum, dimSums(input, dim = 3), 10^-5, "Total areas remain unchanged")
  toolExpectTrue(all(out[, -1, c("primf", "primn")] <= setYears(out[, -nyears(out), c("primf", "primn")],
                                                                getYears(out[, -1, ]))),
                 "primf and primn are never expanding", falseStatus = "warn")

  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Harmonized land data"))
}
