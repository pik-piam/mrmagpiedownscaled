calcLandHarmonized <- function(input = "magpie", target = "luh2",
                               harmonizeYear = 1995, finalYear = 2015, method = "fade") {
  input    <- toolAddCheckReport(calcOutput("LandHarmonizedCategories", input = input,
                                            target = target, aggregate = FALSE))
  geometry <- attr(input, "geometry")
  crs      <- attr(input, "crs")

  target   <- toolAddCheckReport(calcOutput("LandTargetData", target = target, aggregate = FALSE))
  # bring target data to spatial resolution of input data
  ref    <- as.SpatVector(input[, 1, 1])[, c(".region", ".id")]
  target <- terra::extract(target, ref, sum, na.rm = TRUE, bind = TRUE)
  target <- as.magpie(target)
  stopifnot(setequal(getItems(input, 3), getItems(target, 3)))
  target <- target[, , getItems(input, 3)] # harmonize order of dim 3

  # check  input data for consistency
  toolCheck("Land Harmonized input", {
    inSum <- dimSums(input, dim = 3)
    tSum <- dimSums(target, dim = 3)
    toolExpectLessDiff(inSum, inSum[, 1, ], 10^-5, "Total areas in input stay constant over time")
    toolExpectLessDiff(tSum, tSum[, 1, ], 10^-5, "Total areas in target stay constant over time")
    toolExpectLessDiff(inSum[, 1, ], tSum[, 1, ], 10^-5, "Total areas are the same in target and input data")
    if (max(abs(inSum[, 1, ] - tSum[, 1, ])) >= 10^-5) {
      corr <- setYears(dimSums(target[, 1, ], dim = 3) / dimSums(input[, 1, ], dim = 3), NULL)
      input <- input * corr
      vcat(1, "[!] input data multiplied with correction factors to match target areas (max ratio = ",
           round(max(corr), 2), ", min ratio = ", round(min(corr), 2),  ")", show_prefix = FALSE)
    }
  })

  if (method == "offset") {
    out <- toolHarmonizeOffset(input, target, harmonizeYear = harmonizeYear, finalYear = finalYear)
  } else if (method == "fade") {
    out <- toolHarmonizeFade(input, target, harmonizeYear = harmonizeYear, finalYear = finalYear)
  } else {
    stop("Unexpected harmonization method: ", method)
  }
  attr(out, "geometry") <- geometry
  attr(out, "crs")      <- crs

  # check  input data for consistency
  toolCheck("Land Harmonized output", {
    toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
    toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
    toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
    toolExpectTrue(setequal(getItems(out, dim = 3), getItems(input, dim = 3)), "Land categories remain unchanged")
    toolExpectTrue(all(out >= 0), "All values are >= 0")
    outSum <- dimSums(out, dim = 3)
    toolExpectLessDiff(outSum, outSum[, 1, ], 10^-5, "Total areas in output stay constant over time")
    toolExpectLessDiff(outSum, dimSums(input, dim = 3), 10^-5, "Total areas remain unchanged")
  })

  attr(out, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Harmonized data"))
}
