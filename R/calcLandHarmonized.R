calcLandHarmonized <- function(input = "magpie", target = "luh2",
                           harmonizeYear = 1995, finalYear = 2015, method = "fade") {
  input <- calcOutput("LandHarmonizedCategories", input = input, target = target, aggregate = FALSE)
  geometry <- attr(input, "geometry")
  crs      <- attr(input, "crs")

  # get target data
  if (target == "luh2") {
    target <- madrat::readSource("LUH2v2h")
  } else {
    stop("Unsupported output type \"", target, "\"")
  }

  # bring target data to spatial resolution of input data
  ref    <- as.SpatVector(input[, 1, 1])[, c(".region", ".id")]
  target <- terra::extract(target, ref, sum, na.rm = TRUE, bind = TRUE)
  target <- as.magpie(target)
  stopifnot(setequal(getItems(input, 3), getItems(target, 3)))
  target <- target[, , getItems(input, 3)] # harmonize order of dim 3

  # correct for differences in areas
  corr <- setYears(dimSums(target[, 1, ], dim = 3) / dimSums(input[, 1, ], dim = 3), NULL)
  if (max(corr) > 1.01) warning("Total areas differ significantly. (max ratio = ", round(max(corr), 2), ")")
  if (min(corr) < 0.99) warning("Total areas differ significantly. (min ratio = ", round(min(corr), 2), ")")
  input <- input * corr
  message("Inputs have been multiplied by area correction factor to match total target areas")

  # check  input data for consistency
  sep <- paste(rep("-", 100), collapse = "")
  vcat(1, sep, show_prefix = FALSE)
  vcat(1, "Check LandHarmonized input", show_prefix = FALSE)
  inSum <- dimSums(input, dim = 3)
  tSum <- dimSums(target, dim = 3)
  toolExpectLessDiff(inSum, inSum[, 1, ], 10^-5, "Total areas in input stay constant over time")
  toolExpectLessDiff(tSum, tSum[, 1, ], 10^-5, "Total areas in target stay constant over time")
  toolExpectLessDiff(inSum[, 1, ], tSum[, 1, ], 10^-5, "Total areas are the same in target and input data")
  vcat(1, sep, show_prefix = FALSE)

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
  sep <- paste(rep("-", 100), collapse = "")
  vcat(1, sep, show_prefix = FALSE)
  vcat(1, "Check LandHarmonized output", show_prefix = FALSE)
  toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
  toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(input, dim = 3)), "Land categories remain unchanged")
  toolExpectTrue(all(out >= 0), "All values are > 0")
  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-5, "Total areas in output stay constant over time")
  toolExpectLessDiff(outSum, dimSums(input, dim = 3), 10^-5, "Total areas remain unchanged")
  vcat(1, sep, show_prefix = FALSE)

  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Harmonized data"))
}
