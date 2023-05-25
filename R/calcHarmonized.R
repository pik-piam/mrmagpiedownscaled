calcHarmonized <- function(input = "magpie", target = "luh2",
                           harmonizeYear = 1995, finalYear = 2015, method = "fade") {
  input <- calcOutput("HarmonizedCategories", input = input, target = target, aggregate = FALSE)
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

  tryCatch(testthat::test_that("input fullfills requirements", {
    inSum <- dimSums(input, dim = 3)
    tSum <- dimSums(target, dim = 3)

    # ensure cluster areas in input are consistent over the years
    testthat::expect_lt(max(abs(inSum - inSum[, 1, ])), 0.001)

    # # ensure cluster areas in target are consistent over the years
    testthat::expect_lt(max(abs(tSum - tSum[, 1, ])), 0.001)

    # ensure cluster areas in input are equal to those in target
    testthat::expect_equal(inSum[, 1, ], tSum[, 1, ], tolerance = 0.001)
  }), error = function(e) warning(e))

  if (method == "offset") {
    out <- toolHarmonizeOffset(input, target, harmonizeYear = harmonizeYear, finalYear = finalYear)
  } else if (method == "fade") {
    out <- toolHarmonizeFade(input, target, harmonizeYear = harmonizeYear, finalYear = finalYear)
  } else {
    stop("Unexpected harmonization method: ", method)
  }

  attr(out, "geometry") <- geometry
  attr(out, "crs")      <- crs

  tryCatch(testthat::test_that("output fullfills requirements", {

    testthat::expect_type(attr(out, "geometry"), "character")
    testthat::expect_type(attr(out, "crs"), "character")

    testthat::expect_identical(unname(getSets(out)), c("region", "id", "year", "data"))
    testthat::expect_gte(min(out), 0)

    # check for expected land categories
    testthat::expect_setequal(getItems(out, dim = 3), getItems(input, dim = 3))

    # check for constant total areas
    outSum <- dimSums(out, dim = 3)
    testthat::expect_lt(max(abs(outSum - outSum[, 1, ])), 0.001)
    inSum <- dimSums(input, dim = 3)
    testthat::expect_lt(max(abs(outSum - inSum)), 0.001)
  }), error = function(e) warning(e))

  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Harmonized data"))
}
