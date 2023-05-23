calcHarmonized <- function(input = "magpie", target = "luh2",
                           harmonizeYear = 1995, finalYear = 2015, method = "fade") {
  input <- calcOutput("HarmonizedCategories", input = input, target = target, aggregate = FALSE)

  # get target data
  if (target == "luh2") {
    target <- madrat::readSource("LUH2v2h")
  } else {
    stop("Unsupported output type \"", target, "\"")
  }

  # bring target data to spatial resolution of input data
  ref    <- as.SpatVector(input[, , 1])[, 1:2]
  target <- terra::extract(target, ref, sum, na.rm = TRUE, bind = TRUE)
  target <- as.magpie(target)
  stopifnot(setequal(getItems(input, 3), getItems(target, 3)))
  target <- target[, , getItems(input, 3)] # harmonize order of dim 3

  if (method == "offset") {
    .df <- function(input) {
      df <- magclass::as.data.frame(input, rev = 3)
      stopifnot(identical(names(df), c("region", "id", "year", "data", ".value")))
      df <- df[, -1]
      names(df) <- c("region", "period", "variable", "value")
      df$region <- as.character(df$region)
      df$scenario <- "scenario"
      df$model <- "model"
      df$unit <- "Mha"
      return(df)
    }
    dInput  <- .df(input)
    dTarget <- .df(target)

    out <- mip::harmonize(dInput, dTarget,
                          harmonizeYear = as.character(harmonizeYear), finalYear = as.character(finalYear),
                          method = "offset")

    out <- as.magpie(out, spatial = "region", temporal = "period")
    getSets(out)[1] <- "id"
    spatElems <- getItems(input, dim = 1.1, full = TRUE)
    names(spatElems) <- getItems(input, dim = "id", full = TRUE)
    getItems(out, dim = "region", maindim = 1) <- unname(spatElems[getItems(out, dim = 1)])
    out <- collapseDim(magpiesort(dimOrder(out, 2:1, dim = 1)))
    getSets(out) <- c("region", "id", "year", "data")
  } else if (method == "fade") {
    out <- toolHarmonizeFade(input, target, harmonizeYear = harmonizeYear, finalYear = finalYear)
  } else {
    stop("Unexpected harmonization method: ", method)
  }

  attr(out, "geometry") <- attr(input, "geometry")
  attr(out, "crs")      <- attr(input, "crs")

  testthat::test_that("data fullfills format requirement", {
    testthat::expect_identical(unname(getSets(out)), c("region", "id", "year", "data"))
    testthat::expect_true(all(out >= 0))

    # check for expected land categories
    testthat::expect_setequal(getItems(out, dim = 3), getItems(input, dim = 3))

    # check for constant total areas
    outSum <- dimSums(out, dim = 3)
    testthat::expect_lt(max(abs(outSum - outSum[, 1, ])), 10^-3)
    inSum <- dimSums(input, dim = 3)
    testthat::expect_lt(max(abs(outSum - inSum)), 10^-3)
  })

  return(return(list(x = out,
                     class = "magpie",
                     isocountries = FALSE,
                     unit = "Mha",
                     min = 0,
                     description = "Harmonized data")))
}
