
readMagpie <- function() {

  "!# @monitor magpie4:::addGeometry"

  stopifnot(file.exists("fulldata.gdx"),
            length(Sys.glob("clustermap_*.rds")) == 1)

  clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
  landUse    <- magpie4::land("fulldata.gdx", level = "cell")
  cropArea   <- magpie4::croparea("fulldata.gdx", level = "cell", product_aggr = FALSE)

  x <- magclass::mbind(landUse, cropArea)
  x <- x[, , "crop", invert = TRUE] # remove crop to avoid double counting of areas
  x <- magpie4::addGeometry(x, clustermap)
  getSets(x) <- c("region", "id", "year", "data") # fix spatial set names

  testthat::test_that("data fullfills format requirement", {
    testthat::expect_identical(unname(getSets(x)), c("region", "id", "year", "data"))
    testthat::expect_true(all(x >= 0))

    # check for expected land categories
    mag2ref <- toolGetMapping("magpie2ref.csv", where = "mrdownscale")
    testthat::expect_setequal(getItems(x, dim = 3), mag2ref$data)

    # check for constant total areas
    xSum <- dimSums(x, dim = 3)
    testthat::expect_lt(max(abs(xSum - xSum[, 1, ])), 10^-5)
  })

  return(list(x = x,
              class = "magpie",
              unit = "Mha",
              description = "Land cover information computed by MAgPIE"))
}
