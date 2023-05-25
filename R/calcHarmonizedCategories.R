calcHarmonizedCategories <- function(input = "magpie", target = "luh2") {
  if (input == "magpie") {
    x <- madrat::readSource("Magpie")
    input2ref <- toolGetMapping("magpie2ref.csv", where = "mrdownscale")
  } else {
    stop("Unsupported input type \"", input, "\"")
  }
  if (target == "luh2") {
    output2ref <- toolGetMapping("luh2ref.csv", where = "mrdownscale")
  } else {
    stop("Unsupported output type \"", target, "\"")
  }

  # merge input and output map
  .getMap <- function(input2ref, output2ref) {
    if (!setequal(input2ref$reference, output2ref$reference)) {
      warning("Input map and output map contain inconsistent reference information")
    }
    map <- merge(input2ref, output2ref, by = "reference", suffixes = c("Input", "Output"))
    map$merge <- paste(map$dataInput, map$dataOutput, sep = "_")
    if (anyDuplicated(map$reference)) {
      warning("Insuficient granularity of reference categories, as a reference category is mapped more than once (\"",
              paste(unique(map$reference[duplicated(map$reference)]), collapse = "\", \""), "\").")
    }
    return(map)
  }
  map <- .getMap(input2ref, output2ref)

  # get weights for disaggregation to reference categories
  ref <- calcOutput("CategorizationWeight", map = map, geometry = attr(x, "geometry"),
                    crs = attr(x, "crs"), aggregate = FALSE)

  y   <- toolAggregate(x, map, dim = 3, from = "dataInput", to = "merge", weight = ref)
  out <- toolAggregate(y, map, dim = 3, from = "merge",     to = "dataOutput")

  attr(out, "crs") <- attr(x, "crs")
  attr(out, "geometry") <- attr(x, "geometry")

  # tests
  tryCatch(testthat::test_that("data fullfills format requirement", {
    testthat::expect_identical(unname(getSets(out)), c("region", "id", "year", "data"))
    testthat::expect_true(all(out >= 0))

    # check for expected land categories
    testthat::expect_setequal(getItems(out, dim = 3), map$dataOutput)

    # check for constant total areas
    outSum <- dimSums(out, dim = 3)
    testthat::expect_lt(max(abs(outSum - outSum[, 1, ])), 10^-5)
    xSum <- dimSums(x, dim = 3)
    testthat::expect_lt(max(abs(outSum - xSum)), 10^-5)
  }), error = function(e) warning(e))

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Input data with land categories remapped to categories of output target data set"))
}
