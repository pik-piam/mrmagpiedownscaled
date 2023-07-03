calcNonlandHarmonized <- function(input = "magpie", target = "luh2",
                                  harmonizeYear = 2015, finalYear = 2050,
                                  method = "extrapolateFade") {
  input <- toolAddCheckReport(calcOutput("NonlandInputData", input = input, aggregate = FALSE))
  geometry <- attr(input, "geometry")
  crs <- attr(input, "crs")

  target <- toolAddCheckReport(calcOutput("NonlandTargetData", target = target, aggregate = FALSE))
  # bring target data to spatial resolution of input data
  ref    <- as.SpatVector(input[, 1, 1])[, c(".region", ".id")]
  target <- terra::extract(target, ref, sum, na.rm = TRUE, bind = TRUE)
  target <- as.magpie(target)
  stopifnot(setequal(getItems(input, 3), getItems(target, 3)))
  target <- target[, , getItems(input, 3)] # harmonize order of dim 3

  harmonizer <- toolGetHarmonizer(method)
  out <- harmonizer(input, target, harmonizeYear = harmonizeYear, finalYear = finalYear)

  attr(out, "geometry") <- geometry
  attr(out, "crs")      <- crs

  toolCheck("Nonland Harmonized output", {
    toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
    toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
    toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
    toolExpectTrue(setequal(getItems(out, dim = 3), getItems(input, dim = 3)), "Nonland categories remain unchanged")
    toolExpectTrue(all(out >= 0), "All values are >= 0")
    toolExpectTrue(all(out <= 1.0001), "All values are <= 1")
  })

  attr(out, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "1",
              min = 0,
              max = 1.0001,
              description = "Harmonized nonland data"))
}
