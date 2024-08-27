# TODO documentation
calcLandTargetLowRes <- function(input = "magpie", target = "luh2mod") {
  xInput <- calcOutput("LandInputRecategorized", input = input,
                       target = target, aggregate = FALSE)
  xTarget <- calcOutput("LandTarget", target = target, aggregate = FALSE)

  # bring target data to spatial resolution of input data
  ref    <- as.SpatVector(xInput[, 1, 1])[, c(".region", ".id")]
  xTarget <- terra::extract(xTarget, ref, "sum", na.rm = TRUE, bind = TRUE)
  xTarget <- as.magpie(xTarget)
  stopifnot(setequal(getItems(xInput, 3), getItems(xTarget, 3)))
  out <- xTarget[, , getItems(xInput, 3)] # harmonize order of dim 3

  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")),
                 "Dimensions are named correctly")
  map <- toolLandCategoriesMapping(input = input, target = target)
  toolExpectTrue(setequal(getItems(out, dim = 3), map$dataOutput),
                 "Land target categories match the corresponding mapping")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-5, "Total area is constant over time")
  toolExpectTrue(all(out[, -1, c("primf", "primn")] <= setYears(out[, -nyears(out), c("primf", "primn")],
                                                                getYears(out[, -1, ]))),
                 "primf and primn are never expanding")

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Land target data at the same low resolution as the input dataset for harmonization"))
}
