calcNonlandInputData <- function(input = "magpie") {
  if (input == "magpie") {
    wood <- readSource("Magpie", subtype = "woodHarvest")
    geometry <- attr(wood, "geometry")
    crs <- attr(wood, "crs")

    stopifnot(!is.na(wood),
              identical(getNames(wood), c("wood", "woodfuel")))
    getNames(wood) <- c("rndwd", "fulwd")
    wood <- wood / dimSums(wood, dim = 3)
    wood[is.na(wood)] <- 0 # replace NAs introduced by division by zero

    fertilizer <- readSource("Magpie", subtype = "fertilizer")
    cropArea <- readSource("Magpie", subtype = "land")[, , "crop"]
    # convert from Tg Nr yr-1 to kg ha-1 yr-1
    fertilizer <- fertilizer / collapseDim(cropArea) / 1e9
    # clusters without crop area are NA, replace with 0
    fertilizer[is.na(fertilizer)] <- 0
    fertilizer[fertilizer < 0] <- 0 # TODO there should not be negative values
    getItems(fertilizer, 3) <- paste0("fertl_", getItems(fertilizer, 3))

    out <- mbind(wood, fertilizer)
    attr(out, "geometry") <- geometry
    attr(out, "crs") <- crs
  } else {
    stop("Unsupported input dataset \"", input, "\"")
  }

  # check data for consistency
  toolCheck("Nonland input data", {
    toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
    toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
    toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
    toolExpectTrue(all(out >= 0), "All values are >= 0")
    toolExpectTrue(all(out[, , c("rndwd", "fulwd")] <= 1.0001), "All shares are < 1.0001")
  })
  attr(out, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = out,
              isocountries = FALSE,
              unit = "rndwd, fulwd: 1, fertl_*: kg ha-1 yr-1",
              min = 0,
              description = "Nonland input data for data harmonization and downscaling pipeline"))
}
