#' calcNonlandInput
#'
#' Prepare the nonland input data for category mapping, checking data for consistency before returning.
#'
#' All "Land" functions deal with area data, as opposed to "Nonland" functions which deal with non-area
#' data such as the amount of applied fertilizer. These are treated differently, because for area
#' data other constraints apply, e.g. the total area must be constant over time.
#'
#' @param input name of an input dataset, currently only "magpie"
#' @return nonland input data
#' @author Pascal Sauer
calcNonlandInput <- function(input = "magpie") {
  if (input == "magpie") {
    # wood <- readSource("Magpie", subtype = "woodHarvest")

    # stopifnot(!is.na(wood),
    #           identical(getNames(wood), c("wood", "woodfuel")))
    # getNames(wood) <- c("rndwd", "fulwd")
    # wood <- wood / dimSums(wood, dim = 3)
    # wood[is.na(wood)] <- 0.5 # assume equal shares for cells without wood harvest, so they still sum up to 1

    fertilizer <- readSource("Magpie", subtype = "fertilizer")
    geometry <- attr(fertilizer, "geometry")
    crs <- attr(fertilizer, "crs")
    # clusters without crop area are NA, replace with 0
    fertilizer[is.na(fertilizer)] <- 0
    # there are some negative values very close to zero, replace with 0
    stopifnot(min(fertilizer) >= -10^-10)
    fertilizer[fertilizer < 0] <- 0
    # convert from Tg yr-1 to kg yr-1
    fertilizer <- fertilizer * 10^9
    getItems(fertilizer, 3) <- paste0(getItems(fertilizer, 3), "_fertilizer")

    out <- mbind(#wood,
                 fertilizer)
    attr(out, "geometry") <- geometry
    attr(out, "crs") <- crs
  } else {
    stop("Unsupported input dataset \"", input, "\"")
  }

  # check data for consistency
  toolExpectTrue(!is.null(attr(out, "geometry")), "Data contains geometry information")
  toolExpectTrue(!is.null(attr(out, "crs")), "Data contains CRS information")
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "data")), "Dimensions are named correctly")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  # toolExpectTrue(all(round(dimSums(out[, , c("rndwd", "fulwd")]), 14) == 1), "Wood harvest shares sum up to 1")

  return(list(x = out,
              isocountries = FALSE,
              unit = "rndwd, fulwd: 1; *_fertilizer: kg yr-1",
              min = 0,
              description = "Nonland input data for data harmonization and downscaling pipeline"))
}
