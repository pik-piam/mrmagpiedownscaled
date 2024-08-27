#' calcNonlandHighRes
#'
#' This function performs the downscaling: It calculates a high resolution dataset
#' from the low resolution input dataset and the high resolution target dataset.
#'
#' @param input name of an input dataset, currently only "magpie"
#' @param target name of a target dataset, currently only "luh2mod"
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @return downscaled nonland data
#' @author Pascal Sauer
calcNonlandHighRes <- function(input = "magpie", target = "luh2mod", harmonizationPeriod = c(2015, 2050)) {
  xInput <- calcOutput("NonlandHarmonized", input = input, target = target,
                       harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)

  xTarget <- calcOutput("NonlandTarget", target = target, aggregate = FALSE)

  stopifnot(harmonizationPeriod[1] %in% terra::time(xTarget))
  weight <- xTarget[[terra::time(xTarget) == harmonizationPeriod[1]]]
  weight <- weight + 10^-10 # add 10^-10 to prevent weight 0
  names(weight) <- sub("^y[0-9]+\\.\\.", "", names(weight))
  stopifnot(setequal(getNames(xInput), names(weight)))

  # simple weighted disaggregation
  out <- do.call(mbind, lapply(seq_along(getNames(xInput)), function(i) {
    category <- getNames(xInput)[[i]]
    message(i, "/", length(getNames(xInput)), " ", category)
    x <- as.SpatVector(xInput[, , category])
    x$.region <- NULL
    x$.id <- NULL

    # calculate weight sum in each cluster
    clusterTotal <- terra::extract(weight[[category]], x, ID = FALSE, fun = "sum", na.rm = TRUE)
    stopifnot(all(clusterTotal[[1]] > 0))

    # divide input cluster values by weight sum, now it only needs to be multiplied by weight and we're done
    for (n in names(x)) {
      x[[n]] <- x[[n]] / clusterTotal
    }

    factorRaster <- do.call(c, lapply(names(x), function(name) {
      return(terra::rasterize(x, weight[[category]], field = name, fun = "max"))
    }))
    result <- as.magpie(factorRaster * weight[[category]])

    return(result)
  }))

  inSum <- dimSums(xInput, dim = 1)
  outSum <- dimSums(out, dim = 1)

  maxdiff <- max(abs(inSum - outSum) / inSum, na.rm = TRUE)
  toolExpectTrue(maxdiff < 10^-5,
                 paste0("Relative global sum difference per category before and after downscaling < 10^-5 ",
                        "(max relative diff: ", signif(maxdiff, 2), ")"))
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(xInput, dim = 3)),
                 "Nonland categories remain unchanged")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")
  outRaster <- as.SpatRaster(out[, harmonizationPeriod[1], ])
  deviation <- terra::extend(outRaster, xTarget) - xTarget[[names(outRaster)]]
  toolExpectLessDiff(max(abs(terra::minmax(deviation))), 0, 10^-5,
                     paste("In", harmonizationPeriod[1], "downscaled data equals target data"))

  land <- calcOutput("LandHighRes", input = input, target = target,
                     harmonizationPeriod = harmonizationPeriod, aggregate = FALSE)
  toolCheckWoodHarvestArea(out[, getYears(land), grep("wood_harvest_area$", getItems(out, 3))], land)

  return(list(x = out,
              min = 0,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg yr-1",
              description = "Downscaled nonland data"))
}
# TODO use woody land (instead of historical harvest_area) as weight for harvest_area
# TODO distribute bioh according to harvest_area