#' calcNonlandHighRes
#'
#' This function performs the downscaling: It calculates a high resolution dataset
#' from the low resolution input dataset and the high resolution target dataset.
#'
#' @param input name of an input dataset, currently only "magpie"
#' @param target name of a target dataset, currently only "luh2"
#' @return downscaled nonland data
#' @author Pascal Sauer
calcNonlandHighRes <- function(input = "magpie", target = "luh2mod") {
  xInput <- calcOutput("NonlandHarmonized", input = input, target = target, aggregate = FALSE)

  xTarget <- calcOutput("NonlandTarget", target = target, aggregate = FALSE)

  # use latest year of historical data as weight
  weight <- xTarget[[terra::time(xTarget) == max(terra::time(xTarget))]]
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

  return(list(x = out,
              min = 0,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg yr-1",
              description = "Downscaled nonland data"))
}
