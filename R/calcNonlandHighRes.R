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
  out <- lapply(seq_along(getNames(xInput)), function(i) {
    category <- getNames(xInput)[[i]]
    message(i, "/", length(getNames(xInput)), " ", category)
    x <- as.SpatVector(xInput[, , category])
    x$.region <- NULL
    x$.id <- NULL

    # calculate weight sum in each cluster
    clusterTotal <- terra::extract(weight[[category]], x, ID = FALSE, fun = "sum", na.rm = TRUE)

    # divide input cluster values by weight sum, now it only needs to be multiplied by weight and we're done
    for (n in names(x)) {
      x[[n]] <- x[[n]] / clusterTotal
    }

    factorRaster <- do.call(c, lapply(names(x), function(name) terra::rasterize(x, weight[[category]], name)))
    result <- factorRaster * weight[[category]]
    terra::time(result, tstep = "years") <- as.integer(sub("^y([0-9]+)\\.\\..+$", "\\1", names(result)))
    return(result)
  })
  out <- do.call(c, out)
  # write tif to reduce memory usage when loading from cache
  out <- terra::writeRaster(out, file = tempfile(fileext = ".tif"))

  return(list(x = out,
              class = "SpatRaster",
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha; fertilizer: kg yr-1",
              description = "Downscaled nonland data"))
}
