#' calcNonlandHighRes
#'
#' This function performs the downscaling: It calculates a high resolution dataset
#' from the low resolution input dataset and the high resolution target dataset.
#'
#' @param input name of an input dataset, currently only "magpie"
#' @param target name of a target dataset, currently only "luh2"
#' @return downscaled nonland data
#' @author Pascal Sauer
calcNonlandHighRes <- function(input = "magpie", target = "luh2") {
  xInput <- calcOutput("NonlandHarmonized", input = input, target = target, aggregate = FALSE)
  xTarget <- calcOutput("NonlandTargetData", target = target, aggregate = FALSE)

  # simple weighted disaggregation for fertl
  cropTypes <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per")
  downscaledFertilizer <- lapply(cropTypes, function(cropType) {
    fertilizer <- as.SpatVector(xInput[, , grep(paste0(cropType, "_fertilizer"), getNames(xInput))])
    fertilizer$.region <- NULL
    fertilizer$.id <- NULL

    # use latest year of historical data as weight
    weight <- xTarget[[terra::time(xTarget) == max(terra::time(xTarget))]]
    weight <- weight[paste0(cropType, "_fertilizer")]
    names(weight) <- sub("^y[0-9]+\\.\\.", "", names(weight))

    # calculate weight sum in each cluster
    x <- terra::extract(weight, fertilizer, bind = TRUE, fun = "sum", na.rm = TRUE)

    # divide input cluster values by weight sum, now it only needs to be multiplied by weight and we're done
    for (n in names(fertilizer)) {
      x[[n]] <- x[[n]] / x[[paste0(cropType, "_fertilizer")]]
    }
    x[[paste0(cropType, "_fertilizer")]] <- NULL

    factorRaster <- do.call(c, lapply(names(x), function(name) terra::rasterize(x, weight, name)))
    result <- factorRaster * weight
    terra::time(result, tstep = "years") <- as.integer(sub("^y([0-9]+)\\.\\..+$", "\\1", names(result)))
    return(result)
  })
  downscaledFertilizer <- do.call(c, downscaledFertilizer)
  out <- as.magpie(downscaledFertilizer)

  # TODO rndwd and fulwd

  return(list(x = out,
              isocountries = FALSE,
              unit = "*_fertilizer: kg yr-1",
              min = 0,
              description = "Downscaled nonland data"))
}
