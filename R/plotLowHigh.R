#' plotLowHigh
#'
#' Plot a land variable in low (after harmonization) and high resolution (after
#' downscaling) for a given year, unit is area share.
#'
#' @param variable character, a regex that should match at least one variable
#' name in LandHighRes and LandHarmonized, if multiple are matched they are
#' summed up
#' @param year integer, year to plot
#' @param range range for plot legend
#' @param xlim min and max x coordinate to plot
#' @param ylim min and max y coordinate to plot
#' @examples
#' \dontrun{
#' plotLowHigh("c3ann", 2040, range = c(0, 0.9),
#'             ylim = c(60, 30), xlim = c(-10, 60))
#' }
#'
#' @author Pascal Sauer
#' @export
plotLowHigh <- function(variable, year, range = c(0, 1), xlim = c(-180, 180), ylim = c(-90, 90)) {
  cellArea <- readSource("LUH2v2h", "cellArea", convert = FALSE)

  landHighRes <- calcOutput("LandHighRes", aggregate = FALSE)
  variables <- grep(variable, getItems(landHighRes, 3), value = TRUE)
  if (length(variables) == 0) {
    stop("No variable matched ", variable, " in LandHighRes. Options are: ",
         paste(getItems(landHighRes, 3), collapse = ", "))
  }
  message("plotting ", paste(variables, collapse = ", "))
  high <- landHighRes[, year, variables]
  high <- dimSums(high, 3)
  highRaster <- as.SpatRaster(high)
  # multiply by 10000 to convert from Mha to km2, divide to get area share
  highRaster <- highRaster * 10000 / terra::crop(cellArea, highRaster)

  landHarmonized <- calcOutput("LandHarmonized", aggregate = FALSE)
  geometry <- attr(landHarmonized, "geometry")
  stopifnot(setequal(variables, grep(variable, getItems(landHarmonized, 3), value = TRUE)))
  low <- landHarmonized[, year, variables]
  low <- dimSums(low, 3)
  attr(low, "geometry") <- geometry
  lowVector <- as.SpatVector(low)
  clusterSize <- terra::extract(cellArea, lowVector, fun = sum) # in km2
  stopifnot(identical(clusterSize$ID, 1:200),
            identical(getItems(low, 1.2), as.character(1:200)))
  clusterSizeMag <- low
  clusterSizeMag[] <- clusterSize$carea
  # multiply by 10000 to convert from Mha to km2, divide to get area share
  low <- low * 10000 / clusterSizeMag
  attr(low, "geometry") <- geometry
  lowVector <- as.SpatVector(low)
  lowRaster <- terra::rasterize(lowVector, highRaster, paste0("y", year, ".."))

  grDevices::dev.new(width = 12, height = 9, noRStudioGD = TRUE)
  withr::with_par(list(mfrow = c(2, 1)), {
    terra::plot(lowRaster, range = range, mar = c(1, 1, 3, 0), xlim = xlim, ylim = ylim)
    graphics::title(paste0(variable, " in ", year, " (area share)"))
    terra::plot(highRaster, range = range, mar = c(3, 1, 1, 0), xlim = xlim, ylim = ylim)
  })
}
