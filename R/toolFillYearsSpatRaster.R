#' toolFillYearsSpatRaster
#'
#' Fill data for missing years using linear interpolation.
#'
#' @param x SpatRaster with years in layer names
#' @param years data for these years will be added if they are not already present,
#' if years is NULL all years between the first and last year in x will be filled
#' @return SpatRaster with filled years
#' @author Pascal Sauer
toolFillYearsSpatRaster <- function(x, years = NULL) {
  stopifnot(grepl("^y[0-9]{4}\\.\\.", names(x)[[1]]))
  category <- sub("^y[0-9]{4}\\.\\.", "", names(x)[[1]])
  if (any(is.na(terra::time(x)))) {
    terra::time(x, tstep = "years") <- as.integer(substr(names(x), 2, 5))
  }
  xYears <- terra::time(x)
  if (is.unsorted(xYears)) {
    stop("xYears must be sorted")
  }
  years <- if (is.null(years)) min(xYears):max(xYears) else sort(years)
  out <- lapply(years, function(year) {
    if (year %in% xYears) {
      return(toolSelectYear(x, year))
    }
    # find the last xYear smaller than `year`
    a <- Find(function(y) y < year, xYears, right = TRUE)
    # find the first xYear bigger than `year`
    b <- Find(function(y) y > year, xYears)
    p <- (year - a) / (b - a)
    stopifnot(length(p) == 1, p >= 0, p <= 1)
    # interpolate between anchor years a and b
    out <- (1 - p) * toolSelectYear(x, a) + p * toolSelectYear(x, b)
    names(out) <- paste0("y", year, "..", category)
    return(out)
  })
  return(do.call(c, out))
}

toolSelectYear <- function(x, year) {
  stopifnot(inherits(x, "SpatRaster"),
            length(year) == 1)
  if (any(is.na(terra::time(x)))) {
    if (all(grepl("^y[0-9]{4}\\.\\..+$", names(x)))) {
      terra::time(x, tstep = "years") <- as.integer(substr(names(x), 2, 5))
    } else {
      stop("terra::time is not set and cannot determine years from names")
    }
  }
  out <- x[[terra::time(x) == year]]
  stopifnot(terra::nlyr(out) == 1)
  return(out)
}
