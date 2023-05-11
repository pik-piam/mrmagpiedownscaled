readLUH2v2h <- function(subset = "1995|2000|2005|2010|2015") {
  x <- terra::rast("states.nc")

  # remove secma & secmb
  x <- x[[grep("secm[ab]", names(x), invert = TRUE, value = TRUE)]]

  # fix years in names
  names(x) <- paste0(terra::time(x),"..",sub("_.+", "", names(x)))

  x <- x[as.character(subset)]

  return(list(x = x,
              class = "SpatRaster",
              cache = FALSE,
              unit = "1"))
}
