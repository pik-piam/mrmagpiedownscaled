readLUH2v2h <- function() {
  return(list(x = terra::rast("states.nc"), class = "SpatRaster"))
}
