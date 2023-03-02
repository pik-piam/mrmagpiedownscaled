readMagpie <- function() {
  stop("TODO read in magpie output, convert to *.nc (netcdf)")
  return(list(x = terra::rast("magpie_states.nc"), class = "SpatRaster"))
}
