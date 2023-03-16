readMagpie <- function() {
  return(list(x = terra::vect("land_cluster.gpkg"), class = "SpatVector", cache = FALSE))
}
