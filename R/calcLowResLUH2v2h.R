calcLowResLUH2v2h <- function() {
  x <- madrat::readSource("LUH2v2h", supplementary = TRUE)
  stopifnot(x$unit == "Mha")
  x <- x$x

  clusters <- madrat::readSource("Magpie")["clusterId"]
  clusters <- clusters[!duplicated(clusters$clusterId), ]

  # calc category shares for each cluster
  x <- terra::extract(x, clusters, sum, na.rm = TRUE, bind = TRUE)

  return(list(x = x,
              class = "SpatVector",
              unit = "Mha",
              description = "LUH2v2h data on cluster resolution"))
}
