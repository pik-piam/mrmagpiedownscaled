calcLowResLUH2v2h <- function() {
  luh <- madrat::readSource("LUH2v2h")
  luh2015 <- luh[[grep("1166", names(luh))]]
  names(luh2015) <- sub("_[0-9]+$", "", names(luh2015))
  # add clusterId to each cell
  # aggregate to clusters
}
