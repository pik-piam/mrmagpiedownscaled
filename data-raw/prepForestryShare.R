# as of 2023-09-25 this needs remotes::install_github("FelicitasBeier/mrcommons", ref="Clustering2")
# to get coordinates for 67k cells, which are required to convert to SpatRaster
withr::with_package("mrcommons", {
  x <- calcOutput("LanduseInitialisation", cellular = TRUE, cells = "lpjcell",
                  selectyears = seq(1995, 2015, 5), aggregate = FALSE)
  x <- x[, , c("forestry", "secdforest")]
  forestrySecdforest <- dimSums(x)
  x <- x[, , "forestry"] / forestrySecdforest # calc the share forestry / (forestry + secdforest)
  x[forestrySecdforest == 0] <- 0 # replace expected NAs with 0
  write.magpie(x, "inst/extdata/forestryShare.mz")
})
