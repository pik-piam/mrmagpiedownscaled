withr::with_package("mrcommons", {
  x <- calcOutput("LanduseInitialisation", cellular = TRUE, cells = "lpjcell",
                  selectyears = seq(1995, 2015, 5), aggregate = FALSE)
  x <- x[, , c("forestry", "secdforest")]
  forestrySecdforest <- dimSums(x)
  x <- x[, , "forestry"] / forestrySecdforest # calc the share forestry / (forestry + secdforest)
  x[forestrySecdforest == 0] <- 0 # replace expected NAs with 0
  write.magpie(x, "inst/extdata/forestryShare.mz")
})
