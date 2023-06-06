withr::with_package("mrcommons", {
  # TODO switch to "lpjcell" when magclass::as.SpatRaster works with 67k cells
  rice <- calcOutput("Ricearea", cellular = TRUE, cells = "magpiecell",
                     aggregate = FALSE, share = FALSE)
  rice <- rice[, getYears(rice, TRUE) >= 1995, ]
  floodedShare <- rice[, , "flooded"] / setNames(rice[, , "total"], NULL)
  stopifnot(min(floodedShare, na.rm = TRUE) >= 0,
            max(floodedShare, na.rm = TRUE) < 1.0001)
})
write.magpie(floodedShare, "inst/extdata/floodedShareRice.mz")
