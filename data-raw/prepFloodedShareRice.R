withr::with_package("mrcommons", {
  rice <- calcOutput("Ricearea", cellular = TRUE,
                     cells = "lpjcell", # = 67k cells, use "magpiecell" for 59k cells
                     aggregate = FALSE, share = FALSE)
  rice <- rice[, getYears(rice, TRUE) >= 1995, ]
  floodedShare <- rice[, , "flooded"] / setNames(rice[, , "total"], NULL)
  stopifnot(min(floodedShare, na.rm = TRUE) >= 0,
            max(floodedShare, na.rm = TRUE) < 1.0001)
})
write.magpie(floodedShare, "inst/extdata/floodedShareRice.mz")
