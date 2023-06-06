withr::with_package("mrcommons", {
  riceHist <- calcOutput("Ricearea", cellular = TRUE,
                         cells = "lpjcell", # = 67k cells, use magpiecell for 59k cells
                         aggregate = FALSE, share = FALSE)
  write.magpie(riceHist, "inst/extdata/riceHistorical.mz")
})
