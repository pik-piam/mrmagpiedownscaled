# following the workflow proposed by https://r-pkgs.org/data.html#sec-data-sysdata
# code to prepare the internal datasets for category mapping
withr::with_package("mrorganic", {
  countries <- calcOutput("WorldCountries", aggregate = FALSE)
  geometryCountries <- attr(as.magpie(countries), "geometry")
})

saveRDS(geometryCountries, file = "inst/extdata/geometryCountries.rds")
