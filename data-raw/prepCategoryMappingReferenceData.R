# following the workflow proposed by https://r-pkgs.org/data.html#sec-data-sysdata
# code to prepare the internal datasets for category mapping
withr::with_package("mrcommons", {
  fao <- collapseDim(madrat::readSource("FAO_online", "Crop")[, 2019, "area_harvested"])
})

saveRDS(fao, file = "inst/extdata/faoAreaHarvested2019.rds")

# get country geometry
withr::with_package("mrorganic", {
  countries <- calcOutput("WorldCountries", aggregate = FALSE)
  geometryCountries <- attr(as.magpie(countries), "geometry")
})

saveRDS(geometryCountries, file = "inst/extdata/geometryCountries.rds")
