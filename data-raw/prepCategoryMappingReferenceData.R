# following the workflow proposed by https://r-pkgs.org/data.html#sec-data-sysdata
# code to prepare the internal datasets for category mapping
withr::with_package("mrcommons", {
  fao <- collapseDim(readSource("FAO_online", "Crop")[, 2019, "area_harvested"])
  getYears(fao) <- NULL
  faoFodder <- collapseDim(readSource("FAO_online", "Fodder")[, 2011, "area_harvested"])
  getYears(faoFodder) <- NULL
  fao <- mbind(fao, faoFodder)

  renameElements <- c("108|Cereals nes" = "108|Cereals, nes",
                      "149|Roots and tubers nes" =  "149|Roots and tubers, nes",
                      "723|Spices nes" = "723|Spices, nes",
                      "211|Pulses nes" = "211|Pulses, nes",
                      "234|Nuts nes" =  "234|Nuts, nes",
                      "489|Plantains and others" = "489|Plantains",
                      "693|Cinnamon (cannella)" = "693|Cinnamon (canella)")

  for (r in names(renameElements)) {
    getItems(fao, dim = 3)[which(getItems(fao, dim = 3) == r)] <- renameElements[r]
  }

  remove <- c("1717|Cereals, Total", "1804|Citrus Fruit, Total", "1753|Fibre Crops Primary",
              "1738|Fruit Primary", "1731|Oilcrops", "1841|Oilcrops, Cake Equivalent",
              "1732|Oilcrops, Oil Equivalent", "1726|Pulses, Total", "1720|Roots and Tubers, Total",
              "1723|Sugar Crops Primary", "1729|Treenuts, Total", "1735|Vegetables Primary",
              "826|Tobacco, unmanufactured", "782|Bastfibres, other", "773|Flax fibre and tow",
              "789|Sisal", "821|Fibre crops nes", "777|Hemp tow waste", "813|Coir",
              "780|Jute", "836|Rubber, natural", "161|Sugar crops nes", "754|Pyrethrum, dried",
              "788|Ramie", "800|Agave fibres nes", "809|Manila fibre (abaca)")
  fao <- fao[, , remove, invert = TRUE]

})

saveRDS(fao, file = "inst/extdata/faoAreaHarvested.rds")

# get country geometry
withr::with_package("mrorganic", {
  countries <- calcOutput("WorldCountries", aggregate = FALSE)
  geometryCountries <- attr(as.magpie(countries), "geometry")
})

saveRDS(geometryCountries, file = "inst/extdata/geometryCountries.rds")
