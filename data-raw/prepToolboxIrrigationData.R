# the data was originally located at /p/projects/rd3mod/inputdata/sources/LandInG/landuse_paper/tmp/work_5min/
# note this is harvested area, physical area might be slightly better, but was not readily available
raw <- terra::rast(paste0("data-raw/raw/harvested_area_GADM_timeseries_2000-2009_20200417_20200127",
                          "_madrat_multicropping_LUH2v2_disaggregated.nc"),
                   c("rainfed_harvested_area", "irrigated_harvested_area"))
year <- 2000
raw <- raw[[terra::time(raw) == year]]
names(raw) <- sub("_1$", "", names(raw))
for (i in seq_len(terra::nlyr(raw))) {
  terra::aggregate(raw[[i]], 6, fun = "sum", filename = paste0("loop-", i, ".tif"), overwrite = TRUE)
  message(i, "/", terra::nlyr(raw), " done")
}

combined <- terra::rast(paste0("loop-", seq_along(Sys.glob("loop-*.tif")), ".tif"))
x <- magclass::as.magpie(combined)

crop <- c("Almonds, with shell", "Anise, badian, fennel, coriander", "Apples", "Apricots", "Barley", "Berries nes",
          "Figs", "Fruit, citrus nes", "Fruit, fresh nes", "Fruit, stone nes", "Grapes", "Linseed", "Maize",
          "Melons, other (incl cantaloupes)", "Millet", "Nuts nes", "Olives", "Onions, dry", "Oranges",
          "Peaches and nectarines", "Pears", "Pistachios", "Plums and sloes", "Potatoes", "Pulses nes", "Rice, paddy",
          "Seed cotton", "Sesame seed", "Spices nes", "Sugar beet", "Sugar cane", "Sunflower seed",
          "Vegetables, fresh nes", "Walnuts, with shell", "Watermelons", "Wheat", "Beans, dry", "Beans, green",
          "Broad beans, horse beans, dry", "Cabbages and other brassicas", "Carrots and turnips",
          "Cauliflowers and broccoli", "Cherries", "Cherries, sour", "Chestnut", "Chillies and peppers, green",
          "Cucumbers and gherkins", "Dates", "Eggplants (aubergines)", "Garlic", "Hops",
          "Leeks, other alliaceous vegetables", "Lemons and limes", "Lettuce and chicory", "Mushrooms and truffles",
          "Oats", "Okra", "Onions, shallots, green", "Peas, green", "Pumpkins, squash and gourds", "Quinces", "Rye",
          "Sorghum", "Soybeans", "Spinach", "Tangerines, mandarins, clementines, satsumas", "Tobacco, unmanufactured",
          "Tomatoes", "Vegetables, leguminous nes", "Vetches", "Artichokes", "Bananas", "Carobs", "Chick peas",
          "Chillies and peppers, dry", "Fruit, tropical fresh nes", "Grapefruit (incl pomelos)",
          "Groundnuts, with shell", "Lentils", "Peas, dry", "Rapeseed", "Strawberries", "Triticale", "Cassava",
          "Cocoa, beans", "Coconuts", "Maize, green", "Pineapples", "Taro (cocoyam)", "Yams", "Bastfibres, other",
          "Cashew nuts, with shell", "Castor oil seed", "Coffee, green", "Oil palm fruit", "Sisal", "Sweet potatoes",
          "Mangoes, mangosteens, guavas", "Asparagus", "Avocados", "Canary seed", "Cereals nes", "Fibre crops nes",
          "Flax fibre and tow", "Lupins", "Mate", "Oilseeds nes", "Papayas", "Peppermint", "Safflower seed",
          "String beans", "Tea", "Tung nuts", "Hazelnuts, with shell", "Blueberries", "Cow peas, dry", "Currants",
          "Kiwi fruit", "Mustard seed", "Persimmons", "Raspberries", "Buckwheat", "Gooseberries", "Grain, mixed",
          "Hemp tow waste", "Poppy seed", "Cranberries", "Pigeon peas", "Plantains and others", "Areca nuts", "Coir",
          "Ginger", "Jute", "Rubber, natural", "Sugar crops nes", "Chicory roots", "Roots and tubers nes",
          "Yautia (cocoyam)", "Fonio", "Karite nuts (sheanuts)", "Kola nuts", "Pepper (piper spp)",
          "Nutmeg, mace and cardamoms", "Brazil nuts, with shell", "Pyrethrum, dried", "Quinoa", "Cashewapple",
          "Gums, natural", "Ramie", "Hempseed", "Melonseed", "Bambara beans", "Cinnamon (cannella)", "Cloves",
          "Tallowtree seed", "Vanilla", "Agave fibres nes", "Manila fibre (abaca)", "Kapok fruit", "Jojoba seed",
          "Fruit, pome nes", "alfalfa", "beetfor", "cabbagefor", "carrotfor", "clover", "fornes", "grassnes",
          "legumenes", "maizefor", "mixedgrass", "oilseedfor", "popcorn", "ryefor", "sorghumfor", "swedefor",
          "turnipfor", "vegfor")
stopifnot(identical(sub("^.+=([0-9]+)$", "\\1", magclass::getItems(x, 3)), as.character(rep(seq_along(crop), 2))))
magclass::getItems(x, 3) <- paste0(rep(crop, 2), ", ", gsub("_.+$", "", magclass::getItems(x, 3)))
remove <- c("Tobacco, unmanufactured, irrigated", "Bastfibres, other, irrigated",
            "Sisal, irrigated", "Fibre crops nes, irrigated", "Flax fibre and tow, irrigated",
            "Hemp tow waste, irrigated", "Coir, irrigated", "Jute, irrigated",
            "Rubber, natural, irrigated", "Sugar crops nes, irrigated", "Pyrethrum, dried, irrigated",
            "Gums, natural, irrigated", "Ramie, irrigated", "Agave fibres nes, irrigated",
            "Manila fibre (abaca), irrigated", "Tobacco, unmanufactured, rainfed",
            "Bastfibres, other, rainfed", "Sisal, rainfed", "Fibre crops nes, rainfed",
            "Flax fibre and tow, rainfed", "Hemp tow waste, rainfed", "Coir, rainfed",
            "Jute, rainfed", "Rubber, natural, rainfed", "Sugar crops nes, rainfed",
            "Pyrethrum, dried, rainfed", "Gums, natural, rainfed", "Ramie, rainfed",
            "Agave fibres nes, rainfed", "Manila fibre (abaca), rainfed")
x <- x[, , remove, invert = TRUE]

# coords are stored as strings, round to prevent coords like "-79p75.-0p249999999999986"
getCoords(x) <- round(getCoords(x), 2)

magclass::write.magpie(x, paste0("inst/extdata/toolbox", year, ".mz"), overwrite = TRUE)
unlink(Sys.glob("loop-*.tif"))
