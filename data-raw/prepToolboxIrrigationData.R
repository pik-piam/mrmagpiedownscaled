raw <- terra::rast("harvested_area_GADM_timeseries_2000-2009_20200417_20200127.nc",
                   c("rainfed_harvested_area", "irrigated_harvested_area"))
raw <- raw[[terra::time(raw) == 2000]]
names(raw) <- sub("_1$", "", names(raw))
for (i in seq_len(terra::nlyr(raw))) {
  terra::aggregate(raw[[i]], 6, fun = "sum", filename = paste0("loop-", i, ".tif"), overwrite = TRUE)
  message(i, "/", terra::nlyr(raw), " done")
}

combined <- terra::rast(paste0("loop-", seq_along(Sys.glob("loop-*.tif")), ".tif"))
mag <- magclass::as.magpie(combined)

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
stopifnot(identical(sub("^.+=([0-9]+)$", "\\1", magclass::getItems(mag, 3)), as.character(rep(seq_along(crop), 2))))

magclass::getItems(mag, 3, raw = TRUE) <- magclass::getItems(mag, 3) |>
  gsub(pattern = "_crop=([0-9]+)$|_harvested_area", replacement = "") |>
  paste0(".", rep(crop, 2))
names(dimnames(mag))[3] <- "irrigation.crop"
magclass::write.magpie(mag, "inst/extdata/0p5.mz", overwrite = TRUE)
unlink(Sys.glob("loop-*.tif"))
