calcMagpieStatesLUH <- function() {
  x <- calcOutput("LandHighRes", input = "magpie", target = "luh2",
                  downscaling = "magpieClassic", aggregate = FALSE)
  stopifnot(" unit: Mha" %in% comment(x))
  cellArea <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
  cellArea <- cellArea[getItems(cellArea, 1) %in% getItems(x, 1), , ]
  stopifnot(all.equal(getItems(x, 1), getItems(cellArea, 1)))
  cellArea <- collapseDim(cellArea, 3)
  x <- x * 10000 / cellArea # * 10000 to convert from Mha to km2, / cellArea to get shares
  return(list(x = x,
              isocountries = FALSE,
              unit = "1",
              min = 0,
              max = 1.0001,
              description = "MAgPIE land use data downscaled to LUH2 resolution"))
}
