calcHarmonized <- function() {
  magpie <- readSource("Magpie") # TODO change to calcOutput("HarmonizedCategories")
  # TODO reformat magpie to fit harmonized interface

  luhVector <- calcOutput("LowResLUH2v2h", aggregate = FALSE)
  stopifnot(identical(luhVector[["cluster", drop = TRUE]], 1:200))
  luh <- NULL
  for (colName in grep("cluster", names(luhVector), value = TRUE, invert = TRUE)) {
    year <- as.integer(strsplit(colName, "..", fixed = TRUE)[[1]][[1]])
    category <- strsplit(colName, "..", fixed = TRUE)[[1]][[2]]
    luh <- rbind(luh, data.frame(cluster = 1:200, year = year, category = category,
                             .value = luhVector[[colName, drop = TRUE]]))
  }

  harmonized <- mip::harmonize(magpie, luh, finalYear = "2050", harmonizeYear = "1995", method = "ratio")
  # TODO turn harmonized dataframe into SpatVector
  return(harmonized)
}
