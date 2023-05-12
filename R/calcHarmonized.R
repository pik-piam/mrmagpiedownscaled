calcHarmonized <- function(harmonizationStart = 1995, harmonizationEnd = 2040) {
  magpie <- readSource("Magpie") # TODO change to calcOutput("HarmonizedCategories")
  # TODO reformat magpie to fit harmonized interface

  luhVector <- calcOutput("LowResLUH2v2h", aggregate = FALSE)
  stopifnot(identical(luhVector[["cluster", drop = TRUE]], 1:200))
  luh <- NULL
  for (colName in grep("cluster", names(luhVector), value = TRUE, invert = TRUE)) {
    year <- as.integer(strsplit(colName, "..", fixed = TRUE)[[1]][[1]])
    category <- strsplit(colName, "..", fixed = TRUE)[[1]][[2]]
    luh <- rbind(luh, data.frame(region = 1:200, period = year, variable = category,
                                 value = luhVector[[colName, drop = TRUE]]))
  }
  luh$region <- as.character(luh$region)
  luh$scenario <- "scenario"
  luh$model <- "LUH"

  harmonized <- mip::harmonize(magpie, luh, harmonizeYear = harmonizationStart,
                               finalYear = harmonizationEnd, method = "ratio")

  years <- unique(harmonized$period)
  repeatedVariables <- rep(unique(harmonized$variable), length(years))
  namesHarmonized <- paste0(years, "..", repeatedVariables)
  columns <- Map(function(year, variable) {
    stopifnot(identical(
      harmonized[harmonized$period == year & harmonized$variable == variable, "region", drop = TRUE],
      as.character(1:200)
    ))
    return(harmonized[harmonized$period == year & harmonized$variable == variable, "value", drop = TRUE])
  }, years, repeatedVariables)
  harmonizedDataframe <- as.data.frame(columns)
  names(harmonizedDataframe) <- namesHarmonized
  harmonizedDataframe$cluster <- 1:200

  out <- terra::merge(luhVector["cluster"], harmonizedDataframe)
  return(out)
}
