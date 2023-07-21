calcLandTargetData <- function(target = "luh2") {
  if (target == "luh2") {
    cropTypes <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per")

    states <- readSource("LUH2v2h", subtype = "states")
    states <- toolSpatRasterToDataset(states)
    man <- readSource("LUH2v2h", subtype = "management", convert = FALSE)
    man <- toolSpatRasterToDataset(man)
    man <- man[c(paste0("crpbf_", cropTypes), paste0("irrig_", cropTypes))]
    stopifnot(all.equal(terra::time(states[1]), terra::time(man[1])))

    out <- list()
    for (cropType in cropTypes) {
      crpbf <- man[paste0("crpbf_", cropType)]
      irrig <- man[paste0("irrig_", cropType)]
      stopifnot(all.equal(terra::time(crpbf), terra::time(states[cropType])),
                all.equal(terra::time(irrig), terra::time(states[cropType])))

      # crpbf_<cropType> = 1st gen biofuel crop share of <cropType>
      # -> get <cropType>_biofuel_1st_gen area in Mha by multiplying with <cropType>
      biofuel1stGen <- crpbf * states[cropType]

      irrigatedBiofuel1stGen <- irrig * biofuel1stGen
      rainfedBiofuel1stGen <- biofuel1stGen - irrigatedBiofuel1stGen
      names(irrigatedBiofuel1stGen) <- sub("\\.\\..+$", paste0("..", cropType, "_irrigated_biofuel_1st_gen"),
                                           names(irrigatedBiofuel1stGen))
      names(rainfedBiofuel1stGen) <- sub("\\.\\..+$", paste0("..", cropType, "_rainfed_biofuel_1st_gen"),
                                         names(rainfedBiofuel1stGen))

      nonBiofuel <- states[cropType] - biofuel1stGen

      rainfedBiofuel2ndGen <- NULL
      if (grepl("per", cropType)) {
        # 2nd gen biofuel is not part of LUH2v2h, but we need it for the harmonization, so fill with zeros
        rainfedBiofuel2ndGen <- 0 * states[cropType]
        names(rainfedBiofuel2ndGen) <- paste0(names(rainfedBiofuel2ndGen), "_rainfed_biofuel_2nd_gen")
        # we assume 2nd gen biofuel is never irrigated, so irrigatedBiofuel2ndGen does not exist
        nonBiofuel <- nonBiofuel - rainfedBiofuel2ndGen
      }

      irrigatedNonBiofuel <- irrig * nonBiofuel
      rainfedNonBiofuel <- nonBiofuel - irrigatedNonBiofuel
      names(irrigatedNonBiofuel) <- sub("\\.\\..+$", paste0("..", cropType, "_irrigated"), names(irrigatedNonBiofuel))
      names(rainfedNonBiofuel) <- sub("\\.\\..+$", paste0("..", cropType, "_rainfed"), names(rainfedNonBiofuel))

      out <- c(out, irrigatedNonBiofuel, rainfedNonBiofuel,
               irrigatedBiofuel1stGen, rainfedBiofuel1stGen, rainfedBiofuel2ndGen)
    }
    out <- do.call(c, out)
    terra::time(out, tstep = "years") <- as.integer(substr(names(out), 2, 5))
  } else {
    stop("Unsupported output type \"", target, "\"")
  }

  toolCheck("Land target data", {
    toolExpectTrue(terra::crs(out) != "", "Data contains CRS information")
    map <- toolLandCategoriesMapping(input = "magpie", target = target)
    toolExpectTrue(setequal(sub("y[0-9]+\\.\\.", "", names(out)), map$dataOutput),
                   "Land target categories match the corresponding mapping")
    toolExpectTrue(min(terra::values(out), na.rm = TRUE) >= 0, "All values are >= 0")
    totalAreas <- vapply(unique(terra::time(out)), function(year) {
      sum(terra::values(out[[terra::time(out) == year]]), na.rm = TRUE)
    }, double(1))
    toolExpectLessDiff(max(totalAreas), min(totalAreas), 10^-5,
                       "Total area is constant over time")
  })
  attr(out, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = out,
              class = "SpatRaster",
              unit = "Mha",
              description = "Land target data for data harmonization"))
}
