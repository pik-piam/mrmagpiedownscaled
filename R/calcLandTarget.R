#' calcLandTarget
#'
#' Prepare the high resolution target land use dataset for
#' harmonization and downscaling, checking data for consistency before returning.
#'
#' @param target name of the target dataset, options are: luh2, luh2mod
#' luh2mod will split secdf into forestry and secdf
#' @return land target data
#' @author Pascal Sauer
calcLandTarget <- function(target = "luh2mod") {
  if (target %in% c("luh2", "luh2mod")) {
    cropTypes <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per")

    states <- readSource("LUH2v2h", subtype = "states")
    nonCropStates <- states[[grep(paste(cropTypes, collapse = "|"), names(states),
                                  value = TRUE, invert = TRUE)]]
    states <- toolSpatRasterToDataset(states)
    man <- readSource("LUH2v2h", subtype = "management", convert = FALSE)
    man <- toolSpatRasterToDataset(man)
    man <- man[c(paste0("crpbf_", cropTypes), paste0("irrig_", cropTypes))]
    stopifnot(all.equal(terra::time(states[1]), terra::time(man[1])))

    out <- list(nonCropStates)
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

      irrigatedBiofuel2ndGen <- NULL
      rainfedBiofuel2ndGen <- NULL
      if (grepl("per", cropType)) {
        # 2nd gen biofuel is not part of LUH2v2h, but we need it for the harmonization, so fill with zeros
        irrigatedBiofuel2ndGen <- 0 * states[cropType]
        rainfedBiofuel2ndGen <- 0 * states[cropType]
        names(irrigatedBiofuel2ndGen) <- paste0(names(irrigatedBiofuel2ndGen), "_irrigated_biofuel_2nd_gen")
        names(rainfedBiofuel2ndGen) <- paste0(names(rainfedBiofuel2ndGen), "_rainfed_biofuel_2nd_gen")
        nonBiofuel <- nonBiofuel - irrigatedBiofuel2ndGen - rainfedBiofuel2ndGen
      }

      irrigatedNonBiofuel <- irrig * nonBiofuel
      rainfedNonBiofuel <- nonBiofuel - irrigatedNonBiofuel
      names(irrigatedNonBiofuel) <- sub("\\.\\..+$", paste0("..", cropType, "_irrigated"), names(irrigatedNonBiofuel))
      names(rainfedNonBiofuel) <- sub("\\.\\..+$", paste0("..", cropType, "_rainfed"), names(rainfedNonBiofuel))

      out <- c(out, irrigatedNonBiofuel, rainfedNonBiofuel, irrigatedBiofuel1stGen,
               rainfedBiofuel1stGen, irrigatedBiofuel2ndGen, rainfedBiofuel2ndGen)
    }
    out <- do.call(c, out)
    terra::time(out, tstep = "years") <- as.integer(substr(names(out), 2, 5))
    # need to write raster to disk to avoid memory issues
    # cannot use withr::local_tempfile because the SpatRaster is invalid as soon as the underlying file is deleted
    out <- terra::writeRaster(out, file = tempfile(fileext = ".tif"))

    if (target == "luh2mod") {
      # split secdf into forestry and secdf
      forestryShare <- read.magpie(system.file("extdata/forestryShare.mz", package = "mrdownscale"))
      forestryShare <- as.SpatRaster(forestryShare)
      forestryShare <- terra::disagg(forestryShare, 2) # convert from 0.5x0.5 to 0.25x0.25 degree cells
      forestryShare <- terra::extend(forestryShare, out)
      forestry <- out["secdf"] * forestryShare
      names(forestry) <- sub("secdf", "forestry", names(forestry))
      secdf <- out["secdf"] - forestry
      out <- c(out[[grep("secdf", names(out), invert = TRUE)]], forestry, secdf)

      # cannot cache SpatRaster with both in-memory and on-disk/file sources,
      # so write `out` to a tif file to get SpatRaster with a single source (the tif file)
      out <- terra::writeRaster(out, file = tempfile(fileext = ".tif"))
    }
  } else {
    stop("Unsupported output type \"", target, "\"")
  }

  # checks
  mstools::toolExpectTrue(terra::crs(out) != "", "Data contains CRS information")
  map <- toolLandCategoriesMapping(input = "magpie", target = target)
  mstools::toolExpectTrue(setequal(sub("y[0-9]+\\.\\.", "", names(out)), map$dataOutput),
                          "Land target categories match the corresponding mapping")
  mstools::toolExpectTrue(min(terra::values(min(out)), na.rm = TRUE) >= 0, "All values are >= 0")
  totalAreas <- vapply(unique(terra::time(out)), function(year) {
    sum(terra::values(out[[terra::time(out) == year]]), na.rm = TRUE)
  }, double(1))
  mstools::toolExpectLessDiff(max(totalAreas), min(totalAreas), 10^-4,
                              "Total area is constant over time")

  return(list(x = out,
              class = "SpatRaster",
              unit = "Mha",
              description = "Land target data for data harmonization"))
}
