calcLandTargetData <- function(target = "luh2") {
  if (target == "luh2") {
    states <- readSource("LUH2v2h", subtype = "states")
    states <- toolSpatRasterToDataset(states)
    man <- readSource("LUH2v2h", subtype = "management", convert = FALSE)
    man <- toolSpatRasterToDataset(man)
    # remove rndwd & fulwd because they cannot be converted to Mha
    man <- man[setdiff(names(man), c("rndwd", "fulwd"))]
    stopifnot(all.equal(terra::time(states[1]), terra::time(man[1])))

    cropTypes <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per")

    # crpbf_<cropType> = 1st gen biofuel crop share of <cropType>
    # -> get <cropType>_biofuel_1st_gen area in Mha by multiplying with <cropType>
    biofuel1stGen <- terra::sds(lapply(cropTypes, function(cropType) {
      crpbf <- man[paste0("crpbf_", cropType)]
      stopifnot(all.equal(terra::time(crpbf), terra::time(states[cropType])))
      out <- crpbf * states[cropType]
      names(out) <- sub("\\.\\.crpbf_(.+)$", "..\\1_biofuel_1st_gen", names(out))
      return(out)
    }))
    names(biofuel1stGen) <- paste0(cropTypes, "_biofuel_1st_gen")

    # 2nd gen biofuel is not part of LUH2v2h, but we need it for the harmonization, so fill with zeros
    perennials <- c("c3per", "c4per")
    biofuel2ndGen <- terra::sds(lapply(perennials, function(cropType) {
      out <- states[cropType] * 0
      names(out) <- paste0(names(out), "_biofuel_2nd_gen")
      return(out)
    }))
    names(biofuel2ndGen) <- paste0(perennials, "_biofuel_2nd_gen")

    crops <- terra::sds(lapply(cropTypes, function(cropType) {
      biofuel1 <- biofuel1stGen[paste0(cropType, "_biofuel_1st_gen")]
      biofuel2 <- if (cropType %in% perennials) biofuel2ndGen[paste0(cropType, "_biofuel_2nd_gen")] else 0
      return(states[cropType] - biofuel1 - biofuel2)
    }))
    names(crops) <- cropTypes

    states <- c(states[setdiff(names(states), cropTypes)], crops)
    man <- c(man[setdiff(names(man), paste0("crpbf_", cropTypes))],
             biofuel1stGen,
             biofuel2ndGen)

    out <- do.call(c, as.list(c(states, man)))
  } else {
    stop("Unsupported output type \"", target, "\"")
  }
  attr(out, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = out,
              class = "SpatRaster",
              unit = "Mha",
              description = "Land target data for data harmonization"))
}
