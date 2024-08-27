#' toolWoodHarvestArea
#'
#' Check wood harvest area is not exceeding land area of the corresponding
#' type in the previous timestep divided by timestep length.
#'
#' @param x magpie object with exactly the following categories:
#' paste0(c("primf", "secyf", "secmf", "primn", "secnf"), "_wood_harvest_area")
#' @param land magpie object with at least the following categories:
#' c("primf", "secdf", "primn", "secdn")
#'
#' @author Pascal Sauer
toolWoodHarvestArea <- function(x, land) {
  stopifnot(setequal(getItems(x, 1), getItems(land, 1)))
  woodHarvestArea <- toolAggregateWoodHarvest(x)
  stopifnot(getItems(woodHarvestArea, 3) %in% getItems(land, 3))

  stopifnot(identical(getYears(woodHarvestArea), getYears(land)),
            getItems(woodHarvestArea, 3) %in% getItems(land, 3))
  years <- getYears(land, as.integer = TRUE)
  timestepLengths <- new.magpie(years = years[-1], fill = diff(years))
  woodland <- setYears(land[, -nyears(land), getItems(woodHarvestArea, 3)], years[-1])

  landOvershoot <- min(woodland - timestepLengths * collapseDim(woodHarvestArea)[, -1, ])
  toolExpectTrue(landOvershoot >= -10^-10, paste0("Wood harvest area is smaller than land ",
                                                  "of the corresponding type ",
                                                  "(max overshoot: ", signif(-landOvershoot, 3), " Mha)"),
                 level = 1)
}

toolAggregateWoodHarvest <- function(woodHarvest) {
  map <- as.data.frame(rbind(c("primf_wood_harvest_area", "primf"),
                             c("secyf_wood_harvest_area", "secdf"),
                             c("secmf_wood_harvest_area", "secdf"),
                             c("primn_wood_harvest_area", "primn"),
                             c("secnf_wood_harvest_area", "secdn")))
  colnames(map) <- c("harvest", "land")

  stopifnot(setequal(getItems(woodHarvest, 3), map$harvest))

  return(toolAggregate(woodHarvest, map, from = "harvest", to = "land", dim = 3))
}

toolDisaggregateWoodHarvest <- function(woodHarvest, weight) {
  map <- as.data.frame(rbind(c("primf_wood_harvest_area", "primf"),
                             c("secyf_wood_harvest_area", "secdf"),
                             c("secmf_wood_harvest_area", "secdf"),
                             c("primn_wood_harvest_area", "primn"),
                             c("secnf_wood_harvest_area", "secdn")))
  colnames(map) <- c("harvest", "land")

  stopifnot(setequal(getItems(woodHarvest, 3), map$land))

  return(toolAggregate(woodHarvest, map, weight = weight, from = "land", to = "harvest", dim = 3))
}
