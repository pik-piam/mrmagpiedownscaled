calcLandHighRes <- function(input = "magpie", target = "luh2") {
  x <- calcOutput("Harmonized", input = input, target = target, aggregate = FALSE)

  # get target data
  if (target == "luh2") {
    xTarget <- madrat::readSource("LUH2v2h")
  } else {
    stop("Unsupported output type \"", target, "\"")
  }

  .getDownscaleMap <- function(x, xTarget) {
    tmp <- as.SpatVector(x[, 1, 1])[, ".id"]
    tmp[[1]]$.id <- as.integer(tmp[[1]]$.id)
    tmp <- terra::rasterize(tmp, xTarget, ".id", touches = TRUE)
    names(tmp) <- "id"
    tmp <- as.magpie(tmp)
    map <- data.frame(cell = getItems(tmp, dim = 1), cluster = as.vector(tmp))

    clusternames <- getItems(x, dim = 1)
    names(clusternames) <- getItems(x, dim = 1.1)
    map$cluster <- clusternames[map$cluster]

    return(map)
  }

  map <- .getDownscaleMap(x, xTarget)

  mTarget <- as.magpie(xTarget)
  intersect <- intersect(getItems(mTarget, dim = 1), map$cell)
  map <- map[map$cell %in% intersect, ]
  mTarget <- mTarget[intersect, , ]

  "!# @monitor luscale::interpolate2"

  out <- luscale::interpolate2(x[, -1, ], mTarget[, 1, ], map)
  getSets(out)[1:2] <- c("x", "y")

  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Downscaled and harmonized land use data"))
}
