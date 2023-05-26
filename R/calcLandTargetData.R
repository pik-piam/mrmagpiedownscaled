
calcLandTargetData <- function(target = "luh2") {
  # get target data
  if (target == "luh2") {
    out <- madrat::readSource("LUH2v2h")
  } else {
    stop("Unsupported output type \"", target, "\"")
  }
  return(list(x = out,
              weight = NULL,
              class = "SpatRaster",
              unit = "Mha",
              cache = FALSE,
              description = "Land target data for data harmonization"))
}
