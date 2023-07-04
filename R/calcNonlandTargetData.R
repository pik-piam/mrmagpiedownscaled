calcNonlandTargetData <- function(target = "luh2") {
  if (target == "luh2") {
    out <- readSource("LUH2v2h", subtype = "management", convert = FALSE)
    out <- out["rndwd|fulwd|fertl"]
  } else {
    stop("Unsupported output type \"", target, "\"")
  }
  attr(out, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = out,
              class = "SpatRaster",
              cache = FALSE,
              unit = "1",
              description = "Nonland target data for data harmonization"))
}
