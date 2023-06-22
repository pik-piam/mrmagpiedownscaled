calcLandTargetData <- function(target = "luh2") {
  # get target data
  if (target == "luh2") {
    states <- readSource("LUH2v2h", subtype = "states")
    man <- readSource("LUH2v2h", subtype = "management", convert = FALSE)

    # crpbf_c3per = biofuel crop share of c3per -> get c3per_biofuel area in Mha by multiplying with c3per
    pattern <- "\\.\\.crpbf_c3per$"
    man[[grep(pattern, names(man))]] <- man[pattern] * states["\\.\\.c3per$"]
    pattern <- "\\.\\.crpbf_c4per$"
    man[[grep(pattern, names(man))]] <- man[pattern] * states["\\.\\.c4per$"]
    names(man) <- sub("crpbf_(c[34]per)$", "\\1_biofuel", names(man))

    # subtract c3per_biofuel from c3per to avoid double counting
    pattern <- "\\.\\.c3per$"
    states[[grep(pattern, names(states))]] <- states[pattern] - man["\\.\\.c3per_biofuel$"]
    pattern <- "\\.\\.c4per$"
    states[[grep(pattern, names(states))]] <- states[pattern] - man["\\.\\.c4per_biofuel$"]

    out <- c(states, man)
  } else {
    stop("Unsupported output type \"", target, "\"")
  }
  attr(out, "toolCheck") <- toolCheckReport(filter = TRUE)
  return(list(x = out,
              weight = NULL,
              class = "SpatRaster",
              unit = "Mha",
              cache = FALSE,
              description = "Land target data for data harmonization"))
}
