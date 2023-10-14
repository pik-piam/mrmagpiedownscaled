extractTransitions <- function(file = "transitions.nc", years=1163:1165) {
  # extract years from transitions.nc and return them in magpie format
  a <- rast("../../mrdownscale_data/LUH2v2/transitions.nc")
  b <- subset(a, grep(paste0("_to_.*_(",paste(years, collapse="|"), ")$"), names(a)))
  names(b) <- sub("^(.*)_to_(.*)_(.*)$", "\\3..\\1.\\2",names(b))
  m <- as.magpie(b)
  getSets(m, fulldim=FALSE)[3] <- "from.to"
  return(m)
}
