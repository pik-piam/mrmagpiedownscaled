extractTransitions <- function(file = "transitions.nc", years=1:3) {
  # extract years from transitions.nc and return them in magpie format
  a <- rast("../../mrdownscale_data/LUH2v2/transitions.nc")
  b <- subset(a, grep(paste0("_(",paste(years, collapse="|"), ")$"), names(a)))
  names(b) <- sub("^(.*)_(.*)$", "\\2..\\1",names(b))
  m <- as.magpie(b)
  return(m)
}
