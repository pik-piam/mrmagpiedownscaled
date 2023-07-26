# Pascal

toolSpatRasterToDataset <- function(x) {
  stopifnot(grepl("^y[0-9]+\\.\\.", names(x)))
  varnames <- unique(sub("^y[0-9]+\\.\\.", "", names(x)))
  datasets <- lapply(varnames, function(varname) x[paste0("\\.\\.", varname, "$")])
  x <- terra::sds(datasets)
  names(x) <- varnames
  return(x)
}
