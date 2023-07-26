#' toolSpatRasterToDataset
#'
#' Convert a SpatRaster to a SpatRasterDataset.
#'
#' @param x SpatRaster with names of the form "y[0-9]+..[varname]"
#' @return SpatRasterDataset
#' @author Pascal Führlich
toolSpatRasterToDataset <- function(x) {
  stopifnot(grepl("^y[0-9]+\\.\\.", names(x)))
  varnames <- unique(sub("^y[0-9]+\\.\\.", "", names(x)))
  datasets <- lapply(varnames, function(varname) x[paste0("\\.\\.", varname, "$")])
  x <- terra::sds(datasets)
  names(x) <- varnames
  return(x)
}
