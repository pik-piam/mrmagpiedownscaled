#' toolMaxExpansion
#'
#' Return the maximum expansion of a variable from one timestep to the next.
#'
#' @param x A magclass object
#' @param variable A character string indicating the variable to be used
#' @param removeNA A logical indicating whether NA values should be ignored
#' @return A numeric value indicating the maximum expansion of the variable
#'
#' @author Pascal Sauer
#' @export
toolMaxExpansion <- function(x, variable, removeNA = FALSE) {
  stopifnot(nyears(x) >= 2,
            variable %in% getItems(x, 3))
  return(max(x[, -1, variable] - setYears(x[, -nyears(x), variable],
                                          getYears(x[, -1, ])),
             na.rm = removeNA))
}
