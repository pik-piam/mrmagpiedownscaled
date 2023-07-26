#' calcTransitionHighRes
#'
#' Computes a land use transition data set for a given input as as well as
#' target data source.
#'
#' @param input name of the land input source to be used (default "magpie")
#' @param target name of the land target source to be used (default "luh2")
#' @param diffYears year between the transitions should be calculated. As these
#' computations can be quite memory consumed it is advised not to select too many
#' years at once
#' @author Jan Philipp Dietrich

calcTransitionsHighRes <- function(input = "magpie", target = "luh2", diffYears = c(1995, 2000, 2005, 2010)) {

  if (length(diffYears) < 2) stop("At least two time steps required to be able to compute transistions")
  if (length(diffYears) > 5) warning("Selecting more than 5 time steps at once will most like cause memory problems.")

  x <- calcOutput("LandHighRes", input = input, target = target, aggregate = FALSE)[, diffYears, ]
  x <- toolTransitionsBasic(x)

  return(list(x = x,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Downscaled land use transitions"))
}
