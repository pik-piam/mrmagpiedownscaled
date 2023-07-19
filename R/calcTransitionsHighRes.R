calcTransitionsHighRes <- function(input = "magpie", target = "luh2", years = c(1995, 2000, 2005, 2010)) {

  if (length(years) < 2) stop("At least two time steps required to be able to compute transistions")
  if (length(years) > 5) warning("Selecting more than 5 time steps at once will most like cause memory problems.")

  x <- toolAddCheckReport(calcOutput("LandHighRes", input = input, target = target, aggregate = FALSE))[, years, ]
  x <- toolTransitionsBasic(x)
  attr(x, "toolCheck") <- toolCheckReport(filter = TRUE)

  return(list(x = x,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Downscaled land use transitions"))
}
