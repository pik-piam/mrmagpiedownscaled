#' toolExtrapolate
#'
#' Extrapolate a dataset into the future. A linear model is fitted for
#' each combination of spatial entity and category, and then used to predict
#' future values. If the linear model is not significant (p > 0.05) or
#' linearModel is FALSE, the mean value is used for all years instead
#' (constant extrapolation).
#'
#' @param x A magpie object with "year" as the temporal dimension and
#' without any NAs
#' @param years A vector of years to extrapolate to
#' @param linearModel If TRUE, a linear model is fitted for each
#' combination of spatial entity and category.
#' @param fallback What to constant value to use for all years if the linear
#' model is disabled or not significant. Either "mean" or "last". If "mean",
#' the mean value of x is used. If "last", use the value of the last year in x.
#' @return A magpie object like x but with the extrapolated years only
#' @author Pascal Sauer
toolExtrapolate <- function(x, years, linearModel = TRUE, fallback = "mean") {
  stopifnot(names(dimnames(x))[2] == "year",
            !is.na(x),
            !years %in% getYears(x, as.integer = TRUE))

  spatial <- strsplit(names(dimnames(x))[1], "\\.")[[1]]

  return(do.call(mbind, lapply(getItems(x, 1), function(location) {
    return(do.call(mbind, lapply(getItems(x, 3), function(category) {
      d <- as.data.frame(x[location, , category], rev = 3)
      prediction <- d[rep(1, length(years)), ]
      prediction$year <- years
      if (fallback == "mean") {
        prediction$.value <- mean(d$.value)
      } else if (fallback == "last") {
        prediction$.value <- d[d$year == max(d$year), ".value"]
      } else {
        stop("Invalid fallback method")
      }

      if (!all(d$.value == d$.value[1]) && linearModel) {
        model <- stats::lm(.value ~ year, data = d)

        # use linear trend only if signicant
        if (summary(model)$coefficients["year", "Pr(>|t|)"] <= 0.05) {
          prediction$.value <- stats::predict(model, newdata = prediction[, "year", drop = FALSE])
        }
      }

      return(as.magpie(prediction, spatial = spatial, temporal = "year"))
    })))
  })))
}
