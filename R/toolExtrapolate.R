#' toolExtrapolate
#'
#' Extrapolate a dataset into the future. A linear model is fitted for
#' each combination of spatial entity and category, and then used to predict
#' future values. If the linear model is not significant (p > 0.05), the mean
#' value is used for all years instead (constant extrapolation).
#'
#' @param x A magpie object with "year" as the temporal dimension and
#' without any NAs
#' @param years A vector of years to extrapolate to
#' @return A magpie object like x but with the extrapolated years only
#' @author Pascal Sauer
toolExtrapolate <- function(x, years) {
  stopifnot(names(dimnames(x))[2] == "year",
            !is.na(x),
            !years %in% getYears(x, as.integer = TRUE))

  spatial <- strsplit(names(dimnames(x))[1], "\\.")[[1]]

  return(do.call(mbind, lapply(getItems(x, 1), function(location) {
    return(do.call(mbind, lapply(getItems(x, 3), function(category) {
      d <- as.data.frame(x[location, , category], rev = 3)

      model <- stats::lm(.value ~ year, data = d)

      prediction <- d[rep(1, length(years)), ]
      prediction$year <- years

      # use linear trend only if signicant
      if (all(d$.value == d$.value[1]) || summary(model)$coefficients["year", "Pr(>|t|)"] > 0.05) {
        prediction$.value <- mean(d$.value)
      } else {
        prediction$.value <- stats::predict(model, newdata = prediction)
      }

      return(as.magpie(prediction, spatial = spatial, temporal = "year"))
    })))
  })))
}
