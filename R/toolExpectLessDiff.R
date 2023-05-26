toolExpectLessDiff <- function(x, y, maxdiff, description) {
  value <- max(abs(x - y))
  description <- paste0(description, " (maxdiff = ", format(value, digits = 2),
                        ", threshold = ", format(maxdiff, digits = 2), ")")
  toolExpectTrue(value <= maxdiff, description)
}
