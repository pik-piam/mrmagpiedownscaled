toolExpectLessDiff <- function(x, y, maxdiff, description, level = 0) {
  value <- max(abs(x - y))
  description <- paste0(description, " (maxdiff = ", format(value, digits = 2),
                        ", threshold = ", format(maxdiff, digits = 2), ")")
  toolExpectTrue(value <= maxdiff, description, level = level + 1)
}
