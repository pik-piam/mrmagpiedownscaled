#' toolExpectLessDiff
#'
#' tool function for status reporting. It performs a difference check between
#' two objects and returns either a message via \code{toolStatusMessage}, that
#' the test was successful or that it failed.
#'
#' @param x object 1
#' @param y object 2 which has the same format as object 1
#' @param maxdiff allowed maximum difference per element between x and y.
#' @param description a description of the check
#' @param level as the test result will be linked to a function call, the function
#' needs to know to which call it should be linked. by default (\code{level = 0}) the parent
#' function call is being used. Increasing the number by one will let the function go
#' up by one in the call stack, \code{level = -1} will use \code{toolExpectTrue} itself as
#' function call.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getMadratMessage}}, \code{\link{toolExpectTrue}}, \code{\link{toolStatusMessage}}
#' @examples
#' mrdownscale:::toolExpectLessDiff(1:3, 2:4, 10, "data is sufficiently close", level = -1)
#' getMadratMessage("status")

toolExpectLessDiff <- function(x, y, maxdiff, description, level = 0) {
  value <- max(abs(x - y))
  description <- paste0(description, " (maxdiff = ", format(value, digits = 2),
                        ", threshold = ", format(maxdiff, digits = 2), ")")
  toolExpectTrue(value <= maxdiff, description, level = level + 1)
}
