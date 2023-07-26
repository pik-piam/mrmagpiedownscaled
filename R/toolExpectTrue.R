#' toolExpectTrue
#'
#' tool function for status reporting. It performs a logical check
#' and returns either a message via \code{toolStatusMessage}, that
#' the test was successful or that it failed.
#'
#' @param check logical check to be run (has to be either TRUE or FALSE)
#' @param description a description of the check
#' @param level as the test result will be linked to a function call, the function
#' needs to know to which call it should be linked. by default (\code{level = 0}) the parent
#' function call is being used. Increasing the number by one will let the function go
#' up by one in the call stack, \code{level = -1} will use \code{toolExpectTrue} itself as
#' function call.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getMadratMessage}}, \code{\link{toolExpectLessDiff}}, \code{\link{toolStatusMessage}}
#' @examples
#' mrdownscale:::toolExpectTrue(is.numeric(1), "data is numeric", level = -1)
#' getMadratMessage("status")

toolExpectTrue <- function(check, description, level = 0) {
  status <- ifelse(isTRUE(check), "ok", "warn")
  toolStatusMessage(status, description, level = 1 + level)
}
