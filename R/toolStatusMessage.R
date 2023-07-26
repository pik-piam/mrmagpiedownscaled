#' toolStatusMessage
#'
#' tool to trigger status messages describing the data quality at different stages
#' of processing. Messages are directly written to the log at execution but also
#' collected to be finally returned as data report.
#'
#' @param status status indicator of the messages. Currently either "ok" (check
#' succesful / quality ok), or "warn" (check unsuccessful / undesired result).
#' @param message message to be triggered.
#' @param level as the test result will be linked to a function call, the function
#' needs to know to which call it should be linked. by default (\code{level = 0}) the parent
#' function call is being used. Increasing the number by one will let the function go
#' up by one in the call stack, \code{level = -1} will use \code{toolExpectTrue} itself as
#' function call.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{getMadratMessage}}, \code{\link{toolExpectLessDiff}}, \code{\link{toolStatusMessage}}
#' @examples
#' mrdownscale:::toolStatusMessage("ok", "everything is ok", level = -1)
#' mrdownscale:::toolStatusMessage("warn", "this is not ok", level = -1)
#' getMadratMessage("status")

toolStatusMessage <- function(status, message, level = 0) {
  symbol <- toolSubtypeSelect(status, c(ok = "\u2713", warn = "!"))
  message <- paste0("[", symbol, "] ", message)
  vcat(1, message, show_prefix = FALSE)
  putMadratMessage("status", message, fname = -2 - level, add = TRUE)
}
