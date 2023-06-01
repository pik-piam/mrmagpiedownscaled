
toolCheck <- function(title, code) {
  n <- 100
  o <- c(paste(paste(rep("-", 3), collapse = ""), toupper(title),
                paste(rep("-", n - nchar(title) - 5), collapse = "")),
         sub("^~* ","", capture.output(out <- eval(code, envir = parent.frame()), type = "message")),
         paste(rep("-", n), collapse = ""))
  for(i in seq_along(o)) vcat(1, o[i], show_prefix = FALSE)
  options(toolCheck = c(getOption("toolCheck"), o))
}
