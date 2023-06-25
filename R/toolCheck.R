
toolCheck <- function(title, code) {
  fname <- as.character(sys.call(-1))[1]
  if (is.na(fname)) fname <- "undefined"

  withr::with_envvar(new = c(toolStatusMessages = NULL),                    {
                      code
                      toolStatusMessages <- getOption("toolStatusMessages")
                    })

  title <- paste0(fname, " | ", toupper(title))
  n <- 100
  o <- c(paste(paste(rep("-", 3), collapse = ""), title,
                paste(rep("-", n - nchar(title) - 5), collapse = "")),
         toolStatusMessages,
         paste(rep("-", n), collapse = ""))
  for (i in seq_along(o)) vcat(1, o[i], show_prefix = FALSE)
  envar <- getOption("toolCheck")
  envar[[fname]] <- c(envar[[fname]], o[-length(o)])
  options(toolCheck = envar) # nolint: undesirable_function_linter
}
