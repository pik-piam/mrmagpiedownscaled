toolLandCategoriesMapping <- function(input, target) {
  .getMap <- function(x) {
    if (x == "magpie") {
      out <- toolGetMapping("magpie2ref.csv", where = "mrdownscale")
    } else if (x == "luh2") {
      out <- toolGetMapping("luh2ref.csv", where = "mrdownscale")
    } else {
      stop("Categories mapping for type \"", x, "\" not available!", call. = FALSE)
    }
    return(out)
  }
  input2ref  <- .getMap(input)
  output2ref <- .getMap(target)

  if (!setequal(input2ref$reference, output2ref$reference)) {
    warning("Input map and output map contain inconsistent reference information")
  }
  map <- merge(input2ref, output2ref, by = "reference", suffixes = c("Input", "Output"))
  map$merge <- paste(map$dataInput, map$dataOutput, sep = "_")
  if (anyDuplicated(map$reference)) {
    warning("Insuficient granularity of reference categories, as a reference category is mapped more than once (\"",
            paste(unique(map$reference[duplicated(map$reference)]), collapse = "\", \""), "\").")
  }
  return(map)
}
