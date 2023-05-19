calcHarmonizedCategories <- function(input = "magpie", output = "luh2") {
  if (input == "magpie") {
    x <- madrat::readSource("Magpie")
    input2ref <- toolGetMapping("magpie2ref.csv", where = "mrdownscale")
  } else {
    stop("Unsupported input type \"", input, "\"")
  }
  if (output == "luh2") {
    output2ref <- toolGetMapping("luh2ref.csv", where = "mrdownscale")
  } else {
    stop("Unsupported output type \"", output, "\"")
  }

  # merge input and output map
  .getMap <- function(input2ref, output2ref) {
    map <- merge(input2ref, output2ref, by = "reference", suffixes = c("Input", "Output"))
    map$merge <- paste(map$dataInput, map$dataOutput, sep = "_")
    return(map)
  }
  map <- .getMap(input2ref, output2ref)

  # get weights for disaggregation to reference categories
  ref <- calcOutput("CategorizationWeight", map = map, geometry = attr(x, "geometry"),
                    crs = attr(x, "crs"), aggregate = FALSE)

  y   <- toolAggregate(x, map, dim = 3, from = "dataInput", to = "merge", weight = ref)
  out <- toolAggregate(y, map, dim = 3, from = "merge",     to = "dataOutput")

  attr(out, "crs") <- attr(x, "crs")
  attr(out, "geometry") <- attr(x, "geometry")

  return(list(x = out,
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Input data with land categories remapped to categories of output target data set"))
}
