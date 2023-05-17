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

  x <- toolRemapCategories(x, input2ref, output2ref)

  return(list(x = x,
              isocountries = FALSE,
              unit = "Mha",
              description = "Input data with land categories remapped to categories of output target data set"))
}
