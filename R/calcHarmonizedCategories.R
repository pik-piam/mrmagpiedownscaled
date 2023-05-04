calcHarmonizedCategories <- function(input = "magpie", output = "luh2") {
  if (input == "magpie") {
    x <- madrat::readSource("Magpie")
    input2fao <- toolGetMapping("magpie2ref.csv", where = "mrdownscale")
  } else {
    stop("Unsupported input type \"", input, "\"")
  }

  if (output == "luh2") {
    output2fao <- toolGetMapping("luh2ref.csv", where = "mrdownscale")
  } else {
    stop("Unsupported output type \"", output, "\"")
  }

  x <- toolRemapCategories(x, input2fao, output2fao)

  return(list(x = x,
              class = "SpatVector",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Input data with land categories remapped to categories of output target data set"))
}
