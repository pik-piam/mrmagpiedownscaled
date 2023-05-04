calcHarmonizedCategories <- function(input = "magpie", output = "luh2") {
  if (input == "magpie") {
    x <- madrat::readSource("Magpie")
    input2fao <- read.csv(system.file("extdata/magpie2ref.csv", package = "mrdownscale"))
  } else {
    stop("Unsupported input type \"", input, "\"")
  }

  if (output == "luh2") {
    output2fao <- read.csv(system.file("extdata/luh2ref.csv", package = "mrdownscale"))
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
