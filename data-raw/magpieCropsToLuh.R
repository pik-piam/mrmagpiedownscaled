# following the workflow proposed by https://r-pkgs.org/data.html#sec-data-sysdata
# code to prepare the internal dataset `magpie2luh` which maps magpie crops to LUH crop categories
withr::with_package("mrcommons", {
  # TODO this is just a rough approximation, also using shares from 2010 is arbitrary
  magpie2luh <- madrat::calcOutput(type = "LUH2MAgPIE", aggregate = FALSE, share = "LUHofMAG",
                                   bioenergy = "fix", missing = "fill", rice = "total")
})
magpie2luh <- magpie2luh[, 2010, ]
magclass::getYears(magpie2luh) <- NULL

usethis::use_data(magpie2luh, internal = TRUE, overwrite = TRUE)
