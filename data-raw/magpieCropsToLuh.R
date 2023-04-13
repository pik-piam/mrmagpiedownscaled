# following the workflow proposed by https://r-pkgs.org/data.html#sec-data-sysdata
# code to prepare the internal dataset `magpieCropsToLuh` which maps magpie crops to LUH crop categories
withr::with_package("mrcommons", {
  # TODO this is just a rough approximation, also using shares from 2010 is arbitrary
  magpieCropsToLuh <- madrat::calcOutput(type = "LUH2MAgPIE", aggregate = FALSE, share = "LUHofMAG",
                                         bioenergy = "fix", missing = "fill", rice = "total")
})
magpieCropsToLuh <- magpieCropsToLuh[, 2010, ]
magclass::getYears(magpieCropsToLuh) <- NULL

usethis::use_data(magpieCropsToLuh, internal = TRUE, overwrite = TRUE)
