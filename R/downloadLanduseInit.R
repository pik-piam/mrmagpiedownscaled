downloadLanduseInit <- function() {
  url <- paste0("https://rse.pik-potsdam.de/data/magpie/public/",
                "rev4.111_h12_fd712c0b_cellularmagpie_c200_MRI-ESM2-0-ssp370_lpjml-8e6c5eb1.tgz")
  utils::download.file(url, "cellularmagpie.tgz", mode = "wb")
  utils::untar("cellularmagpie.tgz", "./avl_land_t_0.5.mz")
  unlink("cellularmagpie.tgz")
  return(list(url          = url,
              doi          = NA,
              title        = "LanduseInitialization",
              description  = "Land use initialization data mostly based on LUH2v2h for use in MAgPIE.",
              author       = "MAgPIE team",
              unit         = "Mha",
              version      = "rev4.111",
              release_date = "2024-09-24",
              license      = NA,
              reference    = NA))
}
