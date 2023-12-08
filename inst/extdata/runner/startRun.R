library(mrdownscale) # nolint
setConfig(mainfolder = "/p/projects/rd3mod/tmp/mrdownscale") # nolint

basePath <- "/p/projects/rescue/tier1_scenarios/v3/cpl/magpie/output/C_" # nolint
scenarios <- c("RESCUE-dir-v2p0-EocBudg1150-OAE_off",
               "RESCUE-dir-v2p0-EocBudg500-OAE_off",
               "RESCUE-dir-v2p0-PkBudg1150-OAE_off",
               "RESCUE-dir-v2p0-PkBudg1150-OAE_on",
               "RESCUE-dir-v2p0-PkBudg500-OAE_off",
               "RESCUE-dir-v2p0-PkBudg500-OAE_on",
               "SSP2EU-Base",
               "SSP2EU-NDC",
               "SSP2EU-NPi")

for (scenario in scenarios) {
  stopifnot(file.exists(paste0(basePath, scenario, "-mag-7/",
                               c("fulldata.gdx", "clustermap_rev4.94_c200_67420_h12.rds"))))
}

now <- format(Sys.time(), "%Y-%m-%d")

for (scenario in scenarios) {
  file.copy(paste0(basePath, scenario, "-mag-7/",
                   c("fulldata.gdx", "clustermap_rev4.94_c200_67420_h12.rds")),
            file.path(getConfig("sourcefolder"), "Magpie"),
            overwrite = TRUE)
  try(retrieveData("RESCUE", rev = now, scenario = gsub("_", "-", scenario)))
}
