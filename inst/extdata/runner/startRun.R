library(mrdownscale) # nolint

basePath <- "/p/projects/rescue/tier1_scenarios/v4p0/cpl/magpie/output/" # nolint
scenarios <- c("C_SSP2EU-Base",
               "C_SSP2EU-NPi",
               "C_SSP2EU-NDC",

               #  Scenarios to be used as training data for the emulator
               # "C_RESCUE-ext-v4p0-EocBudg_cp0400-OAE_off",
               # "C_RESCUE-ext-v4p0-EocBudg_cp0450-OAE_off",
               # "C_RESCUE-ext-v4p0-EocBudg_cp0500-OAE_off",
               # "C_RESCUE-ext-v4p0-EocBudg_cp0600-OAE_off",
               # "C_RESCUE-ext-v4p0-EocBudg_cp0750-OAE_off",
               # "C_RESCUE-ext-v4p0-EocBudg_cp1000-OAE_off",
               # "C_RESCUE-ext-v4p0-EocBudg_cp1300-OAE_off",
               # "C_RESCUE-ext-v4p0-EocBudg_cp1700-OAE_off",
               # "C_RESCUE-ext-v4p0-EocBudg_cp2300-OAE_off",
               "C_RESCUE-ext-v4p0-EocBudg_cp3000-OAE_off",
               # "C_RESCUE-ext-v4p0-EocBudg_cp0400-OAE_on",
               # "C_RESCUE-ext-v4p0-EocBudg_cp0450-OAE_on",
               # "C_RESCUE-ext-v4p0-EocBudg_cp0500-OAE_on",
               # "C_RESCUE-ext-v4p0-EocBudg_cp0600-OAE_on",
               # "C_RESCUE-ext-v4p0-EocBudg_cp0750-OAE_on",
               # "C_RESCUE-ext-v4p0-EocBudg_cp1000-OAE_on",
               # "C_RESCUE-ext-v4p0-EocBudg_cp1300-OAE_on",
               # "C_RESCUE-ext-v4p0-EocBudg_cp1700-OAE_on",
               # "C_RESCUE-ext-v4p0-EocBudg_cp2300-OAE_on",
               "C_RESCUE-ext-v4p0-EocBudg_cp3000-OAE_on",

               #  Scenarios for direct use in ESMs
               "C_RESCUE-dir-v4p0-PkBudg500-OAE_off",
               "C_RESCUE-dir-v4p0-PkBudg1150-OAE_off",
               "C_RESCUE-dir-v4p0-PkBudg500-OAE_on",
               "C_RESCUE-dir-v4p0-PkBudg1150-OAE_on",
               "C_RESCUE-dir-v4p0-EocBudg500-OAE_off",
               "C_RESCUE-dir-v4p0-EocBudg1150-OAE_off",
               "C_RESCUE-dir-v4p0-EocBudg500-OAE_on",
               "C_RESCUE-dir-v4p0-EocBudg1150-OAE_on")

for (scenario in scenarios) {
  stopifnot(file.exists(paste0(basePath, scenario, "-mag-6/",
                               c("fulldata.gdx", "clustermap_rev4.99_c200_67420_h12.rds"))))
}

now <- format(Sys.time(), "%Y-%m-%d")

for (scenario in scenarios) {
  message("Copying fulldata.gdx and clustermap from ", basePath, scenario, "-mag-6/")
  file.copy(paste0(basePath, scenario, "-mag-6/",
                   c("fulldata.gdx", "clustermap_rev4.99_c200_67420_h12.rds")),
            file.path(getConfig("sourcefolder"), "MagpieFulldataGdx"),
            overwrite = TRUE)
  message("md5 of copied fulldata.gdx: ",
          tools::md5sum(file.path(getConfig("sourcefolder"), "MagpieFulldataGdx", "fulldata.gdx")))
  try(retrieveData("RESCUE", rev = now, scenario = gsub("_", "-", scenario)))
}
