downloadMagpie <- function() {
  madrat::toolManualDownload("Run magpie to get a fulldata.gdx.",
                             "Currently magpie data has to be created manually",
                             "Enter full path to the created fulldata.gdx:")
  stopifnot(file.exists("fulldata.gdx"))
  madrat::toolManualDownload(NULL, NULL, "Enter full path to the created clustermap_*.rds:")
  file.rename(Sys.glob("clustermap_*.rds"), "clustermap.rds")
  stopifnot(all(file.exists(c("clustermap.rds"))))
}
