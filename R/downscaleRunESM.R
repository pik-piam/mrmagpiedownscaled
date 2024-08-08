#' downscaleRunESM
#'
#' Downscale MAgPIE results from the given folder to 0.25 degree resolution in
#' LUH2 format for ESMs. The resulting tgz file will be generated in the given folder.
#'
#' @param outputdir path to a folder containing fulldata.gdx and clustermap_*.rds,
#' resulting tgz will be written here
#' @param revision passed on to retrieveData, default: current date
#' @param scenario passed on to retrieveData, default: slightly modified folder name
#' @param ... additional arguments passed on to retrieveData
#' @return Invisibly, the path to the newly created tgz archive.
#'
#' @author Pascal Sauer
#' @export
downscaleRunESM <- function(outputdir, revision = NULL, scenario = NULL, ...) {
  outputdir <- normalizePath(outputdir)

  clustermap <- Sys.glob(file.path(outputdir, "clustermap_*.rds"))
  gdx <- file.path(outputdir, "fulldata.gdx")
  stopifnot(file.exists(gdx), length(clustermap) == 1)

  if (is.null(revision)) {
    revision <- format(Sys.time(), "%Y-%m-%d")
  }

  if (is.null(scenario)) {
    scenario <- gsub("_", "-", sub("-mag-[0-9]+$", "", basename(outputdir)))
  }

  redirectSource("MagpieFulldataGdx", c(clustermap, gdx), linkOthers = FALSE)
  stopifnot(length(getConfig("redirections")) >= 1)
  retrieveData("ESM", rev = revision, scenario = scenario, progress = FALSE,
               outputfolder = outputdir, ...)
}
