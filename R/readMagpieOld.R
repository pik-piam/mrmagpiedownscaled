readMagpieOld <- function() {
  # TODO use downscaling reference dataset (LUH2) instead of avl_land_full_t_c200.mz
  stopifnot(file.exists("fulldata.gdx"),
            file.exists("avl_land_full_t_c200.mz"), # from magpiemodel/magpie/input/avl_land_full_t_c200.mz
            length(Sys.glob("clustermap_*.rds")) == 1)

  landUse <- magpie4::land("fulldata.gdx", level = "cell")

  # TODO move to calc
  # disaggregate other to primother and secdother
  landUse <- magpie4::PrimSecdOtherLand(x = landUse,
                                        ini_file = "avl_land_full_t_c200.mz",
                                        level = "grid") # grid = no aggregation, this is actually cluster level
  cropArea <- magpie4::croparea("fulldata.gdx", level = "cell", product_aggr = FALSE)

  x <- magclass::mbind(landUse, cropArea)

  clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
  returnSpatVector <- FALSE
  if (returnSpatVector) {
    x <- magpie4::clusterOutputToTerraVector(x, clustermap)
  }
  attr(x, "clustermap") <- clustermap
  attr(x, "historic") <- magclass::read.magpie("avl_land_full_t_c200.mz")
  if (returnSpatVector) {
    return(list(x = x, class = "SpatVector"))
  } else {
    return(x)
  }
}
