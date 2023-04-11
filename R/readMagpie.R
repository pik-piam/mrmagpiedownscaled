readMagpie <- function() {
  stopifnot(file.exists("fulldata.gdx"),
            file.exists("avl_land_full_t_c200.mz"),
            length(Sys.glob("clustermap_*.rds")) == 1)

  landUse <- magpie4::land("fulldata.gdx", level = "cell")

  # disaggregate other to primother and secdother
  landUse <- magpie4::PrimSecdOtherLand(x = landUse,
                                        ini_file = "avl_land_full_t_c200.mz",
                                        level = "grid") # grid = no aggregation, this is actually cluster level
  cropArea <- magpie4::croparea("fulldata.gdx", level = "cell", product_aggr = FALSE)

  x <- magclass::mbind(landUse, cropArea)
  attr(x, "clustermap") <- readRDS(Sys.glob("clustermap_*.rds"))
  return(x)
}
