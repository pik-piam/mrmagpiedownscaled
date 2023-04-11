readMagpie <- function() {
  stopifnot(file.exists("fulldata.gdx"),
            file.exists("cell.land_0.5.mz"),
            file.exists("avl_land_full_t_0.5.mz"),
            length(Sys.glob("clustermap_*.rds")) == 1)

  landUse <- magpie4::land("fulldata.gdx", level = "cell")

  primSecdOther <- magpie4::PrimSecdOtherLand(x = "cell.land_0.5.mz",
                                              ini_file = "avl_land_full_t_0.5.mz",
                                              level = "grid") # TODO grid level + aggregate to cluster is not ideal
  primSecdOther <- primSecdOther[, , c("primother", "secdother")]
  clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
  primSecdOther <- madrat::toolAggregate(primSecdOther, clustermap, to = "cluster")

  cropArea <- magpie4::croparea("fulldata.gdx", level = "cell", product_aggr = FALSE)

  x <- magclass::mbind(landUse, primSecdOther, cropArea)
  attr(x, "clustermap") <- clustermap
  return(x)
}
