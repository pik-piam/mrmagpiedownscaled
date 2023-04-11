downloadMagpie <- function() {
  sourceFolder <- sub("-downloadInProgress", "", normalizePath("."))
  dir.create(sourceFolder)
  stop("Please put the following output files of a magpie run into '", sourceFolder, "':\n",
       "fulldata.gdx\n",
       "cell.land_0.5.mz\n",
       "avl_land_full_t_0.5.mz\n",
       "clustermap.rds (needs to be renamed from something like clustermap_rev4.81_c200_h12.rds)")
}
