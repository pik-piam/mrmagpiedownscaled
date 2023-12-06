library(mrdownscale) # nolint
setConfig(mainfolder = "/p/projects/rd3mod/tmp/mrdownscale") # nolint
retrieveData("RESCUE", rev = format(Sys.time(), "%Y-%m-%d"))
