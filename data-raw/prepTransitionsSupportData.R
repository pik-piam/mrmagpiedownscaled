library(terra)
library(magclass)


extractTransitions <- function(file = "transistions.nc", years=2014:2015, filterPrimary = TRUE) {
  # correct years to "years since 850"
  years <- years - 850
  # extract years from transitions.nc and return them in magpie format
  a <- rast(file)

  # only select transitions for the selected years
  filter <- grep(paste0("_to_.*_(",paste(years, collapse="|"), ")$"), names(a))

  if(filterPrimary) {
    # remove primf and primn from the selection as they do not have
    # any bidirectional transitions (primary types cannot grow)
    filter2 <- grep("^(primf|primn)", names(a), invert = TRUE)
    filter <- intersect(filter, filter2)
  }
  b <- subset(a, filter)
  names(b) <- sub("^(.*)_to_(.*)_(.*)$", "\\3..\\1.\\2",names(b))
  m <- as.magpie(b)
  getYears(m) <- paste0("y",getYears(m, as.integer = TRUE) + 850)
  getSets(m, fulldim=FALSE)[3] <- "from.to"
  return(m)
}

extractStatesMean <- function(file = "states.nc", years=1995:2016, filterPrimary = TRUE) {
  # correct years to "years since 850"
  years <- years - 850
  # extract years from transitions.nc and return them in magpie format
  a <- rast(file)

  # only select transitions for the selected years
  filter <- grep(paste0("_(",paste(years, collapse="|"), ")$"), names(a))
  filter2 <- grep("^(secmb|secma)", names(a), invert = TRUE)
  filter <- intersect(filter, filter2)

  if(filterPrimary) {
    # remove primf and primn from the selection as they do not have
    # any bidirectional transitions (primary types cannot grow)
    filter2 <- grep("^(primf|primn)", names(a), invert = TRUE)
    filter <- intersect(filter, filter2)
  }
  b <- subset(a, filter)

  ty <- unique(sub("_.*$","",names(b)))

  out <- NULL
  for(t in ty) {
    tmp <- as.magpie(mean(subset(b, grep(t, names(b)))))
    getItems(tmp, dim = 3) <- t
    out <- mbind(out,tmp)
  }
  return(out)
}

getBidirectionalTransistions <- function(b) {
  # get transition pairs
  pairs <- data.frame(from = getItems(b, dim = 3))
  pairs$to <- sub("^(.*)\\.(.*)$","\\2.\\1",pairs$from)
  # create order independent id to filter for duplicate
  # entries (a.b-b.a and b.a-a.b)
  split <- strsplit(pairs$from,split = "\\.")
  split <- lapply(split, sort)
  pairs$id <- sapply(split, paste, collapse=".")
  pairs <- pairs[!duplicated(pairs$id),]
  # now filter for entries which actually exist in both directions
  pairs <- pairs[(pairs$to %in% getItems(b, dim = 3)) & (pairs$from %in% getItems(b, dim = 3)),]
  pairs$id <- NULL

  # compute bidirectional transitions
  bmin <- pmin(b[,,pairs$from],setItems(b[,,pairs$to], dim = 3, pairs$from, raw=TRUE))
  return(bmin)
}

range <- seq(1995,2011,4)
tSteps <- 4

for(i in range) {
  file <- paste0("../../mrdownscale_data/transitions_",i,"to",i+tSteps-1,".mz")
  write.magpie(extractTransitions("../../mrdownscale_data/LUH2v2/transitions.nc", years=(i:(i+3))),
               file)
  cat(basename(file),"written.")
}

out <- NULL
for(i in range) {
  file <- paste0("../../mrdownscale_data/transitions_",i,"to",i+tSteps-1,".mz")
  m <- read.magpie(file)
  m[m<0] <- 0 # fix data errors (negative values due to rounding issues)
  out <- mbind(out, m)
}
write.magpie(out, "../../mrdownscale_data/transitions_1995to2015.mz")

a <- read.magpie("../../mrdownscale_data/transitions_1995to2015.mz")
abi <- getBidirectionalTransistions(a)
write.magpie(abi, "../../mrdownscale_data/bidirectionalTransistions_1995to2015.mz")
meanBi <- dimSums(abi, dim = 2) / dim(abi)[2]
write.magpie(meanBi, "../../mrdownscale_data/meanBidirectionalTransistions_1995to2015.mz")

o <- extractStatesMean("../../mrdownscale_data/LUH2v2/states.nc")
write.magpie(o, "../../mrdownscale_data/meanStates_1995to2016.mz")




