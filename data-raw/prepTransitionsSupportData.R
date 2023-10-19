library(terra) # nolint: undesirable_function_linter
library(magclass) # nolint: undesirable_function_linter


extractTransitions <- function(file = "transistions.nc", years = 2014:2015, filterPrimary = TRUE) {
  # correct years to "years since 850"
  years <- years - 850
  # extract years from transitions.nc and return them in magpie format
  a <- rast(file)

  # only select transitions for the selected years
  filter <- grep(paste0("_to_.*_(", paste(years, collapse = "|"), ")$"), names(a))

  if (filterPrimary) {
    # remove primf and primn from the selection as they do not have
    # any bidirectional transitions (primary types cannot grow)
    filter2 <- grep("^(primf|primn)", names(a), invert = TRUE)
    filter <- intersect(filter, filter2)
  }
  b <- subset(a, filter)
  names(b) <- sub("^(.*)_to_(.*)_(.*)$", "\\3..\\1.\\2", names(b))
  m <- as.magpie(b)
  getYears(m) <- paste0("y", getYears(m, as.integer = TRUE) + 850)
  getSets(m, fulldim = FALSE)[3] <- "from.to"
  return(m)
}

extractStatesMean <- function(file = "states.nc", years = 1995:2016, filterPrimary = TRUE) {
  # correct years to "years since 850"
  years <- years - 850
  # extract years from transitions.nc and return them in magpie format
  a <- rast(file)

  # only select transitions for the selected years
  filter <- grep(paste0("_(", paste(years, collapse = "|"), ")$"), names(a))
  filter2 <- grep("^(secmb|secma)", names(a), invert = TRUE)
  filter <- intersect(filter, filter2)

  if (filterPrimary) {
    # remove primf and primn from the selection as they do not have
    # any bidirectional transitions (primary types cannot grow)
    filter2 <- grep("^(primf|primn)", names(a), invert = TRUE)
    filter <- intersect(filter, filter2)
  }
  b <- subset(a, filter)

  ty <- unique(sub("_.*$", "", names(b)))

  out <- NULL
  for (t in ty) {
    tmp <- as.magpie(mean(subset(b, grep(t, names(b)))))
    getItems(tmp, dim = 3) <- t
    out <- mbind(out, tmp)
  }
  return(out)
}

getBidirectionalTransistions <- function(b) {
  # get transition pairs
  pairs <- data.frame(from = getItems(b, dim = 3))
  pairs$to <- sub("^(.*)\\.(.*)$", "\\2.\\1", pairs$from)
  # create order independent id to filter for duplicate
  # entries (a.b-b.a and b.a-a.b)
  split <- strsplit(pairs$from, split = "\\.")
  split <- lapply(split, sort)
  pairs$id <- sapply(split, paste, collapse = ".") # nolint: undesirable_function_linter
  pairs <- pairs[!duplicated(pairs$id), ]
  # now filter for entries which actually exist in both directions
  pairs <- pairs[(pairs$to %in% getItems(b, dim = 3)) & (pairs$from %in% getItems(b, dim = 3)), ]
  pairs$id <- NULL

  # compute bidirectional transitions
  bmin <- pmin(b[, , pairs$from], setItems(b[, , pairs$to], dim = 3, pairs$from, raw = TRUE))
  return(bmin)
}


ncFolder <- "../../mrdownscale_data/LUH2v2/"
computationFolder <- "../../mrdownscale_data/"

# 20 year time horizon 1995 - 2016
# 4 year packages (to stay in memory limits)
range <- seq(1995, 2011, 4)
tSteps <- 4

# extract transition information in digestable blocks
for (i in range) {
  file <- file.path(computationFolder, paste0("transitions_", i, "to", i + tSteps - 1, ".mz"))
  write.magpie(extractTransitions(file.path(ncFolder, "transitions.nc"), years = (i:(i + 3))),
               file)
  cat(basename(file), "written.")
}

# read in blocks and generate single 20 year block
out <- NULL
for (i in range) {
  file <- file.path(computationFolder, paste0("transitions_", i, "to", i + tSteps - 1, ".mz"))
  m <- read.magpie(file)
  m[m < 0] <- 0 # fix data errors (negative values due to rounding issues)
  out <- mbind(out, m)
}
write.magpie(out, file.path(computationFolder, "transitions_1995to2015.mz"))

# compute bidirectional transitions (transitions which happen in a cell in the
# same size in both directions or in other words: transitions, which don't lead
# to net transitions)
a <- read.magpie(file.path(computationFolder, "transitions_1995to2015.mz"))
abi <- getBidirectionalTransistions(a)
write.magpie(abi, file.path(computationFolder, "bidirectionalTransistions_1995to2015.mz"))
# compute mean bilateral transitions over the selected time period
meanBi <- dimSums(abi, dim = 2) / dim(abi)[2]
write.magpie(meanBi, file.path(computationFolder, "meanBidirectionalTransistions_1995to2015.mz"))

# calculate corresponding mean state levels for the corresponding period
states <- extractStatesMean(file.path(ncFolder, "states.nc"), years = 1995:2016)
write.magpie(states, file.path(computationFolder, "meanStates_1995to2016.mz"))

# for every possible transition between two states select the smaller state area
# of the two as reference area for the corresponding transition
smallerArea <- mrdownscale:::toolGetSmallerArea(states)
smallerArea <- smallerArea[,,getItems(meanBi, dim = 3)]

# remove very small values
meanBi[meanBi < 10^-6] <- 0

# compute transformation share relative to the smaller area to have a
# area-scalable representative value of the bidirectional transistions
x <- meanBi/(smallerArea+10^-10)


