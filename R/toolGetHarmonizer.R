toolGetHarmonizer <- function(harmonizerName) {
  harmonizers <- list(offset = toolHarmonizeOffset,
                      fade = toolHarmonizeFade,
                      extrapolateFade = toolHarmonizeExtrapolateFade)
  stopifnot(harmonizerName %in% names(harmonizers))
  return(harmonizers[[harmonizerName]])
}
