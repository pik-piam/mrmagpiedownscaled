#' toolGetHarmonizer
#'
#' Get a harmonizer function by name.
#'
#' @param harmonizerName name of a harmonizer function, currently offset, fade, extrapolateFade
#' @return harmonizer function
#' @author Pascal Sauer
toolGetHarmonizer <- function(harmonizerName) {
  toolHarmonizeExtrapolateFadeConstantSum <- function(...) toolHarmonizeExtrapolateFade(..., constantSum = TRUE)
  toolHarmonizeExtrapolateFadeDynamicSum  <- function(...) toolHarmonizeExtrapolateFade(..., constantSum = FALSE)

  harmonizers <- list(offset = toolHarmonizeOffset,
                      fade = toolHarmonizeFade,
                      extrapolateFadeConstantSum = toolHarmonizeExtrapolateFadeConstantSum,
                      extrapolateFadeDynamicSum = toolHarmonizeExtrapolateFadeDynamicSum)
  stopifnot(harmonizerName %in% names(harmonizers))
  return(harmonizers[[harmonizerName]])
}
