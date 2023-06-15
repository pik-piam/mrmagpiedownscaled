fullMAGPIELUH <- function() {
  states <- calcOutput("MagpieStatesLUH", aggregate = FALSE)
  write.magpie(states, "magpie_states.nc")
}
