
#' Get State Population Data
get_state_popsizes <- function() {
  tidycensus::get_decennial(
    geography = 'state',
    variables = "P1_001N",
    year = 2020
  )
}
