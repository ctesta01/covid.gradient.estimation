
#' Load County COVID-19 Deaths
load_county_covid19_deaths <- function(clean = TRUE) {

  # load data
  dfs <- purrr::map(
    2020:2022, ~ readr::read_csv(system.file(
      paste0(
        "nytimes/",
        "us-counties-", ., ".csv"
      ),
      package = 'covid.gradient.estimation'
    ))
  )

  # clean data
  if (clean) {
    dfs %<>% bind_rows()
    dfs$geoid %<>% stringr::str_extract("[0-9]{5}")
  }

  return(dfs)
}
