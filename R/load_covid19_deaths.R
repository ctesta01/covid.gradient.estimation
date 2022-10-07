
#' Load State COVID-19 Deaths
load_state_covid19_deaths <- function(clean = TRUE) {

  # load data
  df <- readr::read_delim(file = system.file(
      "cdc_wonder/States - Provisional Mortality Statistics, 2018 through Last Month.txt",
      package = 'covid.gradient.estimation'
    )
  )

  # optional cleaning steps
  if (clean) {
    df %<>% janitor::clean_names()
    df %<>% filter(is.na(notes))
    df %<>% select(-notes, -population, -crude_rate)
  }

  return(df)
}
