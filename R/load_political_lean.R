
#' Load MIT Election Lab County Level Presidential Returns
load_mit_election_data <- function() {
  df <- readr::read_csv(
    system.file("mit_election_lab/countypres_2000-2020.csv", package = "covid.gradient.estimation")
  )

  df %<>% filter(year == 2020) # filter for just 2020 results

  # combine the election day, early mail-in, and provisional votes
  df %<>% select(county_fips, party, candidatevotes, totalvotes) %>%
    group_by(county_fips, party) %>% summarize(across(everything(), ~sum(.)))

  df %<>% ungroup()

  # pivot wider so we can calculate political lean
  df %<>% tidyr::pivot_wider(
      id_cols = c(county_fips, totalvotes),
      names_from = party,
      values_from = candidatevotes
    )

  # calculate political lean
  df %<>% mutate(
    political_lean = (REPUBLICAN - DEMOCRAT) / totalvotes
  )

  # save only necessary info
  df %<>% select(county_fips, political_lean)

  return(df)
}
