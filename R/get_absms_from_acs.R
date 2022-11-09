
#' Get Area Based Social Metrics from ACS
#'
#' @examples
#' absms <- get_absms_from_acs()
#' saveRDS(absms, file.path(system.file('', package = 'covid.gradient.estimation'), 'cached_absms.rds'))
get_absms_from_acs <- function() {

  absms_dictionary <- tibble::tribble(
    ~var, ~varname, ~description,
    # total population
    "B01001_001",  "total_popsize", "total population estimate",

    # age breakdown
    "B01002_001",   "median_age",   "median age for total population",
    "B06001_001",   "total_for_age", "total population for age distribution",
    "B06001_011",   "65_to_74",     "population aged 65 to 74",
    "B06001_012",   "75_and_up",    "population aged 75 and up",

    # college education
    "B06009_001",   "education_popsize", "total population estimate for education tables",
    "B06009_002",   "less_than_hs",   "less than a high school or GED equivalent",
    "B06009_005",   "college_degree", "bachelors degree",
    "B06009_006",   "graduate_degree", "graduate degree",

    # racial composition
    'B01003_001',  "race_ethnicity_total", "race_ethnicity_total",

    # ICEraceinc
    "B19001_001",  'hhinc_total',   "total population for household income estimates",
    "B19001A_002", 'hhinc_w_1',     "white n.h. pop with household income <$10k",
    "B19001A_003", 'hhinc_w_2',     "white n.h. pop with household income $10k-14 999k",
    "B19001A_004", 'hhinc_w_3',     "white n.h. pop with household income $15k-19 999k",
    "B19001A_005", 'hhinc_w_4',     "white n.h. pop with household income $20k-24 999k",
    "B19001A_014", 'hhinc_w_5',     "white n.h. pop with household income $100 000 to $124 999",
    "B19001A_015", 'hhinc_w_6',     "white n.h. pop with household income $125k-149 999k",
    "B19001A_016", 'hhinc_w_7',     "white n.h. pop with household income $150k-199 999k",
    "B19001A_017", 'hhinc_w_8',     "white n.h. pop with household income $196k+",
    "B19001_002",  'hhinc_total_1', "total pop with household income <$10k",
    "B19001_003",  'hhinc_total_2', "total pop with household income $10k-14 999k",
    "B19001_004",  'hhinc_total_3', "total pop with household income $15k-19 999k",
    "B19001_005",  'hhinc_total_4', "total pop with household income $20k-24 999k",

    # poverty
    "B05010_002",  'in_poverty',    "population with household income < poverty line",
    "B05010_001",  'total_pop_for_poverty_estimates',  "total population for poverty estimates",

    # median income
    "B06011_001",  'median_income',  "median income estimate for total population",

    # crowded housing
    "B25014_005",  'owner_occupied_crowding1', 'owner occupied, 1 to 1.5 per room',
    "B25014_006",  'owner_occupied_crowding2', 'owner occupied, 1.51 to 2 per room',
    "B25014_007",  'owner_occupied_crowding3', 'owner occupied, 2.01 or more per room',
    "B25014_011",  'renter_occupied_crowding1', 'owner occupied, 1 to 1.5 per room',
    "B25014_012",  'renter_occupied_crowding2', 'owner occupied, 1.51 to 2 per room',
    "B25014_013",  'renter_occupied_crowding3', 'owner occupied, 2.01 or more per room',
    "B25014_001",  'crowding_total',            'total for crowding (occupants per room)',

    "B01001I_001",  'total_hispanic',           'total hispanic population estimate',
    "B01001B_001",  'total_black',              'total black, hispanic or non-hispanic estimate',
    "B01001H_001",  'total_white_nh',           'total white, non-hispanic population estimate',
    "B01001C_001",  'total_aian',               'total american indian, alaska native population estimate',
    "B01001D_001",  'total_asian',              'total asian population estimate',
    "B01001E_001",  'total_nhopi',              'total native hawaiian, other pacific islander'
  )

  absms <- tidycensus::get_acs(
    year = 2019,
    geography = 'county',
    variables = absms_dictionary$var, #Get the variables indicated in the data dictionary.
    geometry = TRUE,
    keep_geo_vars = TRUE
  )

  # we only request the geometry because we want the land area variables to calculate
  # population density -- so here we drop the geometry, retaining the ALAND variable we want
  absms %<>% sf::st_drop_geometry()

  # pivot wider so that each row corresponds to a ZCTA
  absms %<>% dplyr::select(-moe) %>%
    tidyr::pivot_wider(names_from = variable, values_from = estimate)
  # Change the new column names to reflect variables names from the dictionary
  rename_vars <- setNames(absms_dictionary$var, absms_dictionary$varname)
  absms <- absms %>% rename(!!rename_vars)

  absms %<>%
    mutate(
      # we calculate the people of color low income counts as the overall
      # low income counts minus the white non-hispanic low income counts
      people_of_color_low_income =
        (hhinc_total_1 + hhinc_total_2 + hhinc_total_3 + hhinc_total_4) -
        (hhinc_w_1 + hhinc_w_2 + hhinc_w_3 + hhinc_w_4),
      # sum up the white non-hispanic high income counts
      white_non_hispanic_high_income =
        (hhinc_w_5 + hhinc_w_6 + hhinc_w_7 + hhinc_w_8),
      # calculate the index of concentration at the extremes for racialized
      # economic segregation (high income white non-hispanic vs. low income
      # people of color)
      ICEraceinc =
        (white_non_hispanic_high_income - people_of_color_low_income) /
        hhinc_total,

      prop_in_poverty = in_poverty / total_pop_for_poverty_estimates,

      crowding = (owner_occupied_crowding1 + owner_occupied_crowding2 + owner_occupied_crowding3 +
                    renter_occupied_crowding1 + renter_occupied_crowding2 + renter_occupied_crowding3) / crowding_total,

      # age distribution
      prop_65_and_up = (`65_to_74` + `75_and_up`) / total_for_age,
      prop_75_and_up = `75_and_up` / total_for_age,

      prop_black = total_black / total_popsize,
      prop_hispanic = total_hispanic / total_popsize,
      prop_white_nh = total_white_nh / total_popsize,
      prop_asian = total_asian / total_popsize,
      prop_aian = total_aian / total_popsize,
      prop_nhopi = total_nhopi / total_popsize,

      # population with a degree
      prop_no_hs = less_than_hs / education_popsize,
      prop_college = (college_degree + graduate_degree) / education_popsize,

      # population density
      pop_density = total_popsize / ( ALAND / 2589988.11) # divide ALAND by 2589988.11 to get sq. miles from sq. meters

    ) %>%
    dplyr::select(
      GEOID,
      ICEraceinc,
      prop_in_poverty,
      median_income,
      median_age,
      prop_65_and_up,
      prop_75_and_up,
      crowding,
      prop_black,
      prop_hispanic,
      prop_white_nh,
      prop_asian,
      prop_aian,
      prop_nhopi,
      prop_no_hs,
      prop_college,
      pop_density)

  return(absms)
}


#' Load Cached ABSMs
load_cached_absms <- function() {
  readRDS(system.file('cached_absms.rds', package = 'covid.gradient.estimation'))
}
