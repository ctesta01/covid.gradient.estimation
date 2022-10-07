
# setup
devtools::load_all(".")
library(here)
library(plotly)
library(htmlwidgets)

# load data
deaths <- load_state_covid19_deaths()
popsizes <- get_state_popsizes()

# join in population size estimates
deaths %<>% left_join(
  popsizes %>% select(GEOID, value) %>% rename(popsize = value),
  by = c('residence_state_code' = 'GEOID')
)

# convert "Suppressed" to NA and the column to numeric
deaths %<>% mutate(
  deaths = ifelse(deaths != 'Suppressed', as.numeric(deaths), NA)
)

# create deaths per 100,000 person-years
deaths %<>% mutate(crude_rate_per_100k_py = deaths / popsize * 1e5 * 12)

# make sure month code is ordered
deaths %<>% mutate(month_code = ordered(month_code))

# add divisions to the dataset
deaths %<>% mutate(
  division = c(setNames(as.character(state.division), state.name), 'District of Columbia' = 'South Atlantic')[residence_state],
  region = c(setNames(as.character(state.region), state.name), 'District of Columbia' = 'South')[residence_state]
)

# incorporate state divisions
deaths %<>% mutate(division = factor(
  division,
  levels = c(
    'Pacific',
    'West North Central',
    'New England',
    'Mountain',
    'East North Central',
    'Middle Atlantic',
    'West South Central',
    'East South Central',
    'South Atlantic'
  )
))

# highlight new york and the midwest states
ny_highlight <- deaths %>% slice_max(crude_rate_per_100k_py, n = 1)
dakota_highlight <-
  deaths %>% filter(residence_state == 'North Dakota') %>% slice_max(crude_rate_per_100k_py, n = 1)

# create our timeseries figure
plt <-
  deaths %>% mutate(
    month = lubridate::ym(month_code)
  ) %>%
  ggplot(
    aes(
      x = month,
      y = crude_rate_per_100k_py,
      color = residence_state
    )) +
  geom_line(alpha = .5) +
  geom_label(
    data = ny_highlight,
    mapping = aes(x = lubridate::ym(month_code) + 500,
                  y = crude_rate_per_100k_py * .75),
    label = stringr::str_wrap("New York and New Jersey experienced early surges peaking in March 2020", 25),
    color = 'grey20',
    size = 2.5
  ) +
  geom_label(
    data = dakota_highlight,
    mapping = aes(x = lubridate::ym(month_code) + 350,
                  y = crude_rate_per_100k_py * 1.15),
    alpha = .65,
    label = stringr::str_wrap("Midwest states like Iowa, Kansas, Minnesota, Missouri, Nebraska, and the Dakotas had their first large surges peaking in November 2020", 30),
    color = 'grey20',
    size = 2.25
  ) +
  ggtitle("Monthly COVID-19 Mortality Rates per 100,000 Person-Years by State", "States are grouped by US Census Division") +
  xlab("Date") +
  ylab("Mortality per 100,000 Person-Years") +
  theme_bw(base_size = 10) +
  facet_wrap(~division) +
  scale_x_date(breaks = "6 months", labels = scales::date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1),
        legend.position = 'none')

# save the static png file
ggsave(here("analysis/01_state_trajectories/state_covid19_mortality_trajectories.png"), width = 8, height = 5.5)

# create an interactive version to save
interactive_plt <- { plt + xlab("") + theme(legend.position = 'bottom') } %>% ggplotly()
saveWidget(interactive_plt, here("analysis/01_state_trajectories/state_trajectories.html"))
