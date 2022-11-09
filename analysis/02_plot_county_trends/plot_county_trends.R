
# investigating suppression

# setup
devtools::load_all(".")
library(here)

# get data
df <- create_analytic_dataset()

# create plot
df %>%
  filter(!is.na(region)) %>%
  ggplot(aes(x = date, y = crude_rate_per_100k_py, group = geoid)) +
  geom_point(alpha = .05) +
  stat_lineribbon(
    mapping = aes(
      group = region,
      fill = region,
      color = region,
      fill_ramp = stat(level)
    ),
    alpha = .75,
    color = 'grey20'
  ) +
  facet_wrap( ~ region) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = 100),
    breaks = c(0, 100, 1000, 10000, 30000),
    limits = c(0, NA),
    labels = scales::comma_format()
  ) +
  theme_bw() +
  guides(
    fill = guide_none(),
    color = guide_none(),
    fill_ramp = guide_legend(title =
                               'Quantile Range')
  ) +
  ylab("COVID-19 Mortality Rate per 100,000 Person-Years") +
  ggtitle(
    "County Observations of COVID-19 Mortality Rates by Region",
    "Median and Quantile Ranges Across Counties"
  ) +
  theme(legend.position = 'bottom')

ggsave(here("analysis/02_plot_county_trends/county_trends.png"), height = 6, width = 8)


df %>%
  filter(!is.na(division)) %>%
  ggplot(aes(x = date, y = crude_rate_per_100k_py, group = geoid)) +
  geom_point(alpha = .05) +
  stat_lineribbon(
    mapping = aes(
      group = division,
      fill = division,
      color = division,
      fill_ramp = stat(level)
    ),
    alpha = .75,
    color = 'grey20'
  ) +
  facet_wrap( ~ division) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = 100),
    breaks = c(0, 250, 1000, 3500, 10000, 30000),
    limits = c(0, NA),
    labels = scales::comma_format()
  ) +
  theme_bw() +
  guides(
    fill = guide_none(),
    color = guide_none(),
    fill_ramp = guide_legend(title =
                               'Quantile Range')
  ) +
  ylab("COVID-19 Mortality Rate per 100,000 Person-Years") +
  ggtitle(
    "County Observations of COVID-19 Mortality Rates by Month and by U.S. County Division",
    "Median and Quantile Ranges Across Counties"
  ) +
  theme(legend.position = 'bottom')

ggsave(here("analysis/02_plot_county_trends/county_trends_by_division.png"), height = 6, width = 10)

