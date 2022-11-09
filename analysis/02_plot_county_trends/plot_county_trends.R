
# investigating suppression

# setup
devtools::load_all(".")
library(here)
library(Hmisc)

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


plt_orig <- df %>%
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
  geom_line(
    mapping = aes(size = ' ')
  ) +
  facet_wrap( ~ division) +
  scale_size_manual(values = c(' ' = 1)) +
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
                               'Weighted Quantile Range')
  ) +
  ylab("COVID-19 Mortality Rate per 100,000 Person-Years") +
  labs(size = 'Weighted Median') +
  ggtitle(
    "County Observations of COVID-19 Mortality Rates by Month and by U.S. County Division",
    "Median and Quantile Ranges Across Counties"
  ) +
  theme(legend.position = 'bottom')


legend <- cowplot::get_legend(plt_orig)

# create manually using weighted quantiles --------------------------------

wtd_quantile_df <-
  df %>%
  filter(! is.na(division)) %>%
  group_by(division, date) %>%
  dplyr::summarize(
    median = Hmisc::wtd.quantile(x = crude_rate_per_100k_py, weights = popsize, probs = .5, na.rm=T),
    ci50_high = Hmisc::wtd.quantile(x = crude_rate_per_100k_py, weights = popsize, probs = .75, na.rm=T),
    ci50_low = Hmisc::wtd.quantile(x = crude_rate_per_100k_py, weights = popsize, probs = .25, na.rm=T),
    ci80_high = Hmisc::wtd.quantile(x = crude_rate_per_100k_py, weights = popsize, probs = .9, na.rm=T),
    ci80_low = Hmisc::wtd.quantile(x = crude_rate_per_100k_py, weights = popsize, probs = .1, na.rm=T),
    ci95_high = Hmisc::wtd.quantile(x = crude_rate_per_100k_py, weights = popsize, probs = .975, na.rm=T),
    ci95_low = Hmisc::wtd.quantile(x = crude_rate_per_100k_py, weights = popsize, probs = .025, na.rm=T)
  )

palette <- scales::hue_pal()(9) # RColorBrewer::brewer.pal(n = 9, name = 'Set1')
palette_named <- setNames(palette, unique(wtd_quantile_df$division))

plt <-
  ggplot(wtd_quantile_df, aes(x = date)) +
  geom_point(
    data = df %>% filter(! is.na(division)),
    mapping = aes(y = crude_rate_per_100k_py, group = geoid),
    color = 'grey40',
    alpha = .05) +
  geom_ribbon(
    mapping = aes(ymax = ci95_high, ymin = ci95_low, group = NULL, y = NULL),
    size = 0,
    alpha = 0.7,
    fill = colorspace::lighten(palette_named[wtd_quantile_df$division], .7)
  ) +
  geom_ribbon(
    mapping = aes(ymax = ci80_high, ymin = ci80_low, group = NULL, y = NULL),
    size = 0,
    alpha = 0.7,
    fill = colorspace::lighten(palette_named[wtd_quantile_df$division], .3)
  ) +
  geom_ribbon(
    mapping = aes(ymax = ci50_high, ymin = ci50_low, group = NULL, y = NULL),
    size = 0,
    alpha = 0.8,
    fill = palette_named[wtd_quantile_df$division]
  ) +
  geom_line(
    mapping = aes(y = median, group = NULL),
    color = 'black',
    size = 1
  ) +
  facet_wrap( ~ division) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = 100),
    breaks = c(0, 250, 1000, 3500, 10000, 30000),
    limits = c(0, NA),
    labels = scales::comma_format()
  ) +
  theme_bw() +
  ylab("COVID-19 Mortality Rate per 100,000 Person-Years") +
  ggtitle(
    "County Observations of COVID-19 Mortality Rates by Month and by U.S. County Division",
    "Median and Quantile Ranges Across Counties Weighted by Population Size"
  ) +
  theme(legend.position = 'bottom')

ggdraw(plot_grid(plt, legend, nrow = 2, rel_heights = c(1,.1)))

ggsave(here("analysis/02_plot_county_trends/county_trends_by_division.png"), height = 6, width = 10, bg = 'white')

