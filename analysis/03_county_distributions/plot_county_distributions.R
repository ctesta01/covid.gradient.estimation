library(here)
devtools::load_all(".")
library(ggcorrplot)

# load our area based measures we'll be considering
absms <- load_cached_absms()

# load our analytic dataset too
df <- create_analytic_dataset()

# pull out the region information
absms %<>% left_join(df %>% select(geoid, region) %>% unique(), by = c('GEOID' = 'geoid'))

# first I think we want a panel of univariate distributions (histograms)
# showing how the counties fall across the measures considered

# broadly we can categorize these into:
#
# purely demographic
#
# purely economic
#
# and socioeconomic
#

library(ggridges)

absms %>% filter(!is.na(region)) %>% mutate(region = as.character(region)) %>%
  ggplot(aes(x = prop_black, y = region)) +
  geom_density_ridges(stat = 'binline', scale = .80) +
  # scale_x_continuous(labels = scales::percent_format()) +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .05)) + # breaks = c(0, 10, 100, 500, 10000), labels = scales::comma_format()) +
  scale_y_discrete(expand = expansion(add = c(.3, 1.1))) +
  theme_bw() +
  labs(x = "Percent Black")

absms %>% filter(!is.na(region)) %>% mutate(region = as.character(region)) %>%
  ggplot(aes(x = prop_hispanic, y = region)) +
  geom_density_ridges(stat = 'binline', scale = .90) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(x = "Percent Hispanic")

absms %>% filter(!is.na(region)) %>% mutate(region = as.character(region)) %>%
  ggplot(aes(x = prop_white_nh, y = region)) +
  geom_density_ridges(stat = 'binline', scale = .90) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(x = "Percent Non-Hispanic White")

absms %>% filter(!is.na(region)) %>% mutate(region = as.character(region)) %>%
  ggplot(aes(x = prop_aian, y = region)) +
  geom_density_ridges(stat = 'binline', scale = .90) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(x = "Percent American Indian or Alaska Native")

absms %>% filter(!is.na(region)) %>% mutate(region = as.character(region)) %>%
  ggplot(aes(x = prop_nhopi, y = region)) +
  geom_density_ridges(stat = 'binline', scale = .90) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(x = "Percent Native Hawaiian or Other Pacific Islander")

absms %>% filter(!is.na(region)) %>% mutate(region = as.character(region)) %>%
  ggplot(aes(x = median_age, y = region)) +
  geom_density_ridges(stat = 'binline', scale = .90) +
  theme_bw() +
  labs(x = "Median Age")

absms %>% filter(!is.na(region)) %>% mutate(region = as.character(region)) %>%
  ggplot(aes(x = prop_65_and_up, y = region)) +
  geom_density_ridges(stat = 'binline', scale = .90) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(x = "Percent 65 and Older")

absms %>% filter(!is.na(region)) %>% mutate(region = as.character(region)) %>%
  ggplot(aes(x = prop_75_and_up, y = region)) +
  geom_density_ridges(stat = 'binline', scale = .90) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(x = "Percent 75 and Older")

absms %>% filter(!is.na(region)) %>% mutate(region = as.character(region)) %>%
  ggplot(aes(x = crowding, y = region)) +
  geom_density_ridges(stat = 'binline', scale = .90) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(x = "Percent of Households Experiencing Crowding")

absms %>% filter(!is.na(region)) %>% mutate(region = as.character(region)) %>%
  ggplot(aes(x = pop_density, y = region)) +
  geom_density_ridges(stat = 'binline', scale = .90) +
  scale_x_continuous(trans = scales::log1p_trans(), breaks = c(0, 10, 100, 500, 10000), labels = scales::comma_format()) +
  theme_bw() +
  labs(x = "Population Density per Square Mile")

# rename variables for with more friendly labels
corr <-
  absms %>% select(ICEraceinc:pop_density) %>%
  rename(
    'Index of Concentration at the Extremes for\nRacialized Economic Segregation' = ICEraceinc,
    'Proportion in Poverty' = prop_in_poverty,
    'Median Income' = median_income,
    'Median Age' = median_age,
    'Proportion 65+' = 'prop_65_and_up',
    'Proportion 75+' = prop_75_and_up,
    'Proportion of Households\nExperiencing Crowding' = crowding,
    'Proportion Black' = prop_black,
    'Proportion Hispanic' = prop_hispanic,
    'Proportion Non-Hispanic White' = prop_white_nh,
    'Proportion Asian' = prop_asian,
    'Proportion American Indian\nor Alaska Native' = prop_aian,
    'Proportion Native Hawaiian\nor Other Pacific Islander' = prop_nhopi,
    'Population Density per Sq. Mi.' = pop_density
  ) %>%
  cor(use = 'pairwise.complete.obs')

# create correlation figure
ggcorrplot(
  corr,
  type = "lower",
  outline.color = "white",
  ggtheme = ggplot2::theme_gray,
  colors = c("#6D9EC1", "white", "#E46726"),
  lab = TRUE,
  legend.title = 'Correlation \u03C1',
  lab_size = 3
) +
  scale_y_discrete(position = 'right') +
  theme_bw() +
  xlab("") +
  ylab("") +
  ggtitle("Correlation Among County Level Area Based Measures") +
  theme(
    axis.text.x = element_text(angle = 75, hjust = 1),
    plot.margin = margin(1,1,1.5,1.2, "cm"))

# save correlation figure
ggsave(here("analysis/03_county_distributions/correlation_figure.png"), height = 8, width = 10)
