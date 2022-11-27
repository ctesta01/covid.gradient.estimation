library(here)
devtools::load_all(".")
library(ggdist)
library(mgcv)
library(mgcViz)
library(urbnmapr)
library(spdep)
library(sf)
library(gtable)
library(grid)

# load analytic dataset
df <- create_analytic_dataset()

# create a numeric version of date
df$date_int <- as.integer(lubridate::year(df$date)) + (as.integer(lubridate::month(df$date))-1)/12


# temporal autocorrelation ------------------------------------------------


# helper function to calculate missingness proportion
missing_proportion <- function(x) { sum(is.na(x)) / length(x) }

# format county observations for calculating ACF
mat_for_acf <-
  df %>% sf::st_drop_geometry() %>%
  ungroup() %>%
  select(geoid, date, crude_rate_per_100k_py) %>%
  tidyr::pivot_wider(
    id_cols = date,
    names_from = geoid,
    values_from = crude_rate_per_100k_py
  ) %>%
  arrange(date) %>%
  select(-date) %>%
  select(where(~ missing_proportion(.x) < .1)) %>%
  as.matrix()

# specify a maximum lag in months
lag_max <- 24

# calculate ACF for each county
ACF_mat <-
  purrr::map(1:ncol(mat_for_acf), ~ as.numeric(acf(
    na.omit(mat_for_acf[4:nrow(mat_for_acf), .]), lag.max = lag_max, plot =
      F
  )$acf))

# convert to data frame
ACF_mat %<>% bind_cols()

# add lag indicator column
ACF_mat %<>% mutate(lag = seq(0,lag_max))

# convert to tidy format
ACF_mat %<>% tidyr::pivot_longer(
  cols = setdiff(colnames(.), "lag"),
  names_to = "id",
  values_to = "cor"
)

# visualize distribution of ACF over time
ggplot(ACF_mat, aes(x = lag, y = cor)) +
  ggdist::stat_lineribbon() +
  scale_fill_brewer(palette = 'Reds') +
  geom_hline(yintercept = .35, linetype = 'dashed') +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_hline(yintercept = -.35, linetype = 'dashed') +
  scale_x_continuous(labels = seq(0, lag_max), breaks = seq(0, lag_max)) +
  ylab("Temporal Autocorrelation (ACF)") +
  xlab("Lag in Months") +
  labs(fill = "Observation\nQuantiles") +
  theme_bw()

# save visualization
ggsave(here("analysis/08_spatiotemporal_autocorrelation/temporal_acf.png"), width = 8, height = 3.5)


# seasonal effect ---------------------------------------------------------

df %<>% mutate(month = lubridate::month(date))

seasonality_model <- gam(
  formula = deaths ~ s(month),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = df
)

model_viz <- mgcViz::getViz(seasonality_model)

plot(sm(model_viz, 1), trans = exp) +
  geom_hline(yintercept = 1, linetype = 'dotted') +
  xlab("Month") +
  scale_x_continuous(breaks = 1:12) +
  ylab("Mortality Rate Ratio") +
  ggtitle("Seasonality Effects as Indicated by Mortality Rate Ratio Estimates by Month")

ggsave("analysis/08_spatiotemporal_autocorrelation/seasonality_1.png", width=8, height=2.5)

# second version excluding early 2020
seasonality_model <- gam(
  formula = deaths ~ s(month),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = df %>% filter(date_int >= 2020.75)
)

model_viz <- mgcViz::getViz(seasonality_model)

plot(sm(model_viz, 1), trans = exp) +
  geom_hline(yintercept = 1, linetype = 'dotted') +
  xlab("Month") +
  scale_x_continuous(breaks = 1:12) +
  ylab("Mortality Rate Ratio") +
  ggtitle("Seasonality with for Observations After September 2020")

ggsave("analysis/08_spatiotemporal_autocorrelation/seasonality_2.png", width=8, height=2.5)


# now with correlation structure

# add year
df %<>% mutate(year = lubridate::year(date))

seasonality_model <- gamm(
  formula = deaths ~ s(month) + offset(log(popsize/1e5/12)),
  correlation = corARMA(form = ~ date | geoid, p=1),
  family = poisson(link=log),
  data = df %>% filter(date_int >= 2020.75)
)

model_viz <- mgcViz::getViz(seasonality_model$gam)

plot(sm(model_viz, 1), trans = exp) +
  geom_hline(yintercept = 1, linetype = 'dotted') +
  xlab("Month") +
  scale_x_continuous(breaks = 1:12) +
  ylab("Mortality Rate Ratio") +
  ggtitle("Seasonality with corARMA(p=1) Structure for Observations After September 2020")

ggsave("analysis/08_spatiotemporal_autocorrelation/seasonality_3.png", width=8, height=2.5)


# spatial autocorrelation -------------------------------------------------

# get county and state maps for using in figures and spatial autocorrelation assessment
county_map <- urbnmapr::get_urbn_map('counties', sf=TRUE)
state_map <- urbnmapr::get_urbn_map('states', sf=TRUE)

# filter for just values from january 2021
df_jan2021 <- county_map %>% left_join(df %>% filter(date == "2021-01-01"), by = c('county_fips' = 'geoid'))

# plot the county map
ggplot(county_map) +
  geom_sf(size = .1) +
  geom_sf(data = state_map, size = .5, fill = NA) +
  theme_bw()

# calculate neighboring relationships
nb_q <- spdep::poly2nb(county_map)
coords <- sp::coordinates(as(county_map, 'Spatial'))

# convert to an sf format
neighbors_sf <- as(nb2lines(nb_q, coords = coords), 'sf')
neighbors_sf <- st_set_crs(neighbors_sf, st_crs(county_map))

# create figure showing the county neighboring relations
ggplot(county_map) +
  geom_sf(size = .1) +
  geom_sf(data = state_map, size = .5, fill = NA) +
  geom_sf(data = neighbors_sf, size = .1) +
  theme_bw()

# save figure
ggsave(here("analysis/08_spatiotemporal_autocorrelation/county_neighboring_relations.png"), width = 10, height = 6)

# use january 2021 covid mortality rates with no missing data
df_jan2021_no_NAs <- df_jan2021 %>% filter(! is.na(crude_rate_per_100k_py))
nb_q <- spdep::poly2nb(df_jan2021_no_NAs)

# use moran's I approach to assess spatial autocorrelation
mp <- moran.plot(x = df_jan2021_no_NAs$crude_rate_per_100k_py, listw = nb2listw(nb_q, zero.policy = TRUE), zero.policy = TRUE,
                 labels=as.character(paste0(df_jan2021_no_NAs$county, ", ", df_jan2021_no_NAs$state_abbv)), pch=19)

mp %<>% mutate(
  quadrant = case_when(
    x > mean(mp$x) & wx > mean(mp$wx) ~ 'high_high',
    x <= mean(mp$x) & wx > mean(mp$wx) ~ 'low_high',
    x <= mean(mp$x) & wx <= mean(mp$wx) ~ 'low_low',
    x > mean(mp$x) & wx <= mean(mp$wx) ~ 'high_low'
  )
)

color1 <- "#FF9C5B"
color2 <- "#F5634A"
color3 <- "#ED303C"
color4 <- "#3B8183"

ggplot(mp, aes(x=x, y=wx, color = quadrant)) + geom_point(alpha = .3) +
  geom_smooth(formula=y ~ x, method = 'lm', mapping = aes(group = 1), color = 'black') +
  geom_hline(yintercept=mean(mp$wx), lty=2) +
  geom_vline(xintercept=mean(mp$x), lty=2) + theme_minimal() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("County COVID-19 Mortality Rate per 100,000 Person-Years") +
  ylab("Average of Neighboring Counties' COVID-19\nMortality Rate per 100,000 Person-Years") +
  annotate(
    geom = 'label',
    x = 30, y = 50, label = stringr::str_wrap("Low rate counties surrounded by low rate counties", 20),
    alpha = .5,
    fill = color1
  ) +
  annotate(
    geom = 'label',
    x = 30, y = 2000, label = stringr::str_wrap("Low rate counties surrounded by high rate counties", 20),
    alpha = .5,
    fill = color2
  ) +
  annotate(
    geom = 'label',
    x = 3000, y = 2000, label = stringr::str_wrap("High rate counties surrounded by high rate counties", 20),
    alpha = .5,
    fill = color3
  ) +
  annotate(
    geom = 'label',
    x = 3000, y = 50, label = stringr::str_wrap("High rate counties surrounded by low rate counties", 20),
    alpha = .5,
    fill = color4
  ) +
  scale_color_manual(
    values = c('high_high' = color3, "low_high" = color2, "low_low" = color1, "high_low" = color4)
  ) +
  theme(legend.position = 'none')

ggsave(here("analysis/08_spatiotemporal_autocorrelation/morans_plot.png"), width = 8, height = 6, bg = 'white')

spdep::moran(
  x = df_jan2021_no_NAs$crude_rate_per_100k_py,
  listw = nb2listw(nb_q, zero.policy = TRUE),
  zero.policy = TRUE,
  n = nrow(df_jan2021_no_NAs),
  S0 = Szero(nb2listw(nb_q, zero.policy = TRUE))
)



# using bam for spatial component, single timeframe ----------------------------------

df_spatial <-
  county_map %>% left_join(df %>% filter(date == "2022-01-01"), by = c('county_fips' = 'geoid')) %>%
  filter(state_abbv %in% c(setdiff(state.abb, c('AK', 'HI')), 'DC'))
df_spatial %<>% sf::st_transform(crs = 'WGS84')

df_latlon <- sf::st_coordinates(sf::st_centroid(df_spatial))
df_for_bam <- bind_cols(sf::st_drop_geometry(df_spatial), df_latlon)
df_for_bam %<>% rename(lat = X, lon = Y)

model <- bam(
  formula = deaths ~ te(lat, lon, k = 5),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = df_for_bam
)

ggplot(df_spatial, aes(fill = crude_rate_per_100k_py)) +
  geom_sf(size = 0) +
  # scale_fill_distiller(palette = 'Reds', direction = 1) +
  scale_fill_viridis_c(option = 'turbo') +
  theme(legend.position = 'bottom')

ggsave(here("analysis/08_spatiotemporal_autocorrelation/base_map.png"))

# grid_latlon <- sf::st_coordinates(grid)
df_latlon %<>% as.data.frame()
df_latlon %<>% rename(lat = X, lon = Y)
df_latlon$pred_rate <- predict.bam(model, type = 'response', newdata = df_latlon)


my_triangle_colourbar <- function(...) {
  guide <- guide_colourbar(...)
  class(guide) <- c("my_triangle_colourbar", class(guide))
  guide
}

guide_gengrob.my_triangle_colourbar <- function(...) {
  # First draw normal colourbar
  guide <- NextMethod()
  # Extract bar / colours
  is_bar <- grep("^bar$", guide$layout$name)
  bar <- guide$grobs[[is_bar]]
  extremes <- c(bar$raster[1], bar$raster[length(bar$raster)])
  # Extract size
  width  <- guide$widths[guide$layout$l[is_bar]]
  height <- guide$heights[guide$layout$t[is_bar]]
  short  <- min(convertUnit(width, "cm",  valueOnly = TRUE),
                convertUnit(height, "cm", valueOnly = TRUE))
  # Make space for triangles
  guide <- gtable_add_rows(guide, unit(short, "cm"),
                           guide$layout$t[is_bar] - 1)
  guide <- gtable_add_rows(guide, unit(short, "cm"),
                           guide$layout$t[is_bar])

  # Draw triangles
  top <- polygonGrob(
    x = unit(c(0, 0.5, 1), "npc"),
    y = unit(c(0, 1, 0), "npc"),
    gp = gpar(fill = extremes[1], col = NA)
  )
  bottom <- polygonGrob(
    x = unit(c(0, 0.5, 1), "npc"),
    y = unit(c(1, 0, 1), "npc"),
    gp = gpar(fill = extremes[2], col = NA)
  )
  # Add triangles to guide
  guide <- gtable_add_grob(
    guide, top,
    t = guide$layout$t[is_bar] - 1,
    l = guide$layout$l[is_bar]
  )
  return(guide)
}

ggplot(df_spatial %>% sf::st_transform(crs = "EPSG:2163"), aes(fill = crude_rate_per_100k_py)) +
  geom_sf(size = 0) +
  scale_fill_viridis_c(
    option = 'turbo',
    direction = 1,
    breaks = c(0, 100, 200, 300, 400, 500),
    limits = c(0, 500),
    oob = scales::squish,
    guide = my_triangle_colourbar()

  ) +
  # scale_fill_distiller(
  #   palette = 'Reds',
  #   breaks = c(0, 100, 250, 500),
  #   direction = 1,
  #   trans = scales::sqrt_trans(),
  #   limits = c(0, 500),
  #   oob = scales::squish,
  #   guide = my_triangle_colourbar()
  # ) +
  labs(fill = stringr::str_wrap("Crude Mortality Rate per 100,000",10)) +
  theme_bw() +
  theme(legend.position = 'right')

ggsave("analysis/08_spatiotemporal_autocorrelation/crude_mortality_map_january2022.png", width = 10, height = 6, bg='white')

ggplot(df_spatial %>% sf::st_transform(crs = "EPSG:2163"), aes(fill = df_latlon$pred_rate)) +
  geom_sf(size = 0) +
  scale_fill_viridis_c(
    option = 'turbo',
    direction = 1,
    breaks = c(0, 100, 200, 300, 400, 500),
    limits = c(0, 500),
    oob = scales::squish,
    guide = my_triangle_colourbar()
  ) +
  # scale_fill_distiller(
  #   palette = 'Reds',
  #   breaks = c(0, 100, 500, 1000),
  #   direction = 1,
  #   trans = scales::sqrt_trans(),
  #   limits = c(0, 500),
  #   oob = scales::squish
  # ) +
  labs(fill = stringr::str_wrap("Spatially Smoothed Mortality Rate per 100,000", 10)) +
  theme_bw() +
  theme(legend.position = 'right')

ggsave("analysis/08_spatiotemporal_autocorrelation/smoothed_mortality_map_january2022.png", width = 10, height = 6, bg = 'white')

plot_grid(
  ggdraw() + draw_image(here("analysis/08_spatiotemporal_autocorrelation/crude_mortality_map_january2022.png")),
  ggdraw() + draw_image(here("analysis/08_spatiotemporal_autocorrelation/smoothed_mortality_map_january2022.png")),
  nrow = 1,
  labels = 'AUTO'
  )

ggsave(here("analysis/08_spatiotemporal_autocorrelation/side_by_side_crude_and_smoothed.png"), width = 10, height = 3.5, bg = 'white')

# spatiotemporal smoothing ------------------------------------------------

center_coords <- sf::st_centroid(county_map)
center_coord_latlons <- center_coords %>% sf::st_coordinates()

center_coords <- center_coord_latlons %>% as.data.frame() %>% rename('latitude' = X, 'longitude' = Y) %>%
  bind_cols(county_fips = county_map$county_fips)

spatiotemporal_df <-
  df %>%
  left_join(center_coords, by = c('geoid' = 'county_fips'))

spatiotemporal_df %<>% filter(state %in% setdiff(c(state.name, 'District of Columbia'), c("Alaska", "Hawaii")))

model <- bam(
  formula = deaths ~ te(latitude, longitude, date_int, d = c(2, 1)),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = spatiotemporal_df
)

selected_df <- spatiotemporal_df %>% filter(year == 2021, month == 3)

selected_df$predictions <- predict.bam(model, type = 'response', newdata = selected_df)

ggplot({county_map %>% left_join(
  selected_df,
  by = c('county_fips' = 'geoid'))} %>%
    filter(! state_abbv %in% c('AK', 'HI')) %>%
    sf::st_transform(crs = "EPSG:2163"), aes(fill = predictions)) +
  geom_sf(size = 0) +
  scale_fill_viridis_c(
    option = 'turbo',
    direction = 1,
    breaks = c(0, 100, 250, 500),
    limits = c(0, 500),
    oob = scales::squish
  ) +
  labs(fill = stringr::str_wrap("Spatio-temporally Smoothed Mortality Rate per 100,000", 10)) +
  theme_bw() +
  theme(legend.position = 'right')


