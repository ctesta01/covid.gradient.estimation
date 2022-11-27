# dependencies
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
library(cowplot)
library(patchwork)


# load analytic dataset
df <- create_analytic_dataset()

# create a numeric version of date
df$date_int <- as.integer(lubridate::year(df$date)) + (as.integer(lubridate::month(df$date))-1)/12

# add year
df %<>% mutate(year = lubridate::year(date))
df %<>% mutate(month = lubridate::month(date))


# get county and state maps for using in figures and spatial autocorrelation assessment
county_map <- urbnmapr::get_urbn_map('counties', sf=TRUE)
state_map <- urbnmapr::get_urbn_map('states', sf=TRUE)

# calculate county centroids
center_coords <- sf::st_centroid(county_map)
center_coord_latlons <- center_coords %>% sf::st_coordinates()

center_coords <- center_coord_latlons %>% as.data.frame() %>% rename('latitude' = X, 'longitude' = Y) %>%
  bind_cols(county_fips = county_map$county_fips)

# construct spatiotemporal data frame
spatiotemporal_df <-
  df %>%
  left_join(center_coords, by = c('geoid' = 'county_fips'))

# restrict to contiguous 48 states
spatiotemporal_df %<>% filter(state %in% setdiff(c(state.name, 'District of Columbia'), c("Alaska", "Hawaii")))

# fit bam (e.g., GAM optimized for very large datasets)
model <- bam(
  formula = deaths ~ s(median_age) + te(latitude, longitude, date_int, d = c(2, 1)),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = spatiotemporal_df
)

# introduce custom color bar
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


# function to plot the model predictions for a given year and month
visualize_gam_map <- function(model, year, month, fulltitle = TRUE) {

  # filter our dataframe for relevant observations
  selected_df <- spatiotemporal_df %>% filter(year == {{ year }}, month == {{ month }})

  # predict
  selected_df$predictions <- predict.bam(model, type = 'response', newdata = selected_df)

  # generate figure
  ggplot({county_map %>% left_join(
    selected_df,
    by = c('county_fips' = 'geoid'))} %>%
      filter(! state_abbv %in% c('AK', 'HI')) %>%
      sf::st_set_crs("EPSG:9311"), aes(fill = predictions)) +
    geom_sf(size = 0) +
    scale_fill_viridis_c(
      option = 'turbo',
      direction = 1,
      breaks = c(0, 125, 250, 375, 500),
      limits = c(0, 500),
      oob = scales::squish,
      guide = my_triangle_colourbar()
    ) +
    labs(fill = "Smoothed\nMortality\nRate") +
    theme_bw() +
    ggtitle(
      if (fulltitle) "Spatio-temporally Smoothed Mortality Rate per 100,000 Person-Years" else "",
      subtitle = paste0(month.name[month], ", ", year)
    )+
    theme(legend.position = 'right')
}

visualize_gam_map(model, 2021, 3)


# create frames for animation
year_vec <- c(rep(2020, 10), rep(2021, 12), rep(2022, 8))
month_vec <- c(3:12, 1:12, 1:8)

for (i in 1:length(year_vec)) {
  plt <- visualize_gam_map(model, year_vec[i], month_vec[i])
  ggsave(
    filename = here(
      paste0("analysis/09_spatiotemporal_models/animation/",
             stringr::str_pad(
               i,
               side = 'left',
               width = 2,
               pad = '0'
             ),
             ".png"
      )),
    plot = plt,
    width = 10, height = 6)
}

# combine frames to create animation
setwd(here("analysis/09_spatiotemporal_models/animation/"))
system("convert -delay 40 -loop 0 *.png spatiotemporal_animation.gif \( +clone -set delay 500 \) +swap +delete animation_with_pause.gif")

# create temporal panel figure

visualize_gam_map(model, year_vec[5], month_vec[5], fulltitle = F) +
  visualize_gam_map(model, year_vec[6], month_vec[6], fulltitle = F) +
  visualize_gam_map(model, year_vec[7], month_vec[7], fulltitle = F) +
  visualize_gam_map(model, year_vec[8], month_vec[8], fulltitle = F) +

  visualize_gam_map(model, year_vec[9], month_vec[9], fulltitle = F) +
  visualize_gam_map(model, year_vec[10], month_vec[10], fulltitle = F) +
  visualize_gam_map(model, year_vec[11], month_vec[11], fulltitle = F) +
  visualize_gam_map(model, year_vec[12], month_vec[12], fulltitle = F) +

  visualize_gam_map(model, year_vec[13], month_vec[13], fulltitle = F) +
  visualize_gam_map(model, year_vec[14], month_vec[14], fulltitle = F) +
  visualize_gam_map(model, year_vec[15], month_vec[15], fulltitle = F) +
  visualize_gam_map(model, year_vec[16], month_vec[16], fulltitle = F) +
  plot_layout(guides = 'collect', nrow = 3)

# ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/animation/05.png")),
# ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/animation/06.png")),
# ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/animation/07.png")),
# ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/animation/08.png")),
#
# ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/animation/09.png")),
# ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/animation/10.png")),
# ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/animation/11.png")),
# ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/animation/12.png")),


ggsave(here("analysis/09_spatiotemporal_models/temporal_panel_figure.png"), width = 12, height = 7, bg = 'white')


# ICEraceinc without spatiotemporal adjustment

model <- bam(
  formula = deaths ~ s(median_age) +
    te(ICEraceinc, date_int, d = c(1,1)),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = spatiotemporal_df
)

png(filename = here("analysis/09_spatiotemporal_models/ICEraceinc_and_time_no_spatiotemporal.png"), width = 1000, height = 1200, res = 150)
vis.gam(
  model,
  view = c('date_int', 'ICEraceinc'),
  cond = list(age = 38.8),
  n.grid = 50,
  theta = 35,
  phi = 32,
  zlab = "",
  contour.col = 'topo',
  type = 'response',
  ticktype = "detailed",
  color = "topo",
  main = "Interaction of ICEraceinc and Time Without\nAdjusting for Spatiotemporal Autocorrelation",
  ylab = '\n\nIndex of Concentration at the Extremes\nRacialized Economic Segregation',
  xlab = '\n\nDate in Years',
  nticks = 5
)
dev.off()


# now consider how ICEraceinc matters after taking into account spatiotemporal autocorrelation

model <- bam(
  formula = deaths ~ s(median_age) + te(latitude, longitude, date_int, d = c(2, 1)) +
    te(ICEraceinc, date_int, d = c(1,1)),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = spatiotemporal_df
)

png(filename = here("analysis/09_spatiotemporal_models/ICEraceinc_and_time.png"), width = 1000, height = 1200, res = 150)
vis.gam(
  model,
  view = c('date_int', 'ICEraceinc'),
  cond = list(age = 38.8),
  n.grid = 50,
  theta = 35,
  phi = 32,
  zlab = "",
  contour.col = 'topo',
  type = 'response',
  ticktype = "detailed",
  color = "topo",
  main = "Interaction of ICEraceinc and Time After\nAdjusting for Spatiotemporal Autocorrelation",
  ylab = '\n\nIndex of Concentration at the Extremes\nRacialized Economic Segregation',
  xlab = '\n\nDate in Years',
  nticks = 5
)
dev.off()

# median age without adjusting for spatiotemporal autocorrelation

model <- bam(
  formula = deaths ~ s(age) +
    te(median_age, date_int, d = c(1,1)),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = spatiotemporal_df
)

png(filename = here("analysis/09_spatiotemporal_models/median_age_and_time_no_spatiotemporal.png"), width = 1000, height = 1200, res = 150)
vis.gam(
  model,
  view = c('date_int', 'median_age'),
  n.grid = 50,
  theta = 35,
  phi = 32,
  zlab = "",
  contour.col = 'topo',
  type = 'response',
  ticktype = "detailed",
  color = "topo",
  main = "Interaction of Median Age and Time Without\nAdjusting for Spatiotemporal Autocorrelation",
  ylab = '\n\nMedian Age',
  xlab = '\n\nDate in Years',
  nticks = 4
)
dev.off()


# median age with adjusting for spatiotemporal autocorrelation

model <- bam(
  formula = deaths ~ s(age) + te(latitude, longitude, date_int, d = c(2, 1)) +
    te(median_age, date_int, d = c(1,1)),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = spatiotemporal_df
)

png(filename = here("analysis/09_spatiotemporal_models/median_age_and_time.png"), width = 1000, height = 1200, res = 150)
vis.gam(
  model,
  view = c('date_int', 'median_age'),
  n.grid = 50,
  theta = 35,
  phi = 32,
  zlab = "",
  contour.col = 'topo',
  type = 'response',
  ticktype = "detailed",
  color = "topo",
  main = "Interaction of Median Age and Time After\nAdjusting for Spatiotemporal Autocorrelation",
  ylab = '\n\nMedian Age',
  xlab = '\n\nDate in Years',
  nticks = 4
)
dev.off()

# now median income separately adjusting for spatiotemporal autocorrelation

model <- bam(
  formula = deaths ~ s(age) + te(latitude, longitude, date_int, d = c(2, 1)) +
    te(median_income, date_int, d = c(1,1)),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = spatiotemporal_df
)

png(filename = here("analysis/09_spatiotemporal_models/median_income_and_time.png"), width = 1000, height = 1200, res = 150)
vis.gam(
  model,
  view = c('date_int', 'median_income'),
  n.grid = 50,
  theta = 35,
  phi = 32,
  zlab = "",
  contour.col = 'topo',
  type = 'response',
  ticktype = "detailed",
  color = "topo",
  main = "Interaction of Median Income and Time After\nAdjusting for Spatiotemporal Autocorrelation",
  ylab = '\n\nMedian Income',
  xlab = '\n\nDate in Years',
  nticks = 4
)
dev.off()



# now population density separately adjusting for spatiotemporal autocorrelation

spatiotemporal_df %<>% mutate(log_pop_density = log10(pop_density))

model <- bam(
  formula = deaths ~ s(age) + te(latitude, longitude, date_int, d = c(2, 1)) +
    te(log_pop_density, date_int, d = c(1,1)),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = spatiotemporal_df
)

png(filename = here("analysis/09_spatiotemporal_models/log_pop_density_and_time.png"), width = 1000, height = 1200, res = 150)
vis.gam(
  model,
  view = c('date_int', 'log_pop_density'),
  n.grid = 50,
  theta = 35,
  phi = 32,
  zlab = "",
  contour.col = 'topo',
  type = 'response',
  ticktype = "detailed",
  color = "topo",
  main = "Interaction of Population Density and Time After\nAdjusting for Spatiotemporal Autocorrelation",
  ylab = '\n\nLog Population Density',
  xlab = '\n\nDate in Years',
  nticks = 4
)
dev.off()


# now political_lean without adjusting for spatiotemporal autocorrelation

model <- bam(
  formula = deaths ~ s(age) +
    te(political_lean, date_int, d = c(1,1)),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = spatiotemporal_df
)

png(filename = here("analysis/09_spatiotemporal_models/political_lean_and_time_no_spatiotemporal.png"), width = 1000, height = 1200, res = 150)
vis.gam(
  model,
  view = c('date_int', 'political_lean'),
  n.grid = 50,
  theta = 35,
  phi = 32,
  zlab = "",
  contour.col = 'topo',
  type = 'response',
  ticktype = "detailed",
  color = "topo",
  main = "Interaction of Political Lean and Time Without\nAdjusting for Spatiotemporal Autocorrelation",
  ylab = '\n\nPolitical Lean',
  xlab = '\n\nDate in Years',
  nticks = 4
)
dev.off()

# now political_lean and adjusting for spatiotemporal autocorrelation

model <- bam(
  formula = deaths ~ s(age) + te(latitude, longitude, date_int, d = c(2, 1)) +
    te(political_lean, date_int, d = c(1,1)),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = spatiotemporal_df
)

png(filename = here("analysis/09_spatiotemporal_models/political_lean_and_time.png"), width = 1000, height = 1200, res = 150)
vis.gam(
  model,
  view = c('date_int', 'political_lean'),
  n.grid = 50,
  theta = 35,
  phi = 32,
  zlab = "",
  contour.col = 'topo',
  type = 'response',
  ticktype = "detailed",
  color = "topo",
  main = "Interaction of Political Lean and Time After\nAdjusting for Spatiotemporal Autocorrelation",
  ylab = '\n\nPolitical Lean',
  xlab = '\n\nDate in Years',
  nticks = 4
)
dev.off()


# now poverty and adjusting for spatiotemporal autocorrelation

model <- bam(
  formula = deaths ~ # te(latitude, longitude, date_int, d = c(2, 1)) +
    te(prop_in_poverty, date_int, d = c(1,1)),
  offset = log(popsize/1e5/12),
  family = nb(),
  data = spatiotemporal_df
)

png(filename = here("analysis/09_spatiotemporal_models/poverty_and_time_no_spatiotemporal.png"), width = 1000, height = 1200, res = 150)
vis.gam(
  model,
  view = c('date_int', 'prop_in_poverty'),
  n.grid = 50,
  theta = 35,
  phi = 32,
  zlab = "",
  contour.col = 'topo',
  type = 'response',
  ticktype = "detailed",
  color = "topo",
  main = "Interaction of Povery and Time Without\nAdjusting for Spatiotemporal Autocorrelation",
  ylab = '\n\nProportion in Poverty',
  xlab = '\n\nDate in Years',
  nticks = 4
)
dev.off()



# make composite images ---------------------------------------------------

plot_grid(
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/log_pop_density_and_time_no_spatiotemporal.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/median_age_and_time_no_spatiotemporal.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/median_income_and_time_no_spatiotemporal.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/ICEraceinc_and_time_no_spatiotemporal.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/political_lean_and_time_no_spatiotemporal.png")),


  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/log_pop_density_and_time.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/median_age_and_time.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/median_income_and_time.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/ICEraceinc_and_time.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/political_lean_and_time.png")),

  labels = 'AUTO',
  nrow = 2
)

ggsave(here("analysis/09_spatiotemporal_models/panel_figure.png"), width = 10, height = 5, bg = 'white')


plot_grid(
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/median_age_and_time_no_spatiotemporal.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/median_age_and_time.png")),

  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/log_pop_density_and_time_no_spatiotemporal.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/log_pop_density_and_time.png")),

  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/median_income_and_time_no_spatiotemporal.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/median_income_and_time.png")),

  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/poverty_and_time_no_spatiotemporal.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/poverty_and_time.png")),

  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/ICEraceinc_and_time_no_spatiotemporal.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/ICEraceinc_and_time.png")),

  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/political_lean_and_time_no_spatiotemporal.png")),
  ggdraw() + draw_image(here("analysis/09_spatiotemporal_models/political_lean_and_time.png")),

  labels = 'AUTO',
  ncol = 4
)

ggsave(here("analysis/09_spatiotemporal_models/panel_figure.png"), height = 8, width = 8, bg = 'white', scale = .8)


