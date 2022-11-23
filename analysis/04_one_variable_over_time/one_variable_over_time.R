library(here)
library(mgcv)
library(mgcViz)
library(patchwork)
library(cowplot)
devtools::load_all(".")

# load our analytic dataset
df <- create_analytic_dataset()

# create a numeric version of date
df$date_int <- as.integer(lubridate::year(df$date)) + (as.integer(lubridate::month(df$date))-1)/12

# median income
model <- gam(
  formula = deaths ~ s(median_age) + te(date_int, median_income, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/income_and_time.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              cond = list(median_age = 38.8),
              view = c('date_int', 'median_income'),
              ticktype = "detailed", color = "topo", main = "Interaction by Median Income and Time",
              xlab = "\n\nDate", ylab = '\n\nMedian Income', nticks = 4)
dev.off()


# poverty
model <- gam(
  formula = deaths ~ s(median_age) + te(date_int, prop_in_poverty, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)

png(
  filename = here("analysis/04_one_variable_over_time/poverty.png")
)
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              cond = list(median_age = 38.8),
              view = c('date_int', 'prop_in_poverty'),
              ticktype = "detailed", color = "topo", main = "Interaction by Poverty and Time",
              xlab = "\n\nDate", ylab = '\n\nProportion in Poverty', nticks = 4)
dev.off()

# population density
df %<>% mutate(log_pop_density = log10(pop_density))
model <- gam(
  formula = deaths ~ s(median_age) + te(date_int, log_pop_density, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/pop_density_and_time.png"))
mgcv::vis.gam(model,
              cond = list(median_age = 38.8),
              view = c('date_int', 'log_pop_density'),
              n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Log Population Density and Time",
              xlab = "\n\nDate", ylab = '\n\nLog Population Density', nticks = 4)
dev.off()

# ICEraceinc
model <- gam(
  formula = deaths ~ s(median_age) + te(date_int, ICEraceinc, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/ICEraceinc_and_time.png"))
mgcv::vis.gam(model,
              cond = list(median_age = 38.8),
              view = c('date_int', 'ICEraceinc'),
              n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by ICEraceinc and Time",
              xlab = "\n\nDate", ylab = '\n\nICEraceinc')
dev.off()

# median age
model <- gam(
  formula = deaths ~ te(date_int, median_age, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/age_and_time.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Median Age and Time",
              xlab = "\n\nDate", ylab = '\n\nMedian Age', nticks = 4)
dev.off()


# political_lean
model <- gam(
  formula = deaths ~ s(median_age) + te(date_int, political_lean, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/political_lean.png"))
mgcv::vis.gam(model,
              cond = list(median_age = 38.8),
              view = c('date_int', 'political_lean'),
              n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Political Lean and Time",
              xlab = "\n\nDate", ylab = '\n\nPolitical Lean', nticks = 4)
dev.off()


plot_grid(
  ggdraw() + draw_image(here("analysis/04_one_variable_over_time/age_and_time.png")),
  ggdraw() + draw_image(here("analysis/04_one_variable_over_time/pop_density_and_time.png")),
  ggdraw() + draw_image(here("analysis/04_one_variable_over_time/income_and_time.png")),
  ggdraw() + draw_image(here("analysis/04_one_variable_over_time/poverty.png")),
  ggdraw() + draw_image(here("analysis/04_one_variable_over_time/ICEraceinc_and_time.png")),
  ggdraw() + draw_image(here("analysis/04_one_variable_over_time/political_lean.png")),

  labels = 'AUTO',
  nrow = 3
)

ggsave(here("analysis/04_one_variable_over_time/panel_figure.png"), height = 7, width = 5, bg='white')



gam_simple <- gam(formula = deaths ~ s(median_age),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb()
)


gam_age <- gam(formula = deaths ~ te(median_age, date_int, d=c(1,1)),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb()
)

gam_simple_for_pop_density <- gam(formula = deaths ~ s(median_age) + s(log_pop_density),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb()
)


gam_log_pop_density <- gam(formula = deaths ~ s(median_age) + te(log_pop_density, date_int, d=c(1,1)),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb()
)

gam_simple_for_income <-
  df %>% filter(! is.na(median_income) & ! is.na(median_age) & ! is.na(date_int) & !is.na(date_int) & !is.na(log(popsize))) %$%
  gam(formula = deaths ~ s(median_age) + s(median_income),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb())


gam_income <- gam(formula = deaths ~ s(median_age) + te(median_income, date_int, d=c(1,1)),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb(),
              na.action = 'exclude'
)

gam_simple_for_poverty <- gam(formula = deaths ~ s(median_age) + s(prop_in_poverty),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb()
)

gam_poverty <- gam(formula = deaths ~ s(median_age) + te(prop_in_poverty, date_int, d=c(1,1)),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb()
)

gam_simple_for_ICEraceinc <- gam(formula = deaths ~ s(median_age) + s(ICEraceinc),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb()
)

gam_ICEraceinc <- gam(formula = deaths ~ s(median_age) + te(ICEraceinc, date_int, d=c(1,1)),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb()
)


gam_simple_for_political_lean <- gam(formula = deaths ~ s(median_age) + s(political_lean),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb()
)
gam_political_lean <- gam(formula = deaths ~ s(median_age) + te(political_lean, date_int, d=c(1,1)),
              offset = log(popsize/1e5/12),
              data = df,
              family = nb()
)

anova.gam(gam_simple, gam_age, test='F')
anova.gam(gam_simple_for_pop_density, gam_log_pop_density, test='F')
anova.gam(gam_simple_for_income, gam_income, test='F')
anova.gam(gam_simple_for_poverty, gam_poverty, test='F')
anova.gam(gam_simple_for_ICEraceinc, gam_ICEraceinc, test='F')
anova.gam(gam_simple_for_political_lean, gam_political_lean, test='F')

AIC(
  gam_simple,
  gam_age,
  gam_simple_for_pop_density,
  gam_log_pop_density,
  gam_simple_for_income,
  gam_income,
  gam_simple_for_poverty,
  gam_poverty,
  gam_simple_for_ICEraceinc,
  gam_ICEraceinc,
  gam_simple_for_political_lean,
  gam_political_lean
)
