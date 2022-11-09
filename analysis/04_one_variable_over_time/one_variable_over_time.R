library(here)
library(mgcv)
library(mgcViz)
devtools::load_all(".")

# load our analytic dataset
df <- create_analytic_dataset()

# create a numeric version of date
df$date_int <- as.integer(lubridate::year(df$date)) + (as.integer(lubridate::month(df$date))-1)/12

# poverty
model <- gam(
  formula = deaths ~ te(date_int, prop_in_poverty, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)

png(
  filename = here("analysis/04_one_variable_over_time/poverty.png")
)
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Poverty and Time",
              xlab = "\n\nDate", ylab = '\n\nProportion in Poverty', nticks = 4)
dev.off()


# median income
model <- gam(
  formula = deaths ~ te(date_int, median_income, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/income_and_time.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Median Income and Time",
              xlab = "\n\nDate", ylab = '\n\nMedian Income', nticks = 4)
dev.off()

# percent black
model <- gam(
  formula = deaths ~ te(date_int, prop_black, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)

png(filename = here("analysis/04_one_variable_over_time/proportion_black_and_time.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Proportion Black and Time",
              xlab = "\n\nDate", ylab = '\n\nProportion Black', nticks = 4)
dev.off()

# percent crowding
model <- gam(
  formula = deaths ~ te(date_int, crowding, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/household_crowding_and_time.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Proportion of Households\nExperiencing Crowding and Time",
              xlab = "\n\nDate", ylab = '\n\nHousehold Crowding', nticks = 4)
dev.off()


# population density
model <- gam(
  formula = deaths ~ te(date_int, log10(pop_density), d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/pop_density_and_time.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Log Population Density and Time",
              xlab = "\n\nDate", ylab = '\n\nLog Population Density', nticks = 4)
dev.off()

# ICEraceinc
model <- gam(
  formula = deaths ~ te(date_int, ICEraceinc, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/ICEraceinc_and_time.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
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


# proportion over 65
model <- gam(
  formula = deaths ~ te(date_int, prop_65_and_up, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/65_plus_and_time.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Proportion over 65 and Time",
              xlab = "\n\nDate", ylab = '\n\nProportion over 65', nticks = 4)
dev.off()

# proportion over 75
model <- gam(
  formula = deaths ~ te(date_int, prop_75_and_up, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/75_plus_and_time.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Proportion over 75 and Time",
              xlab = "\n\nDate", ylab = '\n\nProportion over 75', nticks = 4)
dev.off()


# proportion with no HS degree
model <- gam(
  formula = deaths ~ te(date_int, prop_no_hs, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/prop_no_hs.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Proportion with no HS Degree and Time",
              xlab = "\n\nDate", ylab = '\n\nProportion with no HS Degree', nticks = 4)
dev.off()

# proportion with college degree
model <- gam(
  formula = deaths ~ te(date_int, prop_college, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/prop_college.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Proportion with College Degree and Time",
              xlab = "\n\nDate", ylab = '\n\nProportion with no College', nticks = 4)
dev.off()

# political_lean
model <- gam(
  formula = deaths ~ te(date_int, political_lean, d = c(1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)
png(filename = here("analysis/04_one_variable_over_time/political_lean.png"))
mgcv::vis.gam(model, n.grid = 50, theta = 35, phi = 32, zlab = "", contour.col = 'topo', type = 'response',
              ticktype = "detailed", color = "topo", main = "Interaction by Political Lean and Time",
              xlab = "\n\nDate", ylab = '\n\nPolitical Lean', nticks = 4)
dev.off()

