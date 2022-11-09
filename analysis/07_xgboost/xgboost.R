
library(here)
library(xgboost)
library(vip)
devtools::load_all(".")

# create analytic dataset
df <- create_analytic_dataset()

# create a numeric version of date
df$date_int <- as.integer(lubridate::year(df$date)) + (as.integer(lubridate::month(df$date))-1)/12

# fit model
df_mat <-
  df %>%
  na.omit() %>%
  sf::st_drop_geometry() %>%
  ungroup() %>%
  select(
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
    pop_density,
    date_int
  ) %>%
  as.matrix()


model <- xgboost(
    # formula = crude_rate_per_100k_py ~ ICEraceinc + prop_in_poverty + median_income + median_age + prop_65_and_up +
    #   prop_75_and_up + crowding + prop_black + prop_hispanic + prop_white_nh + prop_asian + prop_aian + prop_nhopi +
    #   pop_density + date_int,
    label = na.omit(df)$crude_rate_per_100k_py,
    nrounds = 1000,
    data = df_mat)

# variable importance plot
vip::vip(model) +
  ggtitle("Variable Importance from XGBoost")
ggsave(here("analysis/07_xgboost/xgboost_variable_importance.png"))

