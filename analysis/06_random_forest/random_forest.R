library(here)
library(ranger)
library(vip)
devtools::load_all(".")

# create analytic dataset
df <- create_analytic_dataset()

# create a numeric version of date
df$date_int <- as.integer(lubridate::year(df$date)) + (as.integer(lubridate::month(df$date))-1)/12

# fit model
model <-
  df %>%
    na.omit() %$%
    ranger(
    formula = crude_rate_per_100k_py ~ ICEraceinc + prop_in_poverty + median_income + median_age + prop_65_and_up +
      prop_75_and_up + crowding + prop_black + prop_hispanic + prop_white_nh + prop_asian + prop_aian + prop_nhopi +
      pop_density + date_int,
    data = .,
    case.weights = .$popsize,
    importance = "impurity_corrected"
)

# variable importance plot
vip::vip(model) +
  ggtitle("Variable Importance from Random Forest Using Impurity-Corrected Method")
ggsave(here("analysis/06_random_forest/impurity_corrected_importance.png"))


# and if we add in divisions
model <-
  df %>%
  na.omit() %$%
  ranger(
    formula = crude_rate_per_100k_py ~ division + ICEraceinc + prop_in_poverty + median_income + median_age + prop_65_and_up +
      prop_75_and_up + crowding + prop_black + prop_hispanic + prop_white_nh + prop_asian + prop_aian + prop_nhopi +
      pop_density + date_int,
    data = .,
    case.weights = .$popsize,
    importance = "impurity_corrected"
  )

# variable importance plot
vip::vip(model) +
  ggtitle("Variable Importance from Random Forest Using impurity Method")


