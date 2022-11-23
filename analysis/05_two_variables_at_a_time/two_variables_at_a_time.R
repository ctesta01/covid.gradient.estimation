library(here)
devtools::load_all(".")
library(mgcv)

# load analytic dataset
df <- create_analytic_dataset()

# create a numeric version of date
df$date_int <- as.integer(lubridate::year(df$date)) + (as.integer(lubridate::month(df$date))-1)/12


# ICEraceinc and Median Age -----------------------------------------------

# construct model
model <- bam(
  formula = deaths ~ te(date_int, ICEraceinc, median_age, d = c(1,1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)

# function to select frames and animate them
plot_two_way_interaction_single_frames <- function(
    date = 2020.25,
    view = c('ICEraceinc', 'median_age'),
    xlab = '\n\nIndex of Concentration at the Extremes\nRacialized Economic Segregation',
    ylab = '\n\nMedian Age',
    main = "March 2020"
) {
  vis.gam(
    model,
    view = view,
    cond = list(date_int = date),
    n.grid = 50,
    theta = 35,
    phi = 32,
    zlab = "",
    contour.col = 'topo',
    type = 'response',
    ticktype = "detailed",
    color = "topo",
    main = main,
    xlab = xlab,
    ylab = ylab,
    nticks = 4
  )
}

# create a set of monthly date-times for animation and corresponding "Month YYYY" titles
animation_frames <- c(2020 + 0:11/12, 2021 + 0:11/12, 2022 + 0:8/12)[3:32]
titles <- apply(expand.grid(month.name, 2020:2022), 1, function(x) paste0(x[1], " ", x[2]))[3:32]

# render each animation frame
for (i in seq_along(animation_frames)) {
  # open png device/file
  png(filename = paste0(here("analysis/05_two_variables_at_a_time/animation_ICEraceinc_age/"), stringr::str_pad(i, width = 2, side = 'left', pad = '0'), ".png"),
      width = 6, height = 6, units = "in", res = 300)

  # create visualization
  plot_two_way_interaction_single_frames(
    date = animation_frames[i],
    main = titles[i]
  )
  # close file/device
  dev.off()
}

# move into the directory where the frames were animated
setwd(here("analysis/05_two_variables_at_a_time/animation_ICEraceinc_age/"))

# use imagemagick to create a gif
system("convert -delay 40 -loop 0 *.png ICEraceinc_age_animation.gif \( +clone -set delay 500 \) +swap +delete animation_with_pause.gif")



# ICEraceinc and Median Income -----------------------------------------------

# construct model
model <- bam(
  formula = deaths ~ te(date_int, ICEraceinc, prop_75_and_up, d = c(1,1,1)),
  offset = log(popsize/1e5/12),
  data = df,
  family = nb()
)


# render each animation frame
for (i in seq_along(animation_frames)) {
  # open png device/file
  png(filename = paste0(here("analysis/05_two_variables_at_a_time/animation_ICEraceinc_75_and_up/"), stringr::str_pad(i, width = 2, side = 'left', pad = '0'), ".png"),
      width = 6, height = 6, units = "in", res = 300)

  # create visualization
  plot_two_way_interaction_single_frames(
    date = animation_frames[i],
    main = titles[i],
    view = c('ICEraceinc', 'prop_75_and_up'),
    ylab = '\n\nProportion Age 75+'
  )
  # close file/device
  dev.off()
}

# move into the directory where the frames were animated
setwd(here("analysis/05_two_variables_at_a_time/animation_income_age/"))

# use imagemagick to create a gif
system("convert -delay 40 -loop 0 *.png income_age_animation.gif \( +clone -set delay 500 \) +swap +delete animation_with_pause.gif")


# Panel figure
plot_grid(
  ggdraw() + draw_image(here("analysis/05_two_variables_at_a_time/animation_ICEraceinc_age/03.png")),
  ggdraw() + draw_image(here("analysis/05_two_variables_at_a_time/animation_ICEraceinc_age/09.png")),
  ggdraw() + draw_image(here("analysis/05_two_variables_at_a_time/animation_ICEraceinc_age/15.png")),
  ggdraw() + draw_image(here("analysis/05_two_variables_at_a_time/animation_ICEraceinc_age/20.png")),
  ggdraw() + draw_image(here("analysis/05_two_variables_at_a_time/animation_ICEraceinc_age/24.png")),
  ggdraw() + draw_image(here("analysis/05_two_variables_at_a_time/animation_ICEraceinc_age/30.png")),

  labels = 'AUTO',
  nrow = 2
)

ggsave(here("analysis/05_two_variables_at_a_time/animation_ICEraceinc_age/panel_figure.png"), width = 8, height = 5.5, bg='white')

