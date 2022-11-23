# supplementary gam figures
library(ggplot2)
library(tibble)
library(magrittr)
library(dplyr)
library(splines)
library(here)

df <- tibble::tibble(x = seq(0,10,length.out=100) + rnorm(n = 100, sd = .25),
                 y = (x-6)^3/100 + x^2/100 + rnorm(n=100, sd = .1) + 5)


set.seed(2022)
n <- 100
df <- tibble::tibble(x = seq(0,10,length.out=n),
                 y = (x-6)^3/100 + rnorm(n=n, sd = .1) + 2.5)

ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  theme_bw() +
  ggtitle("A. Simulated Sample Observations") ->
  scatterplot

basis <- bs(seq(0,10, length.out=1000), df = 15)

basis_tidy <- basis %>%
  as.data.frame() %>%
  mutate(x = seq(1,10,length.out=1000)) %>%
  tidyr::pivot_longer(
    cols = setdiff(colnames(.), "x"),
    names_to = "basis",
    values_to = 'y'
  )

basis_tidy %>% # filter(basis != '15') %>%
  ggplot(aes(x = x, y = y, color = basis)) +
  geom_line() +
  theme_bw()+
  scale_color_viridis_d() +
  theme(legend.position = 'none') +
  ggtitle("B. B-Spline Basis Functions") ->
  basis_plot

model <- lm(y ~ bs(x, df = 15), data = df)

basis_fit <- t( coef(model)[2:16] * t(basis[,c(1:15)]) ) %>%
  rowSums(na.rm = T) + coef(model)[1]

basis_expansion <- t( coef(model)[2:16] * t(basis[,c(1:15)]) ) %>%
  as.data.frame() %>%
  mutate(x = seq(0,10,length.out=1000)) %>%
  tidyr::pivot_longer(
    cols = setdiff(colnames(.), "x"),
    names_to = "basis",
    values_to = 'y'
  )

ggplot(df, aes(x = x, y = y)) +
  geom_line(data = data.frame(x = seq(0, 10, length.out = 1000), y = basis_fit), size = 1.5, alpha = 0.8, color = 'blue') +
  geom_point(alpha = 0.8) +
  geom_line(data = basis_expansion, mapping = aes(color = basis)) +
  theme_bw() +
  scale_color_viridis_d() +
  theme(legend.position = 'none') +
  ggtitle("C. Estimated Fit Using B-Spline Basis Functions") ->

  fit_plot

library(patchwork)

(scatterplot / basis_plot) | fit_plot

ggsave("analysis/10_supplementary_gam_figures/gam_demo_figure.png", width=12, height = 4.5)
