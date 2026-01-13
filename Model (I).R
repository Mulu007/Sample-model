library(tidyverse)
library(modelr)
library(purrr)
library(dplyr)
# a global function that deletes missing data but warns me before doing so
options(na.action = na.warn)

view(sim1)
print(n= 50, sim1)
# y = a_2*x + a_1
# runif(n, min, max)
# Generate n random numbers, each drawn uniformly between min and max
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) +
  geom_abline(
    aes(intercept = a1, slope = a2),
    data = models, alpha = 1/4
  ) +
  geom_point()

# A model predicts a y value
# The data has an actual y value
# The distance between y_actual and y_predicted is called error/residual
# Visually it is the vertical line between the point and the line
# We care how wrong the prediction is up and down
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
# Give me the predicted y-values from the model y = 7 + 1.5x
# for every x in sim1
# 7 + 1.5 * sim1$x
# Just an example
model1(c(7, 1.5), sim1)

# Root mean squared deviation
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

# There are many candidate models therefore for every model we need to compute how far it is from the data
# The function answers:
# If a model has intercept a1 and slope a2, how far is it from the data
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>%
  # map2 -> maps value in the function pairwise
  # mutate takes the model table and adss a new column called dist
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

# Each point in the graph represents possible models in a parameter space
# Highlighting in red shows where the best models cluster
ggplot(models, aes(a1, a2)) +
  geom_point(
    data = filter(models, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(colour = -dist))


grid <- expand.grid(
  # seq() creates 25 evenly spaced numbers
  # 25*25 = 625 models
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>%
  # For every (a1, a2) pair in the grid, compute how far that model is from the data.
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>%
  ggplot(aes(a1, a2)) +
  # Finds the 10 best models with the smallest distance and marks them red
  geom_point(
    data = filter(grid, rank(dist) <= 10),
    size = 4, colour = "red"
  ) +
  geom_point(aes(color = -dist))

# Overlaying the 10 best models on the data
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  )

# Numerical minimisation through Newton-Raphson Search
# Minimising distance gives the best fit line by minimising the loss function
# A loss function measures how bas a model is
# ie: Good line -> small residuals -> small distance
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par
#> [1] 4.22 2.05

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])

# PREDICTIONS
# Predictions are derived from models
# Models are abstract and predictions compare it to reality

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

grid <- sim1 %>%
  data_grid(x)
grid

grid <- grid %>%
  add_predictions(sim1_mod)
grid

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(
    aes(y = pred),
    data = grid,
    colour = "red",
    size = 1
  )

# Adding residuals
sim1 <- sim1 %>%
  add_residuals(sim1_mod)
sim1

# Frequency polygon is one of the way to understand the spread of residuals
ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

# Learning loess
# Loess does not assume linearity unlike lm()
sim1_loess <- loess(y ~ x, data = sim1)

grid <- sim1 %>%
  data_grid(x)
grid

grid <- grid %>%
  add_predictions(sim1_loess)
grid

ggplot(sim1, aes(x, y)) +
  geom_point(color = "grey30") +
  #geom_line(data = grid, aes(y = pred), color = "blue", linewidth = 1)
  geom_smooth(method = "loess", se = FALSE)
