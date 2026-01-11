library(tidyverse)
library(modelr)
# a global function that deletes missing data but warns me before doing so
options(na.action = na.warn)

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
model1(c(7, 1.5), sim1)