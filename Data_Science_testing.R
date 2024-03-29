library(tidyverse)

library(modelr)
options(na.action = na.warn)

ggplot(sim1, aes(x, y)) +
   geom_point()+
   geom_abline(alpha=1/4)

models <- tibble(
    a1 = runif(250, -20, 40),
    a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) +
  geom_abline(
      aes(intercept = a1, slope  = a2),
      data = models, aplpha = 1/4
  ) +
  geom_point()


model1 <- function(a, data) {
    a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
   diff <- data$y - model1(mod, data)
   sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
   measure_distance(c(a1,a2), sim1)
}

models <- models %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

models

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope=a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )


ggplot(models, aes(a1, a2)) +
  geom_point(
    data = filter(models, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(colour = -dist))
  
grid <- expand.grid(
   a1 = seq(-5, 20, length = 25),
   a2 = seq(1, 3, length = 25)
 ) %>%
 mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist)) 

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  ) 
  
ggplot(models, aes(a1, a2)) +
  geom_point(
    data = filter(models, rank(dist) <= 10),
    size =4, color = "red"
  ) +
  geom_point(aes(colour = -dist))

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, )


