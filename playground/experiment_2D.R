library(ggplot2)
library(magrittr)

n = 1000
total_density = 10+0.3+7

observations <- tibble::tibble(
  obsID = 1:n,
  indepV1 = c(
    runif(round((10 /total_density) * n), 0,  10),
    runif(round((0.3/total_density) * n), 10, 15),
    runif(round((7  /total_density) * n), 15, 20)
  )
) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    depV1 = dplyr::case_when(
      indepV1 <= 2               ~ rnorm(1,  4, 1),
      indepV1 > 2 & indepV1 <= 4 ~ rnorm(1, 10, 1),
      indepV1 > 4 & indepV1 <= 6 ~ rnorm(1, 10, 4),
      indepV1 > 6 & indepV1 <= 8 ~ rnorm(1, 10, 1),
      indepV1 > 8                ~ rnorm(1, 6,  1),
    )
  )

observations %>%
  ggplot() +
  geom_point(aes(indepV1, depV1))

prediction_points <- tibble::tibble(
  spatID  = 1:1000,
  indepV1 = seq(0, 20, length.out = 1000)
)

readr::write_tsv(observations, "data/experiments/2D/obs.tsv")
readr::write_tsv(prediction_points, "data/experiments/2D/grid.tsv")

system('locest search -i data/experiments/2D/obs.tsv --anyGridFile data/experiments/2D/grid.tsv -a "kas(c(depV1 = depVar(5, c(indepV1 = 0.5))))" -o data/experiments/2D/interpol.tsv')

res <- readr::read_tsv(file = "data/experiments/2D/interpol.tsv")

ggplot() +
  geom_point(
    data = observations,
    mapping = aes(indepV1, depV1)
  ) +
  geom_ribbon(
    data = res,
    mapping = aes(x = indepV1, ymin = depV1Low, ymax = depV1Up),
    color = "red",
    fill = "red",
    alpha = 0.2
  ) +
  coord_cartesian(ylim = c(0,20))

ggplot() +
  geom_line(
    data = res,
    mapping = aes(x = indepV1, y = depV1EffN),
    color = "red"
  )

ggplot() +
  geom_line(
    data = res,
    mapping = aes(x = indepV1, y = depV1Var),
    color = "red"
  )
