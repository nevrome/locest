library(ggplot2)
library(magrittr)

obs_linear <- tibble::tibble(
  obsID = 1:n,
  indepV1 = 1:1000,
  depV1 = indepV1
)

obs_linear %>%
  ggplot() +
  geom_point(aes(indepV1, depV1))

readr::write_tsv(obs_linear, "data/variograms/obs_linear.tsv")

system('locest vario -i data/variograms/obs_linear.tsv --variogramOutFile data/variograms/vario_linear.tsv')

vario_linear <- readr::read_tsv("data/variograms/vario_linear.tsv")

vario_linear %>%
  ggplot() +
  geom_point(aes(bin, semivariance))

###

obs_constant <- tibble::tibble(
  obsID = 1:n,
  indepV1 = 1:1000,
  depV1 = 5
)

obs_constant %>%
  ggplot() +
  geom_point(aes(indepV1, depV1))

readr::write_tsv(obs_constant, "data/variograms/obs_constant.tsv")

system('locest vario -i data/variograms/obs_constant.tsv --variogramOutFile data/variograms/vario_constant.tsv')

vario_constant <- readr::read_tsv("data/variograms/vario_constant.tsv")

vario_constant %>%
  ggplot() +
  geom_point(aes(bin, semivariance))

###

obs_sin <- tibble::tibble(
  obsID = 1:n,
  indepV1 = 1:1000,
  depV1 = sin(indepV1/100) + 5
)

obs_sin %>%
  ggplot() +
  geom_point(aes(indepV1, depV1))

readr::write_tsv(obs_sin, "data/variograms/obs_sin.tsv")

system('locest vario -i data/variograms/obs_sin.tsv --variogramOutFile data/variograms/vario_sin.tsv')

vario_sin <- readr::read_tsv("data/variograms/vario_sin.tsv")

vario_sin %>%
  ggplot() +
  geom_point(aes(bin, semivariance))

