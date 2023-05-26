library(magrittr)
library(ggplot2)

system('locest search -i testObs.tsv -g testObs.tsv -t -400 -d "varC1=150+varC2=150" -o testSearch.tsv')

system('locest search -i test2Obs.tsv -g test2Grid.tsv -t "-7000,-6500,-6000,-5500,-5200" -d "varC1=0.0461299+varC2=0.00014293" -o test2Search.tsv')

hu <- readr::read_tsv("test2Search.tsv")

hu %>%
  ggplot() +
  facet_wrap(~age) +
  geom_raster(aes(x, y, fill = log(probability))) +
  scale_fill_viridis_c() +
  coord_fixed()

# 5242, so the exact year for Stuttgart, does not work. Probably because of an infinite density
system('locest search -i test2Obs.tsv -g test2GridOnePoint.tsv -t "-5750, -5500,-5250, -5000, -4750" -d "varC1=-0.0885337:0.0570383:0.01+varC2=-0.0669435:0.1100580:0.01" -o test2Interpolate.tsv')

hu <- readr::read_tsv("test2Interpolate.tsv")

hu %>%
  ggplot() +
  facet_wrap(~age) +
  geom_raster(aes(varC1, varC2, fill = log10(probability))) +
  scale_fill_viridis_c() +
  coord_fixed()

system('locest search -i test2Obs.tsv -g test2GridOnePoint.tsv -t -5200 -d "varC1=0.0461299+varC2=0.00014293,varC1=0.0461299+varC2=0.00014293" -o test2Interpolate.tsv')

s <- 200
t <- 200
decay_factor <- 0.0003
sd <- decay_factor * s + decay_factor * t

x <- seq(-0.1, 0.1, 0.00001)
tibble::tibble(x = x, y = dnorm(x,0.0461299,sd)) %>%
  ggplot() + geom_line(aes(x, y)) +
  coord_cartesian(xlim = c(-0.1, 0.1), ylim = c(0,10))

integrate(dnorm, mean=0, sd=1, lower= -Inf, upper= Inf, abs.tol = 0)$value
