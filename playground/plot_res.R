library(magrittr)
library(ggplot2)

system('locest search -i test2Obs.tsv -g test2Grid.tsv -t -7000 -d "varC1:0.0461299,varC2:0.00014293" -o test2Search.tsv')

hu <- readr::read_tsv("test2Search.tsv",col_names = c("x", "y", "t", "C1", "C2", "prob"))

hu %>%
  ggplot() +
  geom_raster(aes(x, y, fill = prob)) +
  scale_fill_viridis_c() +
  coord_fixed()

system('locest interpolate -i test2Obs.tsv -g test2Grid.tsv -t -7000 -d "varC1:0.0461299,varC2:0.00014293" -o test2Interpolate.tsv')

hu <- readr::read_tsv("test2Interpolate.tsv",col_names = c("x", "y", "t", "C1", "C2", "prob"))


s <- 200
t <- 200
decay_factor <- 0.0003
sd <- decay_factor * s + decay_factor * t

x <- seq(-0.1, 0.1, 0.00001)
tibble::tibble(x = x, y = dnorm(x,0.0461299,sd)) %>%
  ggplot() + geom_line(aes(x, y)) +
  coord_cartesian(xlim = c(-0.1, 0.1), ylim = c(0,10))

integrate(dnorm, mean=0, sd=1, lower= -Inf, upper= Inf, abs.tol = 0)$value
