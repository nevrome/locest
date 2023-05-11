setwd("agora/locest/playground/")

library(magrittr)
library(ggplot2)

system("stack install")
system("locest search --obsFile playground/test2Obs.tsv --searchPosFile playground/test2Grid.tsv --outFile troot.tsv")

hu <- readr::read_tsv("../troot.tsv",col_names = c("x", "y", "t", "prob"))

hu %>%
  ggplot() +
  geom_raster(aes(x, y, fill = prob)) +
  scale_fill_viridis_c()



s <- 200
t <- 200
decay_factor <- 0.0003
sd <- decay_factor * s + decay_factor * t

x <- seq(-0.1, 0.1, 0.00001)
tibble::tibble(x = x, y = dnorm(x,0.0461299,sd)) %>%
  ggplot() + geom_line(aes(x, y)) +
  coord_cartesian(xlim = c(-0.1, 0.1), ylim = c(0,10))

integrate(dnorm, mean=0, sd=1, lower= -Inf, upper= Inf, abs.tol = 0)$value
