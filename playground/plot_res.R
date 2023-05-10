setwd("agora/locest/playground/")

library(magrittr)
library(ggplot2)

hu <- readr::read_tsv("../troot.tsv",col_names = c("x", "y", "t", "prob"))

hu %>%
  ggplot() +
  geom_raster(aes(x, y, fill = prob))



s <- 200
t <- 200
factor <- 0.0001
sd <- factor * s + factor * t

x <- seq(-0.1, 0.1, 0.00001)
tibble::tibble(x = x, y = dnorm(x,0.0461299,sd)) %>%
  ggplot() + geom_line(aes(x, y)) +
  coord_cartesian(xlim = c(-0.1, 0.1), ylim = c(0,10))

dnorm(0,0,0)

