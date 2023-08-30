library(magrittr)
library(ggplot2)

obs <- readr::read_tsv("test2Obs.tsv")

# normal search test
system('time locest search --configFile "normalSearch.conf"')

hu5 <- readr::read_tsv("test_res/test2Search.tsv")

hu5 %>%
  ggplot() +
  facet_wrap(~age) +
  geom_raster(aes(x, y, fill = probability)) +
  scale_fill_viridis_c() +
  coord_fixed()

#plot(-1000:1000, dnorm(-1000:1000,0,400), ylim = c(0,0.01))
#mvtnorm::dmvnorm(c(700,700), c(0,0), diag(c(500^2,500^2)))

hu5 %>%
  ggplot() +
  facet_wrap(~age) +
  geom_raster(aes(x, y, fill = varC1Res)) +
  scale_fill_viridis_c() +
  coord_fixed()

hu5 %>%
  ggplot() +
  facet_wrap(~age) +
  geom_raster(aes(x, y, fill = probability)) +
  geom_point(
    data = hu5 %>% dplyr::filter((varC1Dens+varC2Dens)/2 < 0.000001),
    aes(x,y),
    shape = 4, color = "red"
  ) +
  scale_fill_viridis_c() +
  coord_fixed()

ggplot() +
  facet_wrap(~age) +
  geom_raster(
    data = hu5 %>%
      dplyr::filter(varC1ResErr != "Infinity" & varC1ResErr != "NaN") %>%
      dplyr::filter(age == -5000) %>%
      dplyr::mutate(varC1ResErr = log10(as.numeric(varC1ResErr))),
    aes(x,y, fill = varC1ResErr)
  ) +
  geom_point(
    data = obs %>%
      dplyr::filter(age > -7500 & age < -4500) %>%
      dplyr::mutate(age = round(age, -3)) %>%
      dplyr::filter(age == -5000),
    aes(x,y),
    shape = 4, color = "red"
  ) +
  scale_fill_viridis_c() +
  coord_fixed()

# one position test
system('time locest search -i test2Obs.tsv -g test2GridOnePoint.tsv -t "c(-5750, -5500,-5250, -5000, -4750)" -d "c(varC1=-0.0885337:0.0570383:0.01,varC2=-0.0669435:0.1100580:0.01)" -a "SepIDW(c(varC1 = LinearSum(0.00001, 0.00001), varC2 = LinearSum(0.00001, 0.00001)), DistanceWeightedMean)" -o test_res/test2Interpolate.tsv')

hu <- readr::read_tsv("test_res/test2Interpolate.tsv")

hu %>%
  ggplot() +
  facet_wrap(~age) +
  geom_raster(aes(varC1, varC2, fill = probability)) +
  scale_fill_viridis_c() +
  coord_fixed()


# crossvalidation position test
system('time locest crossvalidate -i test2Obs.tsv --testFraction 0.1 --iterations 5 -o test_res/test2Crossvalidate.tsv')

# test with own distance matrix

system('time locest search -i distMatrixObs.tsv -g distMatrixGrid.tsv --spatDistFile distMatrixDists.tsv -t "c(0)" -d "c(varC1 = 0,varC2 = 0)" -a "SepIDW(c(varC1 = LinearSum(0.00001, 0.00001), varC2 = LinearSum(0.00001, 0.00001)), DistanceWeightedMean)" -o test_res/distMatrixTestSearch.tsv')

hu <- readr::read_tsv("test_res/distMatrixTestSearch.tsv")

hu %>%
  ggplot() +
  geom_raster(aes(x, y, fill = probability)) +
  scale_fill_viridis_c() +
  coord_fixed()
