library(magrittr)
library(ggplot2)

# normal search test
system('time locest search --configFile "normalSearch.conf"')

hu <- readr::read_tsv("test_res/test2Search.tsv")

hu %>%
  ggplot() +
  facet_wrap(~age) +
  geom_raster(aes(x, y, fill = probability)) +
  scale_fill_viridis_c() +
  coord_fixed()

# one position test
system('time locest search -i test2Obs.tsv -g test2GridOnePoint.tsv -t "-5750, -5500,-5250, -5000, -4750" -d "varC1=-0.0885337:0.0570383:0.01+varC2=-0.0669435:0.1100580:0.01" -o test_res/test2Interpolate.tsv')

hu <- readr::read_tsv("test_res/test2Interpolate.tsv")

hu %>%
  ggplot() +
  facet_wrap(~age) +
  geom_raster(aes(varC1, varC2, fill = probability)) +
  scale_fill_viridis_c() +
  coord_fixed()


# crossvalidation position test
system('time locest crossvalidate -i test2Obs.tsv --testFraction 0.1 --iterations 5 -o test_res/test2Crossvalidate.tsv')

# 
