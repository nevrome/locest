library(magrittr)
library(ggplot2)

# interface test
system('locest search -i testObs.tsv -g testObs.tsv -t -400 -d "varC1=150+varC2=150" -o testSearch.tsv')

# normal search test
system('time locest search -i test2Obs.tsv -g test2Grid.tsv -t "-7000,-6000,-5000" -d "varC1=0.0461299+varC2=0.00014293" -o test2Search.tsv')

system('time locest --configFile "normalSearchTestConfig.txt"')

system('time locest search --configFile "shortSearchTestConfig.txt" --outFile test2Search.tsv')

hu <- readr::read_tsv("test2Search.tsv")

hu %>%
  ggplot() +
  facet_wrap(~age) +
  geom_raster(aes(x, y, fill = probability)) +
  scale_fill_viridis_c() +
  coord_fixed()

# one position test
system('time locest search -i test2Obs.tsv -g test2GridOnePoint.tsv -t "-5750, -5500,-5250, -5000, -4750" -d "varC1=-0.0885337:0.0570383:0.01+varC2=-0.0669435:0.1100580:0.01" -o test2Interpolate.tsv')

hu <- readr::read_tsv("test2Interpolate.tsv")

hu %>%
  ggplot() +
  facet_wrap(~age) +
  geom_raster(aes(varC1, varC2, fill = probability)) +
  scale_fill_viridis_c() +
  coord_fixed()


# crossvalidation position test
system('time locest crossvalidate -i test2Obs.tsv --testFraction 0.1 --iterations 5 -o test2Crossvalidate.tsv')

# system('locest search -i test2Obs.tsv -g debugGrid.tsv -t "-4000" -d "varC1=0.0410592+varC2=-0.0223009" -o debugInterpolate.tsv')

