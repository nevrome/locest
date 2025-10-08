library(magrittr)
library(ggplot2)

obs <- readr::read_tsv("data/spatiotemporal/obs.tsv")

# stack install --profile
# stack exec --profile -- locest vario --obsFile data/spatiotemporal/obs.tsv --variogramOutFile data/spatiotemporal/vario.tsv +RTS -hc -l
# eventlog2html locest.eventlog
# stack exec --profile -- locest vario --obsFile data/spatiotemporal/obs.tsv --variogramOutFile data/spatiotemporal/vario.tsv +RTS -p
# profiteur locest.prof

system('time locest vario --obsFile data/spatiotemporal/obs.tsv --outMode "EqualSize(100)" --outFile data/spatiotemporal/vario.tsv')

vario <- readr::read_tsv("data/spatiotemporal/vario.tsv")

vario %>%
  ggplot() +
  geom_point(aes(bin_mid, variance)) +
  facet_grid(
    rows = dplyr::vars(depVar),
    cols = dplyr::vars(indepVar),
    scales = "free"
  )

# crossvalidation

# stack exec --profile -- locest cross --configFile code/spatiotemporal/cross.conf +RTS -p
# profiteur locest.prof

system('time locest cross --configFile code/spatiotemporal/cross.conf +RTS -N3 -RTS')

cross <- readr::read_tsv("data/spatiotemporal/cross.tsv")

cross %>%
  ggplot() +
  geom_raster(aes(x = kernel_space_length, y = kernel_time_length, fill = sum_log_likelihood)) +
  facet_grid(
    rows = dplyr::vars(depVar)
  ) +
  scale_fill_viridis_c()

# system('time locest serialise --obsFile data/spatiotemporal/obs.tsv --outFile data/spatiotemporal/obs.cbor')

# normal search test
# stack exec --profile -- locest search --configFile code/spatiotemporal/basic.conf +RTS -p
# profiteur locest.prof
# stack exec --profile -- locest search --configFile code/spatiotemporal/basic.conf +RTS -hy
# hp2ps -c locest.hp
system('time locest search --configFile code/spatiotemporal/basic.conf  +RTS -N22 -RTS')

hu5 <- readr::read_tsv("data/spatiotemporal/basic_result.tsv")

# https://stackoverflow.com/questions/30510898/split-facet-plot-into-list-of-plots
splitFacet <- function(x){
  facet_vars <- names(x$facet$params$facets)        
  x$facet    <- ggplot2::ggplot()$facet             
  datasets   <- split(x$data, x$data[facet_vars])   
  lapply(datasets,function(new_data) {x$data <- new_data; x})
}

hu5 %>%
  dplyr::filter(search_obsID == "Stuttgart_published.DG") %>%
  ggplot() +
  facet_wrap(~yearBCAD) +
  geom_raster(aes(x, y, fill = exp(agg_log_likelihood))) +
  geom_raster(
    data = hu5 %>% dplyr::filter(!interpol_depC1_post),
    aes(x, y), fill = "white", alpha = 0.3
  ) +
  geom_point(
    data = obs %>%
      dplyr::filter(yearBCAD > -7500 & yearBCAD < -4500) %>%
      dplyr::mutate(yearBCAD = round(yearBCAD, -3)),
    aes(x,y),
    shape = 4, color = "red"
  ) +
  scale_fill_viridis_c() +
  coord_fixed() -> p

splitFacet(p)

hu5 %>%
  ggplot() +
  facet_wrap(~yearBCAD) +
  geom_raster(aes(x, y, fill = dep_dist_euclidean)) +
  scale_fill_viridis_c() +
  coord_fixed()

# # one position test
# system('time locest search -i test2Obs.tsv -g test2GridOnePoint.tsv -t "c(-5750, -5500,-5250, -5000, -4750)" -d "c(varC1=-0.0885337:0.0570383:0.01,varC2=-0.0669435:0.1100580:0.01)" -a "SepIDW(c(varC1 = LinearSum(0.00001, 0.00001), varC2 = LinearSum(0.00001, 0.00001)), DistanceWeightedMean)" -o test_res/test2Interpolate.tsv')
# 
# hu <- readr::read_tsv("test_res/test2Interpolate.tsv")
# 
# hu %>%
#   ggplot() +
#   facet_wrap(~yearBCAD) +
#   geom_raster(aes(varC1, varC2, fill = probability)) +
#   scale_fill_viridis_c() +
#   coord_fixed()
# 
# 
# # test with own distance matrix
# 
# system('time locest search -i distMatrixObs.tsv -g distMatrixGrid.tsv --spatDistFile distMatrixDists.tsv -t "c(0)" -d "c(varC1 = 0,varC2 = 0)" -a "KAS(c(varC1 = Normal(200, 200), varC2 = Normal(200, 200)))" -o test_res/distMatrixTestSearch.tsv')
# 
# hu <- readr::read_tsv("test_res/distMatrixTestSearch.tsv")
# 
# hu %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = probability)) +
#   scale_fill_viridis_c() +
#   coord_fixed()
# 
# # temporal resampling test


