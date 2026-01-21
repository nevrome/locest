library(magrittr)
library(ggplot2)

obs <- readr::read_tsv("data/spatiotemporal/obs.tsv")
grid <- readr::read_tsv("data/spatiotemporal/grid.tsv")

obsGridDists <- fields::rdist(
  as.matrix(obs[c("x", "y")]),
  as.matrix(grid[c("x", "y")])
) %>%
  reshape2::melt() %>%
  dplyr::left_join(
    obs %>% dplyr::transmute(id = 1:dplyr::n(), obsID),
    by = c("Var1" = "id")
  ) %>%
  dplyr::left_join(
    grid %>% dplyr::transmute(id = 1:dplyr::n(), gridID = spatID),
    by = c("Var2" = "id")
  ) %>%
  #dplyr::transmute(obsID, gridID, space = value/1000)
  dplyr::transmute(space = round(value/1000, 1))
readr::write_tsv(obsGridDists, "data/spatiotemporal/obsGridDistFile.tsv")
system('time locest serialise audist -i data/spatiotemporal/obs.tsv -g data/spatiotemporal/grid.tsv --distFile data/spatiotemporal/obsGridDistFile.tsv -o data/spatiotemporal/obsGridDistFile.cbor')

space_mat <- fields::rdist(as.matrix(obs[c("x","y")])) / 1000
n <- nrow(obs)
pairs <- do.call(rbind, lapply(0:(n-1), function(i) cbind(row = i, col = 0:i)))
obsObsPacked <- tibble::tibble(
  #id1 = obs$obsID[pairs[, "row"] + 1],
  #id2 = obs$obsID[pairs[, "col"] + 1],
  #space = space_mat[cbind(pairs[, "row"] + 1, pairs[, "col"] + 1)]
  space = round(space_mat[cbind(pairs[, "row"] + 1, pairs[, "col"] + 1)], 1)
)
readr::write_tsv(obsObsPacked, "data/spatiotemporal/obsObsDistFile.tsv")
system('time locest serialise sudist -i data/spatiotemporal/obs.tsv --distFile data/spatiotemporal/obsObsDistFile.tsv -o data/spatiotemporal/obsObsDistFile.cbor')

space_mat <- fields::rdist(as.matrix(grid[c("x","y")])) / 1000
n <- nrow(grid)
pairs <- do.call(rbind, lapply(0:(n-1), function(i) cbind(row = i, col = 0:i)))
gridGridPacked <- tibble::tibble(
  #id1 = grid$spatID[pairs[, "row"] + 1],
  #id2 = grid$spatID[pairs[, "col"] + 1],
  #space = space_mat[cbind(pairs[, "row"] + 1, pairs[, "col"] + 1)]
  space = round(space_mat[cbind(pairs[, "row"] + 1, pairs[, "col"] + 1)], 1)
)
readr::write_tsv(gridGridPacked, "data/spatiotemporal/gridGridDistFile.tsv")
system('time locest serialise sudist -g data/spatiotemporal/grid.tsv --distFile data/spatiotemporal/gridGridDistFile.tsv -o data/spatiotemporal/gridGridDistFile.cbor')

# stack install --profile
# stack exec --profile -- locest vario --obsFile data/spatiotemporal/obs.tsv --variogramOutFile data/spatiotemporal/vario.tsv +RTS -hc -l
# eventlog2html locest.eventlog
# stack exec --profile -- locest vario --obsFile data/spatiotemporal/obs.tsv --variogramOutFile data/spatiotemporal/vario.tsv +RTS -p
# profiteur locest.prof

system('time locest vario --obsFile data/spatiotemporal/obs.tsv --outMode "EqualSize(100)" --outFile data/spatiotemporal/vario.tsv --across AllCombinations')

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

system('time OMP_NUM_THREADS=4 locest cross --configFile code/spatiotemporal/cross.conf +RTS -N3 -RTS')

cross <- readr::read_tsv("data/spatiotemporal/cross.tsv")

cross %>%
  dplyr::group_by(depVar, kernel_space_length, kernel_time_length) %>%
  dplyr::summarise(
    dplyr::across(
      tidyselect::all_of(c(
        "sum_dep_dist_euclidean",
        "mean_squared_dep_dist_euclidean",
        "sum_log_likelihood")),
      mean
    )
  ) %>%
  dplyr::filter(depVar == "depC1") %>%
  ggplot() +
  geom_raster(aes(x = kernel_space_length, y = kernel_time_length, fill = sum_log_likelihood)) +
  facet_grid(rows = dplyr::vars(depVar)) +
  scale_fill_viridis_c()#direction = -1)

# system('time locest serialise --obsFile data/spatiotemporal/obs.tsv --outFile data/spatiotemporal/obs.cbor')

# normal search test
# stack exec --profile -- locest search --configFile code/spatiotemporal/basic.conf +RTS -p
# profiteur locest.prof
# memory profiling:
# OMP_NUM_THREADS=4 stack exec --profile -- locest search --configFile code/spatiotemporal/basic.conf +RTS -hy -N4 -RTS
# hp2ps -c locest.hp

system('time OMP_NUM_THREADS=4 locest search --configFile code/spatiotemporal/basic.conf  +RTS -N4 -RTS')

# better memory profiling with GNU time
# export TIME="time result\ncmd: %C\nreal %es\nuser %Us \nsys  %Ss \nmemory: %MKB \ncpu: %P"
# OMP_NUM_THREADS=4 /usr/bin/time locest search --configFile code/spatiotemporal/basic.conf  +RTS -N4 -RTS

hu5 <- readr::read_tsv("data/spatiotemporal/basic_result.tsv")

# normalization sanity check
hu5 %>% dplyr::group_by(search_obsID, yearBCAD) %>%
  dplyr::summarize(hu = sum(probability))

# https://stackoverflow.com/questions/30510898/split-facet-plot-into-list-of-plots
# splitFacet <- function(x){
#   facet_vars <- names(x$facet$params$facets)        
#   x$facet    <- ggplot2::ggplot()$facet             
#   datasets   <- split(x$data, x$data[facet_vars])   
#   lapply(datasets,function(new_data) {x$data <- new_data; x})
# }

hu5 %>%
  dplyr::filter(temp_sampling_iteration == 0) %>%
  ggplot() +
  facet_grid(rows = dplyr::vars(yearBCAD), cols = dplyr::vars(search_obsID)) +
  geom_raster(aes(x, y, fill = probability)) +
  # geom_point(
  #   data = obs %>%
  #     dplyr::filter(yearBCAD > -7500 & yearBCAD < -3500) %>%
  #     dplyr::mutate(yearBCAD = round(yearBCAD, -3)),
  #   aes(x,y),
  #   shape = 4, color = "red"
  # ) +
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


