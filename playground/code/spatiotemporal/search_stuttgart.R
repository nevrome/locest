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
system('time locest serialise crossdist -i data/spatiotemporal/obs.tsv -g data/spatiotemporal/grid.tsv --distFile data/spatiotemporal/obsGridDistFile.tsv -o data/spatiotemporal/obsGridDistFile.cbor')

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
system('time locest serialise selfdist -i data/spatiotemporal/obs.tsv --distFile data/spatiotemporal/obsObsDistFile.tsv -o data/spatiotemporal/obsObsDistFile.cbor')

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
system('time locest serialise selfdist -g data/spatiotemporal/grid.tsv --distFile data/spatiotemporal/gridGridDistFile.tsv -o data/spatiotemporal/gridGridDistFile.cbor')

# stack install --profile
# stack exec --profile -- locest vario --obsFile data/spatiotemporal/obs.tsv --variogramOutFile data/spatiotemporal/vario.tsv +RTS -hc -l
# eventlog2html locest.eventlog
# stack exec --profile -- locest vario --obsFile data/spatiotemporal/obs.tsv --variogramOutFile data/spatiotemporal/vario.tsv +RTS -p
# profiteur locest.prof

system('time locest vario --obsFile data/spatiotemporal/obs.tsv --outMode "EqualSize(100)" --outFile data/spatiotemporal/vario.tsv --across AllCombinations')

vario_res <- readr::read_tsv("data/spatiotemporal/vario.tsv")

vario_res %>%
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

system('time OMP_NUM_THREADS=4 locest cross --configFile code/spatiotemporal/cross.conf')

cross_res <- readr::read_tsv("data/spatiotemporal/cross.tsv")

cross_res %>%
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
  dplyr::filter(depVar == "depC2") %>%
  ggplot() +
  geom_raster(aes(x = kernel_space_length, y = kernel_time_length, fill = sum_log_likelihood)) +
  facet_grid(rows = dplyr::vars(depVar)) +
  scale_fill_viridis_c()#direction = -1)

# system('time locest serialise --obsFile data/spatiotemporal/obs.tsv --outFile data/spatiotemporal/obs.cbor')

# normal search test
# stack exec --profile -- locest search --configFile code/spatiotemporal/basic.conf +RTS -p
# profiteur locest.prof
# memory profiling:
# OMP_NUM_THREADS=4 stack exec --profile -- locest search --configFile code/spatiotemporal/basic.conf +RTS -hy -RTS
# hp2ps -c locest.hp

system('time OMP_NUM_THREADS=20 locest search --configFile code/spatiotemporal/basic.conf')

# better memory profiling with GNU time
# export TIME="time result\ncmd: %C\nreal %es\nuser %Us \nsys  %Ss \nmemory: %MKB \ncpu: %P"
# OMP_NUM_THREADS=4 /usr/bin/time locest search --configFile code/spatiotemporal/basic.conf

search_res <- readr::read_tsv("data/spatiotemporal/basic_result.tsv")

# normalization sanity check
search_res %>% dplyr::group_by(search_obsID, grid_yearBCAD) %>%
  dplyr::summarize(hu = sum(search_probability))

search_res %>%
  dplyr::mutate(probability = search_probability) %>%
  dplyr::filter(temp_sampling_iteration == 0) %>%
  ggplot() +
  facet_grid(rows = dplyr::vars(grid_yearBCAD), cols = dplyr::vars(search_obsID)) +
  geom_raster(aes(grid_x, grid_y, fill = probability)) +
  # geom_point(
  #   data = obs %>%
  #     dplyr::filter(yearBCAD > -7500 & yearBCAD < -3500) %>%
  #     dplyr::mutate(yearBCAD = round(yearBCAD, -3)),
  #   aes(x,y),
  #   shape = 4, color = "red"
  # ) +
  scale_fill_viridis_c() +
  coord_fixed()
