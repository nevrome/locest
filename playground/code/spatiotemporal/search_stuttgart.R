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
  #dplyr::transmute(gridID, obsID, space = value/1000)
  dplyr::transmute(space = round(value/1000, 1))
readr::write_tsv(obsGridDists, "data/spatiotemporal/obsGridDistFile.tsv")
system('time locest serialise crossdist -i data/spatiotemporal/obs.tsv -g data/spatiotemporal/grid.tsv --obsGridDistFile data/spatiotemporal/obsGridDistFile.tsv -o data/spatiotemporal/obsGridDistFile.cbor')

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
system('time locest serialise selfdist -i data/spatiotemporal/obs.tsv --obsObsDistFile data/spatiotemporal/obsObsDistFile.tsv -o data/spatiotemporal/obsObsDistFile.cbor')

# space_mat <- fields::rdist(as.matrix(grid[c("x","y")])) / 1000
# n <- nrow(grid)
# pairs <- do.call(rbind, lapply(0:(n-1), function(i) cbind(row = i, col = 0:i)))
# gridGridPacked <- tibble::tibble(
#   #id1 = grid$spatID[pairs[, "row"] + 1],
#   #id2 = grid$spatID[pairs[, "col"] + 1],
#   #space = space_mat[cbind(pairs[, "row"] + 1, pairs[, "col"] + 1)]
#   space = round(space_mat[cbind(pairs[, "row"] + 1, pairs[, "col"] + 1)], 1)
# )
# readr::write_tsv(gridGridPacked, "data/spatiotemporal/gridGridDistFile.tsv")
# system('time locest serialise selfdist -g data/spatiotemporal/grid.tsv --gridGridDistFile data/spatiotemporal/gridGridDistFile.tsv -o data/spatiotemporal/gridGridDistFile.cbor')

#### vario ####

# stack install --profile
# stack exec --profile -- locest vario --obsFile data/spatiotemporal/obs.tsv --variogramOutFile data/spatiotemporal/vario.tsv +RTS -hc -l
# eventlog2html locest.eventlog
# stack exec --profile -- locest vario --obsFile data/spatiotemporal/obs.tsv --variogramOutFile data/spatiotemporal/vario.tsv +RTS -p
# profiteur locest.prof

# full empirical variogram
# --across AllCombinations
system('time locest varioemp --obsFile data/spatiotemporal/obs.tsv --outMode "equalSize(100)" --outFile data/spatiotemporal/vario_emp.tsv')
vario_emp <- readr::read_tsv("data/spatiotemporal/vario_emp.tsv")
vario_emp %>%
  ggplot() +
  facet_grid(rows = dplyr::vars(depVar), cols = dplyr::vars(indepVar), scales = "free") +
  geom_point(aes(bin_mid, variance)) +
  scale_y_continuous(limits = c(0, NA))

# distance-filtered empirical variogram
system('time locest varioemp --obsFile data/spatiotemporal/obs.tsv --outMode "equalSize(100)" --outFile data/spatiotemporal/vario_emp.tsv --indepVarsThresholds "c(space = 2500, time = 2500)"')
vario_emp <- readr::read_tsv("data/spatiotemporal/vario_emp.tsv")
vario_emp %>%
  ggplot() +
  facet_grid(rows = dplyr::vars(depVar), cols = dplyr::vars(indepVar), scales = "free") +
  geom_point(aes(bin_mid, variance)) +
  scale_y_continuous(limits = c(0, NA))

# fit theoretical variogram
system('time locest variofit --empVarioFile data/spatiotemporal/vario_emp.tsv --outFile data/spatiotemporal/vario_fit.tsv -k SqEx')

vario_fit <- readr::read_tsv("data/spatiotemporal/vario_fit.tsv")

variogram_fun <- function(kernel, h, nug, psill, range) {
  switch(
    kernel,
    "SqEx"   = nug + psill * (1 - exp(-(h^2) / (range^2))),
    "Ex"     = nug + psill * (1 - exp(-h / range)),
    "Linear" = nug + psill * pmin(1, h / range),
    stop("Unknown kernel")
  )
}

vario_curves <- vario_emp %>%
  dplyr::group_by(indepVar, depVar) %>%
  dplyr::summarise(h_min = min(bin_mid), h_max = max(bin_mid[!is.infinite(bin_mid)]), .groups = "drop") %>%
  dplyr::left_join(vario_fit, by = c("indepVar", "depVar")) %>%
  dplyr::mutate(
    h = purrr::map2(h_min, h_max, \(x, y) seq(x, y, length.out = 200)),
  ) %>%
  tidyr::unnest(h) %>%
  dplyr::mutate(
    .,
    gamma = purrr::pmap_dbl(., \(kernel, h, nugget, partial_sill, range, ...) {
      variogram_fun(kernel, h, nugget, partial_sill, range)
    })
  )

ggplot() +
  facet_grid(rows = vars(depVar), cols = vars(indepVar), scales = "free") +
  geom_point(
    data = vario_emp,
    aes(x = bin_mid, y = variance),
  ) +
  geom_line(
    data = vario_curves,
    aes(x = h, y = gamma, colour = kernel)
  ) +
  geom_vline(
    data = vario_fit,
    aes(xintercept = range, colour = kernel)
  ) +
  scale_y_continuous(limits = c(0, NA))

#### cross ####

# stack install --profile
# stack exec --profile -- locest cross --configFile code/spatiotemporal/cross.conf +RTS -p
# profiteur locest.prof

system('time OMP_NUM_THREADS=3 locest cross --configFile code/spatiotemporal/cross.conf')

# run with slurm
# srun --cpus-per-task=3 --export=ALL,OMP_NUM_THREADS=3,OPENBLAS_VERBOSE=2 time locest cross --configFile code/spatiotemporal/cross.conf

cross_res <- readr::read_tsv("data/spatiotemporal/cross.tsv")

kernel_grid_locest <- cross_res %>%
  dplyr::filter(iteration == 1) %>%
  dplyr::group_by(depVar, kernel_space_length, kernel_time_length) %>%
  dplyr::summarise(
    dplyr::across(
      tidyselect::all_of(c(
        "sum_dep_dist_euclidean",
        "mean_squared_dep_dist_euclidean",
        "sum_log_likelihood")),
      mean
    ), .groups = "drop"
  ) %>%
  dplyr::mutate(
    meas = mean_squared_dep_dist_euclidean
  )

kernel_grid_locest %>%
  dplyr::group_by(depVar) %>%
  dplyr::slice_min(meas)

p1 <- ggplot() +
  geom_raster(
    data = kernel_grid_locest %>% dplyr::filter(depVar == "depC1"),
    mapping = aes(x = kernel_space_length, y = kernel_time_length, fill = meas)
  ) +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed()

p2 <- ggplot() +
  geom_raster(
    data = kernel_grid_locest %>% dplyr::filter(depVar == "depC2"),
    mapping = aes(x = kernel_space_length, y = kernel_time_length, fill = meas)
  ) +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed()

cowplot::plot_grid(p1, p2)

#### search ####

# normal search test
# stack install --profile
# stack exec --profile -- locest search --configFile code/spatiotemporal/basic.conf +RTS -p
# profiteur locest.prof
# memory profiling:
# OMP_NUM_THREADS=20 stack exec --profile -- locest search --configFile code/spatiotemporal/basic.conf +RTS -hy -RTS
# hp2ps -c locest.hp

system('time OMP_NUM_THREADS=3 locest search --configFile code/spatiotemporal/basic.conf')

# run with slurm
# srun --cpus-per-task=20 --export=ALL,OMP_NUM_THREADS=20 time locest search --configFile code/spatiotemporal/basic.conf
# access slurm node to see resource use
# srun --pty -w hpc034 bash

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
