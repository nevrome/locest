library(magrittr)
library(ggplot2)

start.time <- Sys.time()

obs <- readr::read_tsv("data/spatiotemporal/obs.tsv")
grid <- readr::read_tsv("data/spatiotemporal/grid.tsv")
search_obs <- readr::read_tsv("data/spatiotemporal/search_obs.tsv")

spatial_pred_grid <- grid %>%
  dplyr::transmute(id = spatID, x, y) %>%
  set_class(., c("mobest_spatialpositions", class(.)))

ind <- mobest::create_spatpos(
  id = obs$obsID,
  x  = obs$x,
  y  = obs$y,
  z  = obs$yearBCAD
)
dep <- mobest::create_obs(
  C1 = obs$depC1,
  C2 = obs$depC2
)

search_ind <- mobest::create_spatpos(
  id = search_obs$obsID,
  x  = search_obs$x,
  y  = search_obs$y,
  z  = search_obs$yearBCAD
)
search_dep <- mobest::create_obs(
  C1 = search_obs$depC1,
  C2 = search_obs$depC2
)

kernset <- mobest::create_kernset(
  C1 = mobest::create_kernel(
    dsx = 800 * 1000, dsy = 800 * 1000, dt = 800,
    g = 0.1
  ),
  C2 = mobest::create_kernel(
    dsx = 800 * 1000, dsy = 800 * 1000, dt = 800,
    g = 0.1
  )
)

search_result <- mobest::locate(
  independent        = ind,
  dependent          = dep,
  kernel             = kernset,
  search_independent = search_ind,
  search_dependent   = search_dep,
  search_space_grid  = spatial_pred_grid,
  search_time        = c(-7000, -6000, -5000),
  search_time_mode   = "absolute"
)

search_product <- mobest::multiply_dependent_probabilities(search_result)

end.time <- Sys.time()
end.time - start.time

ggplot() +
  facet_grid(rows = dplyr::vars(field_z), cols = dplyr::vars(search_id)) +
  geom_raster(
    data = search_product,
    mapping = aes(x = field_x, y = field_y, fill = probability)
  ) +
  scale_fill_viridis_c() +
  coord_fixed()

# crossvalidation
kernels_to_test <-
  # create a permutation grid of spatial (ds) and temporal (dt) lengthscale parameters
  expand.grid(
    ds = seq(200, 1200, 200)*1000, # *1000 to transform from kilometres to meters
    dt = seq(200, 1200, 200)
  ) %>%
  # create objects of type mobest_kernelsetting from them
  purrr::pmap(function(...) {
    row <- list(...)
    mobest::create_kernset(
      C1 = mobest::create_kernel(
        dsx = row$ds,
        dsy = row$ds,
        dt  = row$dt,
        g   = 0.1
      ),
      C2 = mobest::create_kernel(
        dsx = row$ds,
        dsy = row$ds,
        dt  = row$dt,
        g   = 0.1
      )
    )
  }) %>%
  # name then and  package them in an object of type mobest_kernelsetting_multi
  magrittr::set_names(paste("kernel", 1:length(.), sep = "_")) %>%
  do.call(mobest::create_kernset_multi, .)

interpol_comparison <- mobest::crossvalidate(
  independent = ind,
  dependent   = dep,
  kernel      = kernels_to_test,
  iterations  = 2, # in a real-world setting this should be set to 10+ iterations
  groups      = 5, # and this to 10
  quiet       = F
)

kernel_grid_mobest <- interpol_comparison %>%
  dplyr::group_by(dependent_var_id, ds = dsx, dt) %>%
  dplyr::summarise(
    meas = mean(difference^2),
    .groups = "drop"
  )

p1 <- ggplot() +
  geom_raster(
    data = kernel_grid_mobest %>% dplyr::filter(dependent_var_id == "C1"),
    mapping = aes(x = ds / 1000, y = dt, fill = meas)
  ) +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed() +
  theme_bw()

p2 <- ggplot() +
  geom_raster(
    data = kernel_grid_mobest %>% dplyr::filter(dependent_var_id == "C2"),
    mapping = aes(x = ds / 1000, y = dt, fill = meas)
  ) +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed()

cowplot::plot_grid(p1, p2)
