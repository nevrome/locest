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

