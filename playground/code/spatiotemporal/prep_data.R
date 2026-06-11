library(magrittr)

#### compile test data files from the mobest data analysis project ####

# load("~/agora/mobest.analysis.2022/data/genotype_data/janno_final.RData")
# janno_final %>%
#   dplyr::select(
#     Poseidon_ID, Genetic_Sex, Group_Name,
#     Latitude, Longitude, x, y,
#     Date_Type, Date_C14_Labnr, Date_C14_Uncal_BP, Date_C14_Uncal_BP_Err,
#     Date_BC_AD_Start, Date_BC_AD_Stop,
#     Date_BC_AD_Median_Derived,
#     C1_mds_u, C2_mds_u,
#     Publication
#   ) %>%
#   janno::as.janno() %>%
#   janno::write_janno(
#     path = "data_tracked/test_observations.janno",
#     remove_source_file_column = T
#   )
# 
# load("~/agora/mobest.analysis.2022/data/spatial/extended_area.RData")
# extended_area %>% sf::st_write(dsn = "data_tracked/test_area.gpkg")

#### prepare derived data products for locest tests ####

# prediction grid
test_area <- sf::st_read("data_tracked/test_area.gpkg")
sf::write_sf(test_area, "data/spatiotemporal/area.geojson", delete_dsn = TRUE)
system("locest grid --polygonFile data/spatiotemporal/area.geojson -x 75000 -y 75000 -o data/spatiotemporal/grid.tsv")
grid <- readr::read_tsv("data/spatiotemporal/grid.tsv")
plot(grid$x, grid$y)

# observations
test_observations <- readr::read_tsv("data_tracked/test_observations.janno")
obs <- test_observations %>%
  dplyr::select(
    obsID = Poseidon_ID,
    x, y,
    yearBCAD = Date_BC_AD_Median_Derived,
    depC1 = C1_mds_u,
    depC2 = C2_mds_u
  )
obs %>% readr::write_tsv(file = "data/spatiotemporal/obs.tsv")

# temporal resampling
system("currycarbon -t data_tracked/test_observations.janno -q --samplesFile data/spatiotemporal/age_samples.tsv -n 5 --seed 123")

# search observations
obs %>%
  dplyr::filter(obsID %in% c("UzOO77", "Stuttgart_published.DG", "R19.SG")) %>%
  readr::write_tsv(file = "data/spatiotemporal/search_obs.tsv")

# search position
test_observations %>%
  dplyr::filter(grepl("Stuttgart", Poseidon_ID)) %>%
  dplyr::select(Poseidon_ID, C1_mds_u, C2_mds_u) %>%
  as.matrix
