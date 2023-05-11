library(magrittr)

load("../../mobest.analysis.2022/data/genotype_data/janno_final.RData")
load("../../mobest.analysis.2022/data/spatial/extended_area.RData")

hu <- mobest::create_prediction_grid(
  extended_area,
  spatial_cell_size = 50000
) %>% mobest::geopos_to_spatpos(-7000)

hu %>%
  dplyr::select(
    id, x, y,
    age = z
  ) %>%
  readr::write_tsv(file = "~/agora/locest/playground/test2Grid.tsv")

janno_final %>%
  dplyr::select(
    id = Poseidon_ID,
    x, y,
    age = Date_BC_AD_Median_Derived,
    pc1 = C1_mds_u
  )  %>%
  readr::write_tsv(file = "~/agora/locest/playground/test2Obs.tsv")
