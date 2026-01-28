library(magrittr)

#### derive test data files from the mobest data analysis project ####

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

test_area <- sf::st_read("data_tracked/test_area.gpkg")

# prediction grid
spatiotemporal_grid <- mobest::create_prediction_grid(
  test_area,
  spatial_cell_size = 30000
) %>% mobest::geopos_to_spatpos(-7000)

spatiotemporal_grid %>%
  dplyr::select(
    spatID = id, x, y
  ) %>%
  dplyr::mutate(
    yearBCAD = -5000
  ) %>%
  readr::write_tsv(file = "data/spatiotemporal/grid.tsv")

# observations file
test_observations <- readr::read_tsv("data_tracked/test_observations.janno")

test_observations %>%
  dplyr::select(
    obsID = Poseidon_ID,
    x, y,
    yearBCAD = Date_BC_AD_Median_Derived,
    depC1 = C1_mds_u,
    depC2 = C2_mds_u
  )  %>%
  readr::write_tsv(file = "data/spatiotemporal/obs.tsv")

# temporal resampling
test_observations %>%
  dplyr::select(
    Poseidon_ID,
    Date_Type,
    Date_C14_Uncal_BP, Date_C14_Uncal_BP_Err,
    Date_BC_AD_Start, Date_BC_AD_Stop
  ) %>%
  dplyr::mutate(
    currycarbon_expression =
      dplyr::case_when(
        Date_Type == "C14" ~
          purrr::pmap_chr(
            list(Poseidon_ID, Date_C14_Uncal_BP, Date_C14_Uncal_BP_Err),
            \(id, bp, sigma) {
              paste0(
                id, ": ",
                paste0("(", bp, ",", sigma, ")", collapse = " + "))
            }
          ),
        TRUE ~ paste0(
            Poseidon_ID, ": ",
            "rangeBCAD(", Date_BC_AD_Start, ",", Date_BC_AD_Stop, ")"
          )
      )
  ) %$%
  currycarbon_expression %>%
  writeLines(con = "data/spatiotemporal/currycarbon_input.txt")

system("currycarbon -i data/spatiotemporal/currycarbon_input.txt -q --samplesFile data/spatiotemporal/currycarbon_result.tsv -n 5 --seed 123")

# search position
test_observations %>%
  dplyr::filter(grepl("Stuttgart", Poseidon_ID)) %>%
  dplyr::select(Poseidon_ID, C1_mds_u, C2_mds_u) %>%
  as.matrix
