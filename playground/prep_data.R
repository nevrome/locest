library(magrittr)

load("../../mobest.analysis.2022/data/genotype_data/janno_final.RData")
load("../../mobest.analysis.2022/data/spatial/extended_area.RData")

hu <- mobest::create_prediction_grid(
  extended_area,
  spatial_cell_size = 80000
) %>% mobest::geopos_to_spatpos(-7000)

hu %>%
  dplyr::select(
    id, x, y
  ) %>%
  readr::write_tsv(file = "~/agora/locest/playground/test2Grid.tsv")

janno_final %>%
  dplyr::select(
    obsID = Poseidon_ID,
    x, y,
    age = Date_BC_AD_Median_Derived,
    varC1 = C1_mds_u,
    varC2 = C2_mds_u
  )  %>%
  readr::write_tsv(file = "~/agora/locest/playground/test2Obs.tsv")

janno_final %>%
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
              paste0(id, ": ", paste0("(", bp, ",", sigma, ")", collapse = " + "))
            }
          ),
        TRUE ~ paste0(
            Poseidon_ID, ": rangeBCAD(", Date_BC_AD_Start, ",", Date_BC_AD_Stop, ")"
          )
      )
  ) %$%
  currycarbon_expression %>%
  writeLines(con = "test2CurrycarbonInput.txt")

system("currycarbon -i test2CurrycarbonInput.txt -q --samplesFile test2CurrycarbonSamples.tsv -n 5 --seed 123")

janno_final %>%
  dplyr::filter(grepl("Stuttgart", Poseidon_ID)) %>%
  dplyr::select(Poseidon_ID, C1_mds_u, C2_mds_u) %>%
  as.matrix

janno_final %>%
  dplyr::filter(grepl("Stuttgart", Poseidon_ID)) %>%
  dplyr::select(
    obsID = Poseidon_ID,
    x, y
  )  %>%
  readr::write_tsv(file = "~/agora/locest/playground/test2GridOnePoint.tsv")

range(janno_final$C1_mds_u)
range(janno_final$C2_mds_u)
seq(min(janno_final$C1_mds_u), max(janno_final$C1_mds_u), 0.01)
seq(min(janno_final$C2_mds_u), max(janno_final$C2_mds_u), 0.01)

