# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse"), 
  format = "rds"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/Functions.R")

# Replace the target list below with your own:
list(
  tar_target(
    name = file,
    command = "data/INCLINE_community_subplot_fixed.csv", 
    format = "file"
  ),
  tar_target(
    name = community_all,
    command = read_csv(file)
  ),
  tar_target(
    name = community,
    command = filter(community_all, !treatment == "R") # We are not interested in the removal plots for this study
  ),
  tar_target(
    name = community_cf, # We clean the file accordingly
    command = community |> 
      mutate(species = ifelse(species == "Agr_cap_cf", "Agr_cap", species)) |> 
      mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Gud_4_3", "Car_fla", species)) |> 
      mutate(species = ifelse(species == "Car_cap_cf", "Car_cap", species)) |> 
      mutate(species = ifelse(species == "Car_nig_cf", "Car_big", species)) |> 
      mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_3_3", "Car_nor", species)) |> 
      mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Skj_1_1", "Car_cap", species)) |> 
      mutate(species = ifelse(species == "Epi_ana_cf", "Epi_ana", species)) |> 
      filter(!(species == "Ran_acr_cf")) |> 
      filter(!(species == "Ran_acr" & plotID == "Gud_2_2")) |> 
      filter(!(species == "Vio_can_cf" & plotID == "Ulv_7_4")) |> 
      mutate(value = ifelse(species == "Vio_bif" & plotID == "Ulv_7_4" & subPlot == 30 & year == 2023, "1j", value)) |> 
      mutate(juvenile = ifelse(species == "Vio_bif" & plotID == "Ulv_7_4" & subPlot == 30 & year == 2023, "TRUE", juvenile)) |> 
      unique()
  ),
  tar_target(
    name = carex_double, # Create a duplicate, since it seems both Car_big and Car_vag are present in subplot 8 in 2018
    command = filter(community_cf, species == "Car_sp" & plotID == "Gud_7_2" & subPlot == 8 & year == 2018)
  ),
  tar_target(
    name = community_cf_sp, # We clean the file accordingly
    command = community_cf |> 
      filter(species != "Ant_sp") |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_1_2" & year == 2018, "Car_vag", species)) |> 
      filter(!(species == "Car_sp" & plotID == "Gud_1_2")) |>
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_1_3", "Car_vag", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_1_4", "Car_vag", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_1_5", "Car_vag", species)) |>
      filter(!(species == "Car_sp" & plotID == "Gud_2_1" & subPlot == 1 & year == 2019)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_2_1", "Car_big", species)) |> 
      filter(!(species == "Car_sp" & plotID == "Gud_2_2" & year == 2022 & subPlot == 17)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_2_2", "Car_big", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_2_3", "Car_big", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_2_4", "Car_big", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_3_2" & year == 2018, "Car_big", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_3_2" & year == 2019, "Car_vag", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_3_3", "Car_big", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_3_5", "Car_vag", species)) |> 
      filter(!(species == "Car_sp" & plotID == "Gud_4_1")) |> 
      filter(!(species == "Car_sp" & plotID == "Gud_4_3")) |> 
      filter(!(species == "Car_sp" & plotID == "Gud_4_4")) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_5_1", "Car_cap", species)) |> 
      filter(!(species == "Car_sp" & plotID == "Gud_5_2")) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_5_4", "Car_big", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_5_5", "Car_vag", species)) |> 
      #Gud_7_2 Car_big: 3, 12 (2018), 14, 16. Car_vag: 12 (2021), 21. Both: 8. Remove: 15, 35
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_2" & subPlot %in% c(3, 14, 16), "Car_big", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_2" & subPlot == 12 & year == 2018, "Car_big", species)) |>
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_2" & subPlot == 12 & year == 2021, "Car_vag", species)) |>
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_2" & subPlot == 21, "Car_vag", species)) |>
      filter(!(species == "Car_sp" & plotID == "Gud_7_2" & subPlot %in% c(15, 35))) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_2" & subPlot == 8, "Car_big", species)) |>
      bind_rows(carex_double) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_2" & subPlot == 8, "Car_vag", species)) |>
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_3" & subPlot == 30, "Car_big", species)) |> 
      filter(!(species == "Car_sp" & plotID == "Gud_7_3")) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_4", "Car_big", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_6" & subPlot %in% c(1, 24, 31), "Car_vag", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_6" & subPlot == 5, "Car_big", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_6" & subPlot == 8, "Car_fla", species)) |> 
      filter(!(species == "Car_sp" & plotID == "Gud_7_6" & subPlot == 2)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Lav_2_2", "Car_pil", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Lav_4_3", "Car_vag", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Skj_5_3", "Car_cap", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Ulv_3_3", "Car_pal", species)) |> 
      mutate(species = ifelse(species == "Car_sp" & plotID == "Ulv_3_5", "Car_big", species)) |> 
      filter(!(species == "Car_sp" & plotID == "Ulv_6_3")) |> 
      mutate(species = ifelse(species == "Epi_sp" & plotID %in% c("Skj_2_5", "Lav_5_3"), "Epi_ana", species)) |> 
      filter(!((plotID == "Gud_1_5" | plotID == "Skj_3_4") & species == "Epi_sp")) |> 
      mutate(value = ifelse(species == "Ver_alp" & plotID == "Skj_3_4" & year == 2021 & subPlot == 1, "1J", value)) |> 
      mutate(juvenile = ifelse(species == "Ver_alp" & plotID == "Skj_3_4" & year == 2021 & subPlot == 1, "TRUE", juvenile)) |> 
      mutate(species = ifelse(species == "Equ_sp", "Eup_wet", species)) |> 
      mutate(species = ifelse(species == "Fes_sp" & plotID == "Ulv_1_4", "Fes_ovi", species)) |> 
      mutate(species = ifelse(species == "Fes_sp", "Fes_rub", species)) |> 
      mutate(species = ifelse(species == "Gal_sp" & plotID == "Ulv_5_5", "Gal_bor", species)) |> 
      mutate(species = ifelse(species == "Gen_sp", "Gen_niv", species)) |> 
      mutate(species = ifelse(species == "Hyp_sp", "Hyp_mac", species)) |> 
      mutate(species = ifelse(species == "Leo_sp", "Leo_aut", species)) |> 
      mutate(species = ifelse(species == "Oma_sp", "Oma_sup", species)) |>
      mutate(species = ifelse(species %in% c("Pyr_min", "Pyr_rot"), "Pyr_sp", species)) |> 
      mutate(species = ifelse(species == "Ran_sp", "Ran_pyg", species)) |> 
      mutate(species = ifelse(species == "Rhi_sp", "Rhi_min", species)) |> 
      mutate(species = ifelse(species == "Sag_sp", "Sag_sag", species)) |> 
      filter(!(plotID %in% c("Lav_2_2", "Lav_3_3") & species == "Sal_sp")) |> 
      mutate(species = ifelse(species == "Sal_sp" & plotID == "Gud_5_1", "Sal_lan", species)) |> 
      mutate(species = ifelse(species == "Sel_sp", "Sel_sel", species)) |>
      mutate(species = ifelse(species == "Tri_sp", "Tri_pra", species)) |> 
      filter(!species == "Vio_sp") |> 
      mutate(value = ifelse(species == "Vio_bif" & plotID == "Ulv_1_5" & year == 2021 & subPlot %in% c(24, 29), "1s", value)) |> 
      mutate(seedling = ifelse(species == "Vio_bif" & plotID == "Ulv_1_5" & year == 2021 & subPlot %in% c(24, 29), "TRUE", seedling)) |> 
      unique()
  ),
  tar_target(
    name = community_cf_sp_unknown,
    command = community_cf_sp |> 
      mutate(species = ifelse(species == "Ver_cha_eller_Hyp_mac", "Hyp_mac", species)) |> 
      mutate(species = ifelse(species == "Unknown" & plotID == "Lav_1_3" & year == 2021, "Val_atr", species)) |> 
      mutate(species = ifelse(species == "Unknown" & plotID == "Lav_3_3" & year == 2021, "Suc_pra", species)) |> 
      mutate(species = ifelse(species == "Unknown" & plotID == "Gud_5_5" & year == 2021, "Dip_alp", species)) |> 
      filter(!(species %in% c("Nid_juvenile", "Nid_seedling", "Poaceae_sp", "Unknown"))) |> 
      unique()
  ),
  tar_target(
    name = tar_sp_lav_2_2_2021,
    command = community_cf_sp_unknown |> 
      filter(plotID == "Lav_2_2" & year == 2021 & subPlot %in% c(8, 10, 19, 20, 26, 32, 33) & value == 1) |> 
      mutate(functional_group = "Forbs") |> 
      mutate(species = "Tar_sp") |> 
      unique()
  ),
  tar_target(
    name = ver_alp_lav_2_2_2021,
    command = community_cf_sp_unknown |> 
      filter(plotID == "Lav_2_2" & year == 2021 & subPlot == 1 & functional_group == "Forbs" & value == 1) |> 
      mutate(species = "Ver_alp") |> 
      unique()
  ),
  tar_target(
    name = agr_cap_lav_3_3_2021,
    command = community_cf_sp_unknown |> 
      filter(plotID == "Lav_3_3" & year == 2021 & subPlot == 10 & functional_group == "Graminoids" & value == 1) |> 
      mutate(species = "Agr_cap")
  ),
  tar_target(
    name = community_clean,
    command = community_cf_sp_unknown |> 
      mutate(species = ifelse(species == "Ant_alp" & plotID == "Lav_2_2", "Alc_alp", species)) |> 
      filter(!(species == "Tar_sp" & plotID == "Lav_2_2" & year == 2021)) |> 
      bind_rows(tar_sp_lav_2_2_2021) |> 
      bind_rows(ver_alp_lav_2_2_2021) |> 
      bind_rows(agr_cap_lav_3_3_2021)
  )
)
