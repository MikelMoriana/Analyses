# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse", "turfmapper", "pipebind"), 
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
    name = grid,
    command = make_grid(ncol = 7, nrow = 5) # We create the 5x7 grid for the turfmapper
  ),
  # Some individuals were a bit uncertain (suffix _cf in the file)----
  tar_target(
    name = levels_cf, # We find how many species had the suffix _cf
    command = levels(as.factor(grep("_cf$", community$species, value = TRUE))) # Agr_cap_cf, Car_cap_cf, Car_nig_cf, Car_nor_cf, Epi_ana_cf, Ran_acr_cf and Vio_can_cf
  ),
  tar_target(
    name = plot_year_Agr_cap_cf,
    command = find_plot_year(community, "Agr_cap_cf") # Lav_5_2 and Lav_4_1 in 2021, Gud_7_3 in 2023
  ),
  tar_target(
    name = turfplot_Gud_7_3,
    command = turfplot(community, "Gud_7_3") # Seems it is indeed Agr_cap
  ),
  tar_target(
    name = turfplot_Lav_4_1,
    command = turfplot(community, "Lav_4_1") # Seems it is indeed Agr_cap
  ),
  tar_target(
    name = turfplot_Lav_5_2,
    command = turfplot(community, "Lav_5_2") # Seems it is indeed Agr_cap
  ),
  tar_target(
    name = plot_year_Car_cap_cf,
    command = find_plot_year(community, "Car_cap_cf") # Gud_4_3 and Gud_4_4 in 2021, Gud_6_3 in 2023
  ),
  tar_target(
    name = turfplot_Gud_4_3,
    command = turfplot(community, "Gud_4_3") # Seems it is actually Car_fla
  ),
  tar_target(
    name = turfplot_Gud_4_4,
    command = turfplot(community, "Gud_4_4") # Seems it is indeed Car_cap
  ),
  tar_target(
    name = turfplot_Gud_6_3,
    command = turfplot(community, "Gud_6_3") # Seems it is indeed Car_cap
  ),
  tar_target(
    name = plot_year_Car_nig_cf,
    command = find_plot_year(community, "Car_nig_cf") # Skj_3_1 in 2023
  ),
  tar_target(
    name = turfplot_Skj_3_1,
    command = turfplot(community, "Skj_3_1") # Seems it is actually Car_big
  ),
  tar_target(
    name = plot_year_Car_nor_cf,
    command = find_plot_year(community, "Car_nor_cf") # Lav_3_3 and Skj_1_1 in 2023
  ),
  tar_target(
    name = turfplot_Lav_3_3,
    command = turfplot(community, "Lav_3_3") # Seems it is indeed Car_nor
  ),
  tar_target(
    name = turfplot_Skj_1_1,
    command = turfplot(community, "Skj_1_1") # Seems it is actually Car_cap
  ),
  tar_target(
    name = plot_year_Epi_ana_cf,
    command = find_plot_year(community, "Epi_ana_cf") # Lav_2_4
  ),
  tar_target(
    name = turfplot_Lav_2_4,
    command = turfplot(community, "Lav_2_4") # Seems it is indeed Epi_ana
  ),
  tar_target(
    name = plot_year_Ran_acr_cf,
    command = find_plot_year(community, "Ran_acr_cf") # Gud_2_2
  ),
  tar_target(
    name = turfplot_Gud_2_2,
    command = turfplot(community, "Gud_2_2") # The scans says Rum_ace. But neither of the species grow in this block. I remove it
  ),
  tar_target(
    name = plot_year_Vio_can_cf,
    command = find_plot_year(community, "Vio_can_cf") # Ulv_7_4
  ),
  tar_target(
    name = turfplot_Ulv_7_4,
    command = turfplot(community, "Ulv_7_4") # Seems it is actually Vio_bif
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
      mutate(species = ifelse(species == "Vio_can_cf", "Vio_bif", species)) |> 
      unique()
  ),
  # For some individuals we know the genus but not the species (_sp)----
  tar_target(
    name = levels_sp, # We find how many genera had the suffix _sp
    command = levels(as.factor(grep("_sp$", community$species, value = TRUE))) # Alc_sp, Ant_sp, Car_sp, Epi_sp, Equ_sp, Eri_sp, Fes_sp, Gal_sp, Gen_sp, Hie_sp, Hyp_sp, Leo_sp, Oma_sp, Pyr_sp, Ran_sp, Rhi_sp, Sag_sp, Sal_sp, Sel_sp, Tar_sp, Tri_sp and Vio_sp
    # Alc_sp and Tar_sp are species by themselves, we keep them. We check the rest
  ),
  tar_target(
    name = plot_year_Ant_sp,
    command = find_plot_year(community, "Ant_sp") # Skj_6_3 in 2019
  ),
  tar_target(
    name = turfplot_Skj_6_3,
    command = turfplot(community, "Skj_6_3") # Not really possible to tell, I drop it
  ),
  tar_target(
    name = plot_year_Car_sp,
    command = find_plot_year(community, "Car_sp") # Many cases. I create a file only with Carex
  ),
  tar_target(
    name = community_carex,
    command = community |> filter(grepl("Car_", species))
  ),
  tar_target(
    name = turfplot_Gud_1_2_car,
    command = turfplot(community_carex, "Gud_1_2") # Seems it is Car_vag in 2018. In the other cases either the other species were already present in the subplot, or none appear any year, and then better to drop them
  ),
  tar_target(
    name = turfplot_Gud_1_3_car,
    command = turfplot(community_carex, "Gud_1_3") # Seems it is Car_vag in all cases. No duplicates
  ),
  tar_target(
    name = turfplot_Gud_1_4_car,
    command = turfplot(community_carex, "Gud_1_4") # Seems it is Car_vag in all cases. Duplicates with unique()
  ),
  tar_target(
    name = turfplot_Gud_1_5_car,
    command = turfplot(community_carex, "Gud_1_5") # Seems it is Car_vag in all cases. Duplicate with unique()
  ),
  tar_target(
    name = turfplot_Gud_2_1_car,
    command = turfplot(community_carex, "Gud_2_1") # Seems it is Car_big in all cases. Subplot1 must be removed
  ),
  tar_target(
    name = turfplot_Gud_2_2_car,
    command = turfplot(community_carex, "Gud_2_2") # Seems it is Car_big in all cases. Subplot17 is cf, must be removed
  ),
  tar_target(
    name = turfplot_Gud_2_3_car,
    command = turfplot(community_carex, "Gud_2_3") # Seems it is Car_big in all cases. Duplicates with unique()
  ),
  tar_target(
    name = turfplot_Gud_2_4_car,
    command = turfplot(community_carex, "Gud_2_4") # Seems it is Car_big in all cases. Duplicate with unique()
  ),
  tar_target(
    name = turfplot_Gud_3_2_car,
    command = turfplot(community_carex, "Gud_3_2") # Seems it is Car_big in 2018 and Car_vag in 2019. Duplicate with unique()
  ),
  tar_target(
    name = turfplot_Gud_3_3_car,
    command = turfplot(community_carex, "Gud_3_3") # Seems it is Car_big in all cases. No duplicates
  ),
  tar_target(
    name = turfplot_Gud_3_5_car,
    command = turfplot(community_carex, "Gud_3_5") # Seems it is Car_vag in all cases. No duplicates
  ),
  tar_target(
    name = turfplot_Gud_4_1_car,
    command = turfplot(community_carex, "Gud_4_1") # All other Carex already present in that subplot. I drop it
  ),
  tar_target(
    name = turfplot_Gud_4_3_car,
    command = turfplot(community_carex, "Gud_4_3") # All other Carex already present in those subplots. I drop them
  ),
  tar_target(
    name = turfplot_Gud_4_4_car,
    command = turfplot(community_carex, "Gud_4_4") # All other Carex already present in those subplots. I drop them
  ),
  tar_target(
    name = turfplot_Gud_5_1_car,
    command = turfplot(community_carex, "Gud_5_1") # Seems it is Car_cap (looking at the scan). No duplicates
  ),
  tar_target(
    name = turfplot_Gud_5_2_car,
    command = turfplot(community_carex, "Gud_5_2") # All other Carex already present in that subplot. I drop it
  ),
  tar_target(
    name = turfplot_Gud_5_4_car,
    command = turfplot(community_carex, "Gud_5_4") # Seems it is Car_big. No duplicate
  ),
  tar_target(
    name = turfplot_Gud_5_5_car,
    command = turfplot(community_carex, "Gud_5_5") # Seems it is Car_vag in all cases. No duplicates
  ),
  tar_target(
    name = turfplot_Gud_7_2_car,
    command = turfplot(community_carex, "Gud_7_2") # Car_big: 3, 12 (2018), 14, 16. Car_vag: 12 (2021), 21. Both: 8. Remove: 15, 35
  ),
  tar_target(
    name = carex_double, # Create a duplicate, since it seems both Car_big and Car_vag are present in subplot 8 in 2018
    command = filter(community, species == "Car_sp" & plotID == "Gud_7_2" & subPlot == 8 & year == 2018)
  ),
  tar_target(
    name = turfplot_Gud_7_3_car,
    command = turfplot(community_carex, "Gud_7_3") # One Car_big (subplot 30). All other Carex in the other subplot
  ),
  tar_target(
    name = turfplot_Gud_7_4_car,
    command = turfplot(community_carex, "Gud_7_4") # Seems it is Car_big in all cases. Duplicate with unique()
  ),
  tar_target(
    name = turfplot_Gud_7_6_car,
    command = turfplot(community_carex, "Gud_7_6") # Car_vag: 1, 24, 31. Car_big: 5. Car_fla: 8. Remove: 2
  ),
  tar_target(
    name = turfplot_Lav_2_2_car,
    command = turfplot(community_carex, "Lav_2_2") # Seems it is Car_pil. No duplicates
  ),
  tar_target(
    name = turfplot_Lav_4_3_car,
    command = turfplot(community_carex, "Lav_4_3") # Seems it is Car_vag. Duplicate with unique()
  ),
  tar_target(
    name = turfplot_Skj_5_3_car,
    command = turfplot(community_carex, "Skj_5_3") # Seems it is Car_cap. No duplicates
  ),
  tar_target(
    name = turfplot_Ulv_3_3_car,
    command = turfplot(community_carex, "Ulv_3_3") # It is probably Car_pal. No duplicates
  ),
  tar_target(
    name = turfplot_Ulv_3_5_car,
    command = turfplot(community_carex, "Ulv_3_5") # It is probably Car_big. No duplicates
  ),
  tar_target(
    name = turfplot_Ulv_6_3_car,
    command = turfplot(community_carex, "Ulv_6_3") # Not enough information to decide. I drop it
  ),
  tar_target(
    name = plot_year_Epi_sp,
    command = find_plot_year(community, "Epi_sp") # Skj_2_5 and Gud_1_5 in 2018, Lav_5_3 and Skj_3_4 in 2021
  ),
  tar_target(
    name = turfplot_Gud_1_5,
    command = turfplot(community, "Gud_1_5") # Not enough to tell, might be Ver_alp
  ),
  tar_target(
    name = filtered_Epi_Gud_1,
    command = filter(community, grepl("Epi_",species) & grepl("Gud_1", plotID)) # Still difficult to tell
  ),
  tar_target(
    name = turfplot_Lav_5_3,
    command = turfplot(community, "Lav_5_3") # It is Epi_ana
  ),
  tar_target(
    name = turfplot_Skj_2_5,
    command = turfplot(community, "Skj_2_5") # Seems it is Epi_ana
  ),
  tar_target(
    name = turfplot_Skj_3_4,
    command = turfplot(community, "Skj_3_4") # Not enough to tell, might be Ver_alp
  ),
  tar_target(
    name = filtered_Epi_Skj_3,
    command = filter(community, grepl("Epi_",species) & grepl("Skj_3", plotID)) # Still difficult to tell
  ), # Gud_1_5 and Skj_3_4 are difficult to tell. But small Epilobium can be confounded with Veronica alpina. In both cases Ver_alp is found within the same subplot, so we remove them and edit the values of Ver_alp (if needed)
  tar_target(
    name = plot_year_Equ_sp,
    command = find_plot_year(community, "Equ_sp") # Skj_5_2 in 2021
  ),
  tar_target(
    name = turfplot_Skj_5_2,
    command = turfplot(community, "Skj_5_2") # Checked the scan, this is actually Eup_sp, there was a typo. It is probably Eup_wet
  ),
  tar_target(
    name = plot_year_Eri_sp,
    command = find_plot_year(community, "Eri_sp") # Skj_1_4 in 2018 and 2021
  ),
  tar_target(
    name = turfplot_Skj_1_4,
    command = turfplot(community, "Skj_1_4") # Not enough to tell
  ),
  tar_target(
    name = filtered_Eri,
    command = filter(community, grepl("Eri_", species)) # We do not have enough information, we keep it as Eri_sp
  ),
  tar_target(
    name = plot_year_Fes_sp,
    command = find_plot_year(community, "Fes_sp") # Ulv_1_4 in 2018, Gud_3_5 and Skj_2_6 in 2023
  ),
  tar_target(
    name = turfplot_Gud_3_5,
    command = turfplot(community, "Gud_3_5") # Seems it is Fes_rub
  ),
  tar_target(
    name = turfplot_Skj_2_6,
    command = turfplot(community, "Skj_2_6") # Difficult to tell
  ),
  tar_target(
    name = filtered_Fes_Skj_2,
    command = filter(community, grepl("Fes_", species) & grepl("Skj_2", plotID)) |> select(plotID, year, species) # It is probably Fes_rub
  ),
  tar_target(
    name = turfplot_Ulv_1_4,
    command = turfplot(community, "Ulv_1_4") # It is not Fes_rub
  ),
  tar_target(
    name = filtered_Fes_ovi_UlV,
    command = filter(community, species == "Fes_ovi" & grepl("Ulv_", plotID)) # It is probably Fes_ovi
  ),
  tar_target(
    name = filtered_Fes_viv_Ulv,
    command = filter(community, species == "Fes_viv" & grepl("Ulv_", plotID))
  ),
  tar_target(
    name = plot_year_Gal_sp,
    command = find_plot_year(community, "Gal_sp") # Ulv_5_5 and Gud_3_6 in 2022, Gud_3_6 and Gud_6_4 in 2023
  ),
  tar_target(
    name = turfplot_Gud_3_6,
    command = turfplot(community, "Gud_3_6") # Difficult to tell
  ),
  tar_target(
    name = turfplot_Gud_6_4,
    command = turfplot(community, "Gud_6_4") # Difficult to tell
  ),
  tar_target(
    name = filtered_Gal_Gud,
    command = filter(community, grepl("Gal_", species) & grepl("Gud_", plotID)) # We do not have enough information, we keep all in Gudmedalen as Gal_sp
  ),
  tar_target(
    name = turfplot_Ulv_5_5,
    command = turfplot(community, "Ulv_5_5") # Seems it is Gal_bor
  ),
  tar_target(
    name = plot_year_Gen_sp,
    command = find_plot_year(community, "Gen_sp") # Gud_7_3 in 2019
  ),
  # We already have a turfplot for Gud_7_3 (from Agr_cap_cf). Seems it is Gen_niv for Gen_sp
  tar_target(
    name = plot_year_Hie_sp,
    command = find_plot_year(community, "Hie_sp") # Gud_4_3 in 2018; Gud_4_3, Gud_7_6, Ulv_1_4, Ulv_1_3 and Ulv_6_3 in 2019; Gud_4_1, Gud_4_3 and Ulv_6_4 in 2021; Skj_2_5 in 2022; Gud_7_3 in 2023
  ),
  tar_target(
    name = turfplot_Gud_4_1,
    command = turfplot(community, "Gud_4_1") # Difficult to tell
  ),
  # We already have a turfplot for Gud_4_3 (from Car_cap_cf). Difficult to tell for Hie_sp
  tar_target(
    name = filtered_Hie_Gud_4,
    command = filter(community, grepl("Gud_4", plotID) & grepl("Hie", species)) # Seems it might be Hie_pil, but only found in 2018
  ),
  # We already have a turfplot for Gud_7_3 (from Agr_cap_cf). Difficult to tell for Hie_sp
  tar_target(
    name = turfplot_Gud_7_6,
    command = turfplot(community, "Gud_7_6") # Difficult to tell
  ),
  tar_target(
    name = filtered_Hie_Gud,
    command = filter(community, grepl("Hie", species) & grepl("Gud_", plotID) & species != "Hie_sp") # Not possible to say
  ),
  # We already have a turfplot for Skj_2_5 (from Epi_sp). Difficult to tell for Hie_sp
  tar_target(
    name = filtered_Hie_Skj,
    command = filter(community, grepl("Hie", species) & grepl("Skj_", plotID)) # Not possible to say
  ),
  tar_target(
    name = turfplot_Ulv_1_3,
    command = turfplot(community, "Ulv_1_3") # Difficult to tell
  ),
  # We already have a turfplot for Ulv_1_4 (from Fes_sp). Difficult to tell for Hie_sp
  tar_target(
    name = turfplot_Ulv_6_3,
    command = turfplot(community, "Ulv_6_3") # Difficult to tell
  ),
  tar_target(
    name = turfplot_Ulv_6_4,
    command = turfplot(community, "Ulv_6_4") # Difficult to tell
  ),
  tar_target(
    name = filtered_Hie_Ulv,
    command = filter(community, grepl("Hie", species) & grepl("Ulv_", plotID)) # Not possible to say
    # It is best to keep it as _sp rather than remove it
  ),
  tar_target(
    name = plot_year_Hyp_sp,
    command = find_plot_year(community, "Hyp_sp") # Skj_3_3 in 2021
  ),
  tar_target(
    name = turfplot_Skj_3_3,
    command = turfplot(community, "Skj_3_3") # Seems it is Hyp_mac
  ),
  tar_target(
    name = plot_year_Leo_sp,
    command = find_plot_year(community, "Leo_sp") # Ulv_6_1 in 2021
  ),
  tar_target(
    name = turfplot_Ulv_6_1,
    command = turfplot(community, "Ulv_6_1") # Seems it is Leo_aut
  ),
  tar_target(
    name = plot_year_Oma_sp,
    command = find_plot_year(community, "Oma_sp") # Lav_7_1 in 2021 and Skj_7_1 in 2023
  ),
  tar_target(
    name = turfplot_Lav_7_1,
    command = turfplot(community, "Lav_7_1") # Seems it is Oma_sup
  ),
  tar_target(
    name = turfplot_Skj_7_1,
    command = turfplot(community, "Skj_7_1") # Might be Oma_sup
  ),
  tar_target(
    name = filtered_Oma_Skj_7,
    command = filter(community, grepl("Oma_", species) & grepl("Skj_7", plotID)) # It is probably Oma_sup
  ),
  tar_target(
    name = plot_year_Pyr_sp,
    command = find_plot_year(community, "Pyr_rot") # Many cases. Since it is difficult to distinguish Pyr_min and Pyr_rot, we group all of them under the name Pyr_sp
  ),
  tar_target(
    name = plot_year_Ran_sp,
    command = find_plot_year(community, "Ran_sp") # Lav_2_3 in 2018
  ),
  tar_target(
    name = turfplot_Lav_2_3,
    command = turfplot(community, "Lav_2_3") # Seems it is Ran_pyg
  ),
  tar_target(
    name = plot_year_Rhi_sp,
    command = find_plot_year(community, "Rhi_sp") # Skj_1_4 in 2023
  ),
  # We already have a turfplot for Skj_1_4 (from Eri_sp). Difficult to tell for Rhi_sp
  tar_target(
    name = filtered_Rhi_Skj_1,
    command = filter(community, grepl("Rhi_", species) & grepl("Skj_1", plotID)) # It is probably Rhi_min
  ),
  tar_target(
    name = plot_year_Sag_sp,
    command = find_plot_year(community, "Sag_sp") # Skj_4_1 in 2021
  ),
  tar_target(
    name = turfplot_Skj_4_1,
    command = turfplot(community, "Skj_4_1") # Seems it is Sag_sag
  ),
  tar_target(
    name = plot_year_Sal_sp,
    command = find_plot_year(community, "Sal_sp") # Lav_2_2, Lav_3_3 and Gud_5_1 in 2021
  ),
  tar_target(
    name = turfplot_Gud_5_1,
    command = turfplot(community, "Gud_5_1") # Seems it is Sal_lan
  ),
  tar_target(
    name = turfplot_Lav_2_2,
    command = turfplot(community, "Lav_2_2") # Sal_sp does not exist in the scan. The values are not from another species
  ),
  # We already have a turfplot for Lav_3_3 (from Car_nor_cf). Sal_sp does not exist in the scan. The values are not from another species. I remove it
  tar_target(
    name = plot_year_Sel_sp,
    command = find_plot_year(community, "Sel_sp") # Skj_6_4 in 2019
  ),
  tar_target(
    name = turfplot_Skj_6_4,
    command = turfplot(community, "Skj_6_4") # Difficult to tell
  ),
  tar_target(
    name = filtered_Sel_Skj_6,
    command = filter(community, grepl("Sel_", species) & grepl("Skj_6", plotID)) # It is probably Sel_sel
  ),
  tar_target(
    name = plot_year_Tri_sp,
    command = find_plot_year(community, "Tri_sp") # Lav_2_5 in 2021
  ),
  tar_target(
    name = turfplot_Lav_2_5,
    command = turfplot(community, "Lav_2_5") # Difficult to tell. The scan says Trifolium
  ),
  tar_target(
    name = filtered_Tri_Lav,
    command = filter(community, grepl("Tri", species) & species != "Tri_ces" & grepl("Lav_", plotID)) # It could be Tri_pra
  ),
  tar_target(
    name = plot_year_Vio_sp,
    command = find_plot_year(community, "Vio_sp") # Ulv_1_5 in 2021
  ),
  tar_target(
    name = turfplot_Ulv_1_5,
    command = turfplot(community, "Ulv_1_5") # It is probably Vio_bif
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
  # For some individuals we do not know the species----
  tar_target(
    name = plot_year_Ver_cha_eller_Hyp_mac,
    command = find_plot_year(community, "Ver_cha_eller_Hyp_mac") # Skj_2_1 in 2023
  ),
  tar_target(
    name = turfplot_Skj_2_1,
    command = turfplot(community, "Skj_2_1") # After some discussion we have agreed this is Hyp_mac
  ),
  tar_target(
    name = plot_year_Unknown,
    command = find_plot_year(community, "Unknown") # After looking at the scans, we might be able to fix: Gud_5_5, Lav_1_3 and Lav_3_3 in 2021. Cannot do anything about the rest
  ),
  tar_target(
    name = turfplot_Gud_5_5,
    command = turfplot(community, "Gud_5_5") # The scans says Fjelljamne, it is Dip_alp
  ),
  tar_target(
    name = turfplot_Lav_1_3,
    command = turfplot(community, "Lav_1_3") # The scans says Van_atr, it is Val_atr
  ),
  # We already have a turfplot for Lav_3_3 (from Car_nor_cf). The scans says Suc_vul, it is Suc_pra
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
  # We correct some last few errors----
  # Lav_2_2 has some erros. We already have a turfplot for it from Sal_sp. There are a few mistakes in 2021: Ant_alp is actually Alc_alp, the values of Tar_sp belong to Bis_viv (we need to write the correct ones), Ver_alp is missing in subplot 1
  # Since all values for Tar_sp are 1, we can create a tibble from the original one, choosing the correct subplots and changing the functional group and species
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
  # We already have a turfplot for Lav_3_3 (from Car_nor_cf). Agr_cap missing in subplot 10
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
