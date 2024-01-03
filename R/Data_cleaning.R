# Libraries and data files----

# First we get the data from OSF, this does not go into the final R script
library(osfr)
library(dataDownloader)
# osf_auth(token="...") Write in your own token
get_file(
  node = "zhk3m",
  file = "INCLINE_community_subplot_fixed.csv",
  remote_path = "Community",
  path ="data")

# Check that we have installed the following packages, and the versions according to the renv file
library(tidyverse)
library(turfmapper) # From GitHub: "Between-the-Fjords/turfmapper"
library(pipebind)

# File with community data
community_all <- read_csv("data/INCLINE_community_subplot_fixed.csv")
community <- filter(community_all, !treatment == "R") # We are not interested in the removal plots for this study

grid <- make_grid(ncol = 7, nrow = 5) # We create the 5x7 grid for the turfmapper

# Some individuals were a bit uncertain (suffix _cf in the file)----

levels(as.factor(grep("_cf$", community$species, value = TRUE)))
# Agr_cap_cf, Car_cap_cf, Car_nig_cf, Car_nor_cf, Epi_ana_cf, Ran_acr_cf and Vio_can_cf

find_plot_year(community, "Agr_cap_cf") # Lav_4_1 and Lav_5_2 in 2021, Gud_7_3 in 2023
turfplot(community, "Gud_7_3") # Seems it is indeed Agr_cap
turfplot(community, "Lav_4_1") # Seems it is indeed Agr_cap
turfplot(community, "Lav_5_2") # Seems it is indeed Agr_cap

find_plot_year(community, "Car_cap_cf") # Gud_4_3 and Gud_4_4 in 2021, Gud_6_3 in 2023
turfplot(community, "Gud_4_3") # Seems it is actually Car_fla
turfplot(community, "Gud_4_4") # Seems it is indeed Car_cap
turfplot(community, "Gud_6_3") # Seems it is indeed Car_cap

find_plot_year(community, "Car_nig_cf") # Skj_3_1 in 2023
turfplot(community, "Skj_3_1") # Seems it is actually Car_big

find_plot_year(community, "Car_nor_cf") # Lav_3_3 and Skj_1_1 in 2023
turfplot(community, "Lav_3_3") # Seems it is indeed Car_nor
turfplot(community, "Skj_1_1") # Seems it is actually Car_cap

find_plot_year(community, "Epi_ana_cf") # Lav_2_4
turfplot(community, "Lav_2_4") # Seems it is indeed Epi_ana

find_plot_year(community, "Ran_acr_cf") # Gud_2_2
turfplot(community, "Gud_2_2") # The scans says Rum_ace. But neither of the species grow in this block. I remove it

find_plot_year(community, "Vio_can_cf") # Ulv_7_4
turfplot(community, "Ulv_7_4") # Seems it is actually Vio_bif

community_cf <- community |> 
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
  
# For some individuals we know the genus but not the species (_sp)----

levels(as.factor(grep("_sp", community_cf$species, value = TRUE)))
# Alc_sp, Ant_sp, Car_sp, Epi_sp, Equ_sp, Eri_sp, Fes_sp, Gal_sp, Gen_sp, Hie_sp, Hyp_sp, Leo_sp, Oma_sp, Pyr_sp, Ran_sp, Rhi_sp, Sag_sp, Sal_sp, Sel_sp, Tar_sp, Tri_sp and Vio_sp
# Alc_sp and Tar_sp are species by themselves, we keep them. We check the rest

find_plot_year(community_cf, "Ant_sp") # Skj_6_3 in 2019
turfplot(community_cf, "Skj_6_3") # Not really possible to tell, I drop it

find_plot_year(community_cf, "Car_sp") |> print(n = 41) # Many cases. I create a file only with Carex
community_carex <- community_cf |> filter(grepl("Car_", species))
turfplot(community_carex, "Gud_1_2") # Seems it is Car_vag in 2018. In the other cases either the other species were already present in the subplot, or none appear any year, and then better to drop them
turfplot(community_carex, "Gud_1_3") # Seems it is Car_vag in all cases. No duplicates
turfplot(community_carex, "Gud_1_4") # Seems it is Car_vag in all cases. Duplicates with unique()
turfplot(community_carex, "Gud_1_5") # Seems it is Car_vag in all cases. Duplicate with unique()
turfplot(community_carex, "Gud_2_1") # Seems it is Car_big in all cases. Subplot1 must be removed
turfplot(community_carex, "Gud_2_2") # Seems it is Car_big in all cases. Car_sp in subplot17 2022 is cf, we remove the Car_big. Car_big in subplot34 2023 is D, we remove the Car_sp
turfplot(community_carex, "Gud_2_3") # Seems it is Car_big in all cases. Duplicates with unique()
turfplot(community_carex, "Gud_2_4") # Seems it is Car_big in all cases. Duplicate with unique()
turfplot(community_carex, "Gud_3_2") # Seems it is Car_big in 2018 and Car_vag in 2019. Duplicate with unique()
turfplot(community_carex, "Gud_3_3") # Seems it is Car_big in all cases. No duplicates
turfplot(community_carex, "Gud_3_5") # Seems it is Car_vag in all cases. No duplicates
turfplot(community_carex, "Gud_4_1") # All other Carex already present in that subplot. I drop it
turfplot(community_carex, "Gud_4_3") # All other Carex already present in those subplots. I drop them
turfplot(community_carex, "Gud_4_4") # All other Carex already present in those subplots. I drop them
turfplot(community_carex, "Gud_5_1") # Seems it is Car_cap (looking at the scan). No duplicates
turfplot(community_carex, "Gud_5_2") # All other Carex already present in that subplot. I drop it
turfplot(community_carex, "Gud_5_4") # Seems it is Car_big. No duplicate
turfplot(community_carex, "Gud_5_5") # Seems it is Car_vag in all cases. No duplicates
turfplot(community_carex, "Gud_7_2") # Car_big: 3, 12 (2018), 14, 16. Car_vag: 12 (2021), 21. Both: 8. Remove: 15, 35
carex_double <- filter(community, species == "Car_sp" & plotID == "Gud_7_2" & subPlot == 8 & year == 2018) # Create a duplicate
turfplot(community_carex, "Gud_7_3") # One Car_big (subplot 30). All other Carex in the other subplot
turfplot(community_carex, "Gud_7_4") # Seems it is Car_big in all cases. Duplicate with unique()
turfplot(community_carex, "Gud_7_6") # Car_vag: 1, 24, 31. Car_big: 5. Car_fla: 8. Remove: 2
turfplot(community_carex, "Lav_2_2") # Seems it is Car_pil. No duplicates
turfplot(community_carex, "Lav_4_3") # Seems it is Car_vag. Duplicate with unique()
turfplot(community_carex, "Skj_5_3") # Seems it is Car_cap. No duplicates
turfplot(community_carex, "Ulv_3_3") # It is probably Car_pal. No duplicates
turfplot(community_carex, "Ulv_3_5") # It is probably Car_big. No duplicates
turfplot(community_carex, "Ulv_6_3") # Not enough information to decide. I drop it

find_plot_year(community_cf, "Epi_sp") # Skj_2_5 and Gud_1_5 in 2018, Lav_5_3 and Skj_3_4 in 2021
turfplot(community_cf, "Gud_1_5")
filter(community_cf, grepl("Epi_",species) & grepl("Gud_1", plotID)) |> select(plotID, year, species)
turfplot(community_cf, "Lav_5_3") # It is Epi_ana
turfplot(community_cf, "Skj_2_5") # Seems it is Epi_ana
turfplot(community_cf, "Skj_3_4")
filter(community_cf, grepl("Epi_",species) & grepl("Skj_3", plotID)) |> select(plotID, year, species) |> print(n = 32)
# Gud_1_5 and Skj_3_4 are difficult to tell. But small Epilobium can be confounded with Veronica alpina. In both cases Ver_alp is found within the same subplot, so we remove them and edit the values of Ver_alp (if needed)

find_plot_year(community_cf, "Equ_sp") # Skj_5_2 in 2021
# I have checked the scan, this is actually Eup_sp, there was a typo
turfplot(community_cf, "Skj_5_2") # It is probably Eup_wet

find_plot_year(community_cf, "Eri_sp") # Skj_1_4 in 2018 and 2021
turfplot(community_cf, "Skj_1_4")
filter(community_cf, grepl("Eri_", species)) |> select(plotID, year, species) # We do not have enough information, we keep it as Eri_sp

find_plot_year(community_cf, "Fes_sp") # Ulv_1_4 in 2018, Gud_3_5 and Skj_2_6 in 2023
turfplot(community_cf, "Gud_3_5") # Seems it is Fes_rub
turfplot(community_cf, "Skj_2_6") # Difficult to tell
filter(community_cf, grepl("Fes_", species) & grepl("Skj_2", plotID)) |> select(plotID, year, species) # It is probably Fes_rub
turfplot(community_cf, "Ulv_1_4") # It is not Fes_rub
filter(community_cf, species == "Fes_ovi" & grepl("Ulv_", plotID)) # It is probably Fes_ovi
filter(community_cf, species == "Fes_viv" & grepl("Ulv_", plotID))

find_plot_year(community_cf, "Gal_sp") # Ulv_5_5 and Gud_3_6 in 2022, Gud_3_6 and Gud_6_4 in 2023
turfplot(community_cf, "Gud_3_6") # Difficult to tell
turfplot(community_cf, "Gud_6_4") # Difficult to tell
filter(community_cf, site == "Gudmedalen" & grepl("Gal_", species)) |> select(plotID, year, species) # We do not have enough information, we keep all in Gudmedalen as Gal_sp
turfplot(community_cf, "Ulv_5_5") # Seems it is Gal_bor

find_plot_year(community_cf, "Gen_sp") # Gud_7_3 in 2019
turfplot(community_cf, "Gud_7_3") # Seems it is Gen_niv

find_plot_year(community_cf, "Hie_sp") # Gud_4_3 in 2018; Gud_4_3, Gud_7_6, Ulv_1_4, Ulv_1_3 and Ulv_6_3 in 2019; Gud_4_1, Gud_4_3 and Ulv_6_4 in 2021; Skj_2_5 in 2022; Gud_7_3 in 2023
turfplot(community_cf, "Gud_4_1") # Difficult to tell
turfplot(community_cf, "Gud_4_3") # Difficult to tell
filter(community_cf, grepl("Hie", species) & grepl("Gud_4", plotID)) |> select(plotID, year, species) # Seems they might be Hie_pil, but only found in 2018
turfplot(community_cf, "Gud_7_3") # Difficult to tell
turfplot(community_cf, "Gud_7_6") # Difficult to tell
filter(community_cf, grepl("Hie", species) & species != "Hie_sp" & grepl("Gud_", plotID)) |> select(plotID, year, species) |> print(n = 44) # Not possible to say
turfplot(community_cf, "Skj_2_5") # Difficult to tell
filter(community_cf, grepl("Hie", species) & grepl("Skj_", plotID)) |> select(plotID, year, species) |> print(n = 33) # Not possible to say
turfplot(community_cf, "Ulv_1_3") # Difficult to tell
turfplot(community_cf, "Ulv_1_4") # Difficult to tell
turfplot(community_cf, "Ulv_6_3") # Difficult to tell
turfplot(community_cf, "Ulv_6_4") # Difficult to tell
filter(community_cf, grepl("Hie", species) & grepl("Ulv_", plotID)) |> select(plotID, year, species) |> print(n = 27) # Not possible to say
# It is best to keep it as _sp rather than remove it

find_plot_year(community_cf, "Hyp_sp") # Skj_3_3 in 2021
turfplot(community_cf, "Skj_3_3") # Seems it is Hyp_mac

find_plot_year(community_cf, "Leo_sp") # Ulv_6_1 in 2021
turfplot(community_cf, "Ulv_6_1") # Seems it is Leo_aut

find_plot_year(community_cf, "Oma_sp") # Lav_7_1 in 2021 and Skj_7_1 in 2023
turfplot(community_cf, "Lav_7_1") # Seems it is Oma_sup
turfplot(community_cf, "Skj_7_1") # Might be Oma_sup
filter(community_cf, grepl("Oma_", species) & grepl("Skj_7", plotID)) |> select(plotID, year, species) # It is probably Oma_sup

find_plot_year(community_cf, "Pyr_rot") # Many cases. Since it is difficult to distinguish Pyr_min and Pyr_rot, we group all of them under the name Pyr_sp

find_plot_year(community_cf, "Ran_sp") # Lav_2_3 in 2018
turfplot(community_cf, "Lav_2_3") # Seems it is Ran_pyg

find_plot_year(community_cf, "Rhi_sp") # Skj_1_4 in 2023
turfplot(community_cf, "Skj_1_4") # Difficult to tell
filter(community_cf, grepl("Rhi_", species) & grepl("Skj_1", plotID)) |> select(plotID, year, species) # It is probably Rhi_min

find_plot_year(community_cf, "Sag_sp") # Skj_4_1 in 2021
turfplot(community_cf, "Skj_4_1") # Seems it is Sag_sag

find_plot_year(community_cf, "Sal_sp") # Lav_2_2, Lav_3_3 and Gud_5_1 in 2021
turfplot(community_cf, "Gud_5_1") # Seems it is Sal_lan
turfplot(community_cf, "Lav_2_2") # Sal_sp does not exist in the scan. The values are not from another species
turfplot(community_cf, "Lav_3_3") # Sal_sp does not exist in the scan. The values are not from another species

find_plot_year(community_cf, "Sel_sp") # Skj_6_4 in 2019
turfplot(community_cf, "Skj_6_4") # Difficult to tell
filter(community_cf, grepl("Sel_", species) & grepl("Skj_6", plotID)) |> select(plotID, year, species) |> print(n = 22) # It is probably Sel_sel

find_plot_year(community_cf, "Tri_sp") # Lav_2_5 in 2021
turfplot(community_cf, "Lav_2_5") # Difficult to tell. The scan says Trifolium
filter(community_cf, grepl("Tri", species) & species != "Tri_ces" & site == "Lavisdalen") |> select(plotID, year, species) # It could be Tri_pra

find_plot_year(community_cf, "Vio_sp") # Ulv_1_5 in 2021
turfplot(community_cf, "Ulv_1_5") # It is probably Vio_bif

community_cf_sp <- community_cf |> 
  filter(species != "Ant_sp") |> 
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_1_2" & year == 2018, "Car_vag", species)) |> 
  filter(!(species == "Car_sp" & plotID == "Gud_1_2")) |>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_1_3", "Car_vag", species)) |> 
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_1_4", "Car_vag", species)) |> 
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_1_5", "Car_vag", species)) |>
  filter(!(species == "Car_sp" & plotID == "Gud_2_1" & subPlot == 1 & year == 2019)) |> 
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_2_1", "Car_big", species)) |> 
  filter(!(species == "Car_big" & plotID == "Gud_2_2" & year == 2022 & subPlot == 17)) |> 
  filter(!(species == "Car_sp" & plotID == "Gud_2_2" & year == 2023 & subPlot == 34)) |> 
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
  filter(!(plotID %in% c("Gud_1_5", "Skj_3_4") & species == "Epi_sp")) |> 
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

# For some individuals we do not know the species----

find_plot_year(community_cf_sp, "Ver_cha_eller_Hyp_mac") # Skj_2_1 in 2023
turfplot(community_cf_sp, "Skj_2_1") # After some discussion we have agreed this is Hyp_mac

find_plot_year(community_cf_sp, "Unknown") |> print(n = 21) # After looking at the scans, we might be able to fix: Lav_1_3, Lav_3_3 and Gud_5_5 in 2021. Cannot do anything about the rest
turfplot(community_cf_sp, "Lav_1_3") # The scans says Van_atr, it is Val_atr
turfplot(community_cf_sp, "Lav_3_3") # The scans says Suc_vul, it is Suc_pra
turfplot(community_cf_sp, "Gud_5_5") # The scans says Fjelljamne, it is Dip_alp

community_cf_sp_unknown <- community_cf_sp |> 
  mutate(species = ifelse(species == "Ver_cha_eller_Hyp_mac", "Hyp_mac", species)) |> 
  mutate(species = ifelse(species == "Unknown" & plotID == "Lav_1_3" & year == 2021, "Val_atr", species)) |> 
  mutate(species = ifelse(species == "Unknown" & plotID == "Lav_3_3" & year == 2021, "Suc_pra", species)) |> 
  mutate(species = ifelse(species == "Unknown" & plotID == "Gud_5_5" & year == 2021, "Dip_alp", species)) |> 
  filter(!(species %in% c("Nid_juvenile", "Nid_seedling", "Poaceae_sp", "Unknown"))) |> 
  unique()

# We correct some last few errors----

turfplot(community_cf_sp_unknown, "Lav_2_2") # There are a few mistakes in 2021: Ant_alp is actually Alc_alp, the values of Tar_sp belong to Bis_viv, Ver_alp is missing in subplot 1
# Since all values for Tar_sp are 1, we can create a tibble from the original one, choosing the correct subplots and changing the functional group and species
tar_sp_lav_2_2_2021 <- community_cf_sp_unknown |> 
  filter(plotID == "Lav_2_2" & year == 2021 & subPlot %in% c(8, 10, 19, 20, 26, 32, 33) & value == 1) |> 
  mutate(functional_group = "Forbs") |> 
  mutate(species = "Tar_sp") |> 
  unique()
ver_alp_lav_2_2_2021 <- community_cf_sp_unknown |> 
  filter(plotID == "Lav_2_2" & year == 2021 & subPlot == 1 & functional_group == "Forbs" & value == 1) |> 
  mutate(species = "Ver_alp") |> 
  unique()

turfplot(community_cf_sp_unknown, "Lav_3_3") # Agr_cap missing in subplot 10
agr_cap_lav_3_3_2021 <- community_cf_sp_unknown |> 
  filter(plotID == "Lav_3_3" & year == 2021 & subPlot == 10 & functional_group == "Graminoids" & value == 1) |> 
  mutate(species = "Agr_cap")

community_clean <- community_cf_sp_unknown |> 
  mutate(species = ifelse(species == "Ant_alp" & plotID == "Lav_2_2", "Alc_alp", species)) |> 
  filter(!(species == "Tar_sp" & plotID == "Lav_2_2" & year == 2021)) |> 
  bind_rows(tar_sp_lav_2_2_2021) |> 
  bind_rows(ver_alp_lav_2_2_2021) |> 
  bind_rows(agr_cap_lav_3_3_2021)

# There is one case of Pyr_rot with flower, Gud_7_4 in 2018. In 2023 it was called Pyr_rot, the other years Pyr_sp. I still call all Pyrola, Pyr_sp, but we can discuss