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

levels(as.factor(grep("_cf", community$species, value = TRUE)))
# Agr_cap_cf, Car_cap_cf, Car_nig_cf, Car_nor_cf, Epi_ana_cf, Ran_acr_cf and Vio_can_cf

find_plot_year(community, "Agr_cap_cf") # Lav_5_2 and Lav_4_1 in 2021, Gud_7_3 in 2023
turfplot(community, "Lav_5_2") # Seems it is indeed Agr_cap
turfplot(community, "Lav_4_1") # Seems it is indeed Agr_cap
turfplot(community, "Gud_7_3") # Seems it is indeed Agr_cap

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

community_clean_cf <- community |> 
  mutate(species = ifelse(species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Gud_4_3", "Car_fla", species)) |> 
  mutate(species = ifelse(species == "Car_cap_cf", "Car_cap", species)) |> 
  mutate(species = ifelse(species == "Car_nig_cf", "Car_big", species)) |> 
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_3_3", "Car_nor", species)) |> 
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Skj_1_1", "Car_cap", species)) |> 
  mutate(species = ifelse(species == "Epi_ana_cf", "Epi_ana", species)) |> 
  filter(!(species == "Ran_acr_cf")) |> 
  filter(!(species == "Ran_acr" & plotID == "Gud_2_2")) |> 
  mutate(species = ifelse(species == "Vio_can_cf", "Vio_bif", species))
  
# For some individuals the species is unknown (_sp)----

levels(as.factor(grep("_sp", community$species, value = TRUE)))
# Alc_sp, Ant_sp, Car_sp, Epi_sp, Equ_sp, Eri_sp, Fes_sp, Gal_sp, Gen_sp, Hie_sp, Hyp_sp, Leo_sp, Oma_sp, Pyr_sp, Ran_sp, Rhi_sp, Sag_sp, Sal_sp, Sel_sp, Tar_sp, Tri_sp and Vio_sp
# Alc_sp and Tar_sp are species by themselves, we keep them. We check the rest

find_plot_year(community, "Ant_sp") # Skj_6_3 in 2019
turfplot(community, "Skj_6_3") # Not really possible to tell, I drop it

find_plot_year(community, "Car_sp") # Skal se på den senere
turfplot(community, "Skj_6_3")

find_plot_year(community, "Epi_sp") # Skj_2_5 and Gud_1_5 in 2018, Lav_5_3 and Skj_3_4 in 2021
turfplot(community, "Skj_2_5") # Seems it is Epi_ana
turfplot(community, "Lav_5_3") # It is Epi_ana
turfplot(community, "Gud_1_5")
filter(community, grepl("Epi_",species) & grepl("Gud_1", plotID)) |> select(plotID, year, species)
turfplot(community, "Skj_3_4")
filter(community, grepl("Epi_",species) & grepl("Skj_3", plotID)) |> select(plotID, year, species) |> print(n = 32)
# The last two are difficult to tell. But small Epilobium can be confounded with Veronica alpina. In both cases Ver_alp is found within the same subplot, so we remove them and edit the values of Ver_alp (if needed)

find_plot_year(community, "Equ_sp") # Skj_5_2 in 2021
# I have checked the scan, this is actually Eup_sp, there was a typo
turfplot(community, "Skj_5_2") # It is probably Eup_wet

find_plot_year(community, "Eri_sp") # Skj_1_4 in 2018 and 2021
turfplot(community, "Skj_1_4")
filter(community, grepl("Eri_", species)) |> select(plotID, year, species) # We do not have enough information, we keep it as Eri_sp

find_plot_year(community, "Fes_sp") # Ulv_1_4 in 2018, Gud_3_5 and Skj_2_6 in 2023
turfplot(community, "Ulv_1_4") # It is not Fes_rub
filter(community, species == "Fes_ovi" & grepl("Ulv_", plotID)) # It is probably Fes_ovi
filter(community, species == "Fes_viv" & grepl("Ulv_", plotID))
turfplot(community, "Gud_3_5") # Seems it is Fes_rub
turfplot(community, "Skj_2_6") # Difficult to tell
filter(community, grepl("Fes_", species) & grepl("Skj_2", plotID)) |> select(plotID, year, species) # It is probably Fes_rub

find_plot_year(community, "Gal_sp") # Ulv_5_5 and Gud_3_6 in 2022, Gud_3_6 and Gud_6_4 in 2023
turfplot(community, "Ulv_5_5") # Seems it is Gal_bor
turfplot(community, "Gud_3_6") # Difficult to tell
turfplot(community, "Gud_6_4") # Difficult to tell
filter(community, site == "Gudmedalen" & grepl("Gal_", species)) |> select(plotID, year, species) # We do not have enough information, we keep all in Gudmedalen as Gal_sp

find_plot_year(community, "Gen_sp") # Gud_7_3 in 2019
turfplot(community, "Gud_7_3") # Seems it is Gen_niv

find_plot_year(community, "Hie_sp") # Gud_4_3 in 2018; Gud_4_3, Gud_7_6, Ulv_1_4, Ulv_1_3 and Ulv_6_3 in 2019; Gud_4_1, Gud_4_3 and Ulv_6_4 in 2021; Skj_2_5 in 2022; Gud_7_3 in 2023
turfplot(community, "Gud_4_1") # Difficult to tell
turfplot(community, "Gud_4_3") # Difficult to tell
filter(community, grepl("Gud_4", plotID) & grepl("Hie", species) & species != "Hie_sp") |> select(plotID, year, species) # Seems they might be Hie_pil, but only found in 2018
turfplot(community, "Gud_7_3") # Difficult to tell
turfplot(community, "Gud_7_6") # Difficult to tell
filter(community, grepl("Gud_", plotID) & grepl("Hie", species) & species != "Hie_sp") |> select(plotID, year, species) |> print(n = 44) # Not possible to say
turfplot(community, "Skj_2_5") # Difficult to tell
filter(community, grepl("Skj_", plotID) & grepl("Hie", species)) |> select(plotID, year, species) |> print(n = 33) # Not possible to say
turfplot(community, "Ulv_1_3")
turfplot(community, "Ulv_1_4")
filter(community, grepl("Ulv_", plotID) & grepl("Hie", species)) |> select(plotID, year, species) |> print(n = 27) # Not possible to say
turfplot(community, "Ulv_6_3") # Difficult to tell
turfplot(community, "Ulv_6_4") # Difficult to tell
filter(community, grepl("Ulv_", plotID) & grepl("Hie", species)) |> select(plotID, year, species) |> print(n = 27) # Not possible to say
# It is best to keep it as _sp rather than remove it

find_plot_year(community, "Hyp_sp") # Skj_3_3 in 2021
turfplot(community, "Skj_3_3") # Seems it is Hyp_mac

find_plot_year(community, "Leo_sp") # Ulv_6_1 in 2021
turfplot(community, "Ulv_6_1") # Seems it is Leo_aut

find_plot_year(community, "Oma_sp") # Lav_7_1 in 2021 and Skj_7_1 in 2023
turfplot(community, "Lav_7_1") # Seems it is Oma_sup
turfplot(community, "Skj_7_1") # Might be Oma_sup
filter(community, grepl("Oma_", species) & grepl("Skj_7", plotID)) |> select(plotID, year, species) # It is probably Oma_sup

find_plot_year(community, "Pyr_rot") # Many cases. Since it is difficult to distinguish Pyr_min and Pyr_rot, we group all of them under the name Pyr_sp

find_plot_year(community, "Ran_sp") # Lav_2_3 in 2018
turfplot(community, "Lav_2_3") # Seems it is Ran_pyg

find_plot_year(community, "Rhi_sp") # Skj_1_4 in 2023
turfplot(community, "Skj_1_4") # Difficult to tell
filter(community, grepl("Rhi_", species) & grepl("Skj_1", plotID)) |> select(plotID, year, species) # It is probably Rhi_min

find_plot_year(community, "Sag_sp") # Skj_4_1 in 2021
turfplot(community, "Skj_4_1") # Seems it is Sag_sag

find_plot_year(community, "Sal_sp") # Lav_2_2, Lav_3_3 and Gud_5_1 in 2021
turfplot(community, "Lav_2_2") # Sal_sp does not exist in the scan. The values are not from another species
turfplot(community, "Lav_3_3") # Sal_sp does not exist in the scan. The values are not from another species
turfplot(community, "Gud_5_1") # Seems it is Sal_lan

find_plot_year(community, "Sel_sp") # Skj_6_4 in 2019
turfplot(community, "Skj_6_4") # Difficult to tell
filter(community, grepl("Sel_", species) & grepl("Skj_6", plotID)) |> select(plotID, year, species) |> print(n = 22) # It is probably Sel_sel

find_plot_year(community, "Tri_sp") # Lav_2_5 in 2021
turfplot(community, "Lav_2_5") # Difficult to tell. The scan says Trifolium
filter(community, grepl("Tri", species) & species != "Tri_ces" & site == "Lavisdalen") |> select(plotID, year, species) # It could be Tri_pra

find_plot_year(community, "Vio_sp") # Ulv_1_5 in 2021
turfplot(community, "Ulv_1_5") # It is probably Vio_bif

community_clean_cf_sp <- community_clean_cf |> 
  filter(community_clean_cf, species != "Ant_sp")
  # Car_sp 
  mutate(species = ifelse(species == "Epi_sp" & (plotID == "Skj_2_5" | plotID == "Lav_5_3"), "Epi_ana", species)) |> 
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
  mutate(species = ifelse((species == "Pyr_min" | species == "Pyr_rot"), "Pyr_sp", species))
  mutate(species = ifelse(species == "Ran_sp", "Ran_pyg", species)) |> 
  mutate(species = ifelse(species == "Rhi_sp", "Rhi_min", species)) |> 
  mutate(species = ifelse(species == "Sag_sp", "Sag_sag", species)) |> 
  filter(!((plotID == "Lav_2_2" | plotID == "Lav_3_3") & species == "Sal_sp")) |> 
  mutate(species = ifelse(species == "Sal_sp" & plotID == "Gud_5_1", "Sal_lan", species))
  mutate(species = ifelse(species == "Sel_sp", "Sel_sel", species)) |>
  mutate(species = ifelse(species == "Tri_sp", "Tri_pra", species)) |> 
  filter(!species == "Vio_sp") |> 
  mutate(value = ifelse(plotID == "Ulv_1_5" & year = 2021 & (subPlot == 24 | subPlot == 29) & species == "Vio_bif", "1s", value)) |> 
  mutate(seedling = ifelse(plotID == "Ulv_1_5" & year = 2021 & (subPlot == 24 | subPlot == 29) & species == "Vio_bif", "TRUE", seedling))

# For some individuals we do not know the species----
  
  #mutate(species = ifelse(species == "Ver_cha", "Ver_alp", species)) |> 
  filter(!(species == "Nid_juvenile" | species == "Nid_seedling" | species == "Unknown" | species == "Poaceae_sp" | species == "Ver_cha_eller_Hyp_mac")) #earlier found Ver_off in the plot, but not same subplots. However, creeping stems?))

# Fixed according to what I wrote in the email: _cf, Car_sp, Epi_sp, Gen_sp, Hyp_sp, Leo_sp, Oma_sp, Ran_sp, Rhi_sp, Sag_sp, Sel_sp, Vio_sp, Nid_juvenile, Nid_seedling, Unknown, Poaceae_sp, Ver_cha_eller_Hyp_mac
# I keep Alc_sp, Eri_sp, Pyr_sp, Tar_sp

# Other errors:
# 2021 Lav_2_2 (Kari) Ant_alp is actually Alc_alp. The Tar_sp is wrong, has the values from Bis_viv, There is Ver_alp in subplot 1 - Cover 1
# 2021 Lav_3_3 (Kari) Agr_cap in subplot 10. Car_big makes no sense either in the scan or in the file. Unknown is Suc vul, or something like that. Missing Car_nor_cf (which is Car_nor)
# There is one case of Pyr_rot with flower, Gud_7_4 in 2018. In 2023 it was called Pyr_rot, the other years Pyr_sp. I still call all Pyrola Pyr_sp, but we can discuss

