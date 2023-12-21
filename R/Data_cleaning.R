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
turfplot(community, "Lav_3_3") # Seems it is actually Car_big
turfplot(community, "Skj_1_1") # Seems it is actually Car_cap

find_plot_year(community, "Epi_ana_cf") # Lav_2_4
turfplot(community, "Lav_2_4") # Seems it is indeed Epi_ana

find_plot_year(community, "Ran_acr_cf") # Gud_2_2
turfplot(community, "Gud_2_2") # The scans says Rum_ace. But neither of the species grow in this block. I remove it

find_plot_year(community, "Vio_can_cf") # Ulv_7_4
turfplot(community, "Ulv_7_4") # Seems it is actually Vio_bif

community_clean |> 
  mutate(species = ifelse(species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Gud_4_3", "Car_fla", species)) |> 
  mutate(species = ifelse(species == "Car_cap_cf", "Car_cap", species)) |> 
  mutate(species = ifelse(species == "Car_nig_cf", "Car_big", species)) |> 
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_3_3", "Car_big", species)) |> 
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Skj_1_1", "Car_cap", species)) |> 
  mutate(species = ifelse(species == "Epi_ana_cf", "Epi_ana", species)) |> 
  filter(!(species == "Ran_acr_cf")) |> 
  filter(!(species == "Ran_acr" & plotID == "Gud_2_2")) |> 
  mutate(species = ifelse(species == "Vio_can_cf", "Vio_bif", species))
  
# For some individuals the species is unknown (_sp)----

community_clean <- community |> 
  #filter(!species == "Ant_sp") |> Maybe best to drop it
  filter(!species == "Car_sp") |> 
  mutate(species = ifelse(species == "Epi_sp" & (plotID == "Skj_2_5" | plotID == "Skj_3_4" | plotID == "Lav_5_3"), "Epi_ana", species)) |> 
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Gud_1_5", "Ver_alp", species)) |> 
  #mutate(species = ifelse(species == "Equ_sp", "Equ_arv", species)) |> Have the feeling it is small arvense
  #filter(!species == "Fes_sp") |> Might be best to drop it
  #mutate(species = ifelse(species == "Gal_sp" & plotID == "Ulv_5_5", "Gal_bor", species) It is probably Gal_bor
  # Gal_sp Gudmedalen. Maybe best leave it as a species
  mutate(species = ifelse(species == "Gen_sp", "Gen_niv", species)) |> 
  #Hie_sp Maybe best to leave it, but very unsure
  mutate(species = ifelse(species == "Hyp_sp", "Hyp_mac", species)) |> 
  mutate(species = ifelse(species == "Leo_sp", "Leo_aut", species)) |> 
  mutate(species = ifelse(species == "Oma_sp", "Oma_sup", species)) |> 
  mutate(species = ifelse(species == "Ran_sp", "Ran_pyg", species)) |> 
  mutate(species = ifelse(species == "Rhi_sp", "Rhi_min", species)) |> 
  mutate(species = ifelse(species == "Sag_sp", "Sag_sag", species)) |> 
  #filter(!((plotID == "Lav_2_2" | plotID == "Lav_3_3") & species == "Sal_sp")) |> Drop these ones
  #mutate(species = ifelse(species == "Sal_sp" & plotID == "Gud_5_1", "Sal_gla", species))
  mutate(species = ifelse(species == "Sel_sp", "Sel_sel", species)) |> 
  filter(!species == "Vio_sp") |> 
  #mutate(species = ifelse(species == "Ver_cha", "Ver_alp", species)) |> 
  filter(!(species == "Nid_juvenile" | species == "Nid_seedling" | species == "Unknown" | species == "Poaceae_sp" | species == "Ver_cha_eller_Hyp_mac")) #earlier found Ver_off in the plot, but not same subplots. However, creeping stems?))

# Fixed according to what I wrote in the email: _cf, Car_sp, Epi_sp, Gen_sp, Hyp_sp, Leo_sp, Oma_sp, Ran_sp, Rhi_sp, Sag_sp, Sel_sp, Vio_sp, Nid_juvenile, Nid_seedling, Unknown, Poaceae_sp, Ver_cha_eller_Hyp_mac
# I keep Alc_sp, Eri_sp, Pyr_sp, Tar_sp


