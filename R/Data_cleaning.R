# Libraries and data files----

# First we get the data from OSF, this does not go into the final R script
#library(osfr)
#library(dataDownloader)
# osf_auth(token="...") Write in your own token
#get_file(
#  node = "zhk3m",
#  file = "INCLINE_community_subplot_fixed.csv",
#  remote_path = "Community",
#  path = "data"
#)

# Check that we have installed the following packages, and the versions according to the renv file
library(tidyverse)
library(turfmapper) # From GitHub: "Between-the-Fjords/turfmapper"
library(pipebind)

# File with community data
community_all <- read_csv("data/INCLINE_community_subplot_fixed.csv")
community <- filter(community_all, !treatment == "R") # We are not interested in the removal plots for this study

grid <- make_grid(ncol = 7, nrow = 5) # We create the 5x7 grid for the turfmapper

# In a few cases the subplot number is wrong (there shouldn't be anything in subplots 9, 11, 13, 15, 17, 19)----

# We check the turfplot and the scans to decide what we do with them
community |> 
  filter(subPlot %in% c(9, 11, 13, 23, 25, 27)) |> 
  select(plotID, year) |> 
  unique() |> 
  print(n = Inf)

# Skj_7_1, 2018
find_subplot_species(community, "Skj_7_1", 2018) # Alc_alp 23
turfplot_year(community, "Skj_7_1", 2018) # We remove it

# Skj_3_1  2019
find_subplot_species(community, "Skj_3_1", 2019) # Vio_pal 13
turfplot_year(community, "Skj_3_1", 2019) # It's subplot 14, not 13

# Skj_6_6  2019
find_subplot_species(community, "Skj_6_6", 2019) # Sal_her 9, 11, 13. Oma_sup 13
turfplot_year(community, "Skj_6_6", 2019) # Sal_her has been shifted one to the left (9 for 10, 11 for 12, 13 for 14), and is missing in 7. We remove Oma_sup
sal_her_skj_6_6_7_2019 <- community |>
  filter(plotID == "Skj_6_6" & year == 2019 & subPlot == 7 & species == "Ant_odo") |>
  mutate(species = "Sal_her",
         functional_group = "Deciduous_shrubs",
         value = "D",
         dominance = "D")

# Skj_7_1  2019
find_subplot_species(community, "Skj_7_1", 2019) # Sib_pro 9, 11, 13, 23, 25, 27
turfplot_year(community, "Skj_7_1", 2019) # Sib_pro also counted in transplant subplots. This was for another study. For this one we remove it

# Skj_7_5  2019
find_subplot_species(community, "Skj_7_5", 2019) # Sib_pro 11, 13, 23, 25, 27. Ver_alp 13
turfplot_year(community, "Skj_7_5", 2019) # Sib_pro also counted in transplant subplots. This was for another study. For this one we remove it. Ver_alp was in fact registered in subplot 13. We check other years
turfplot(community, "Skj_7_5") # No Ver_alp near subplot 13, we remove it

# Gud_1_2  2019
find_subplot_species(community, "Gud_1_2", 2019) # Des_ces 23
turfplot_year(community, "Gud_1_2", 2019) # We remove it

# Gud_1_3  2019
find_subplot_species(community, "Gud_1_3", 2019) # Pot_ere 27
turfplot_year(community, "Gud_1_3", 2019) # It's subplot 26 (cross-out something in 26, had to write in 27)

# Gud_1_4  2019
find_subplot_species(community, "Gud_1_4", 2019) # Ast_alp 9
turfplot_year(community, "Gud_1_4", 2019) # It's subplot 8, not 9

# Gud_2_2  2019
find_subplot_species(community, "Gud_2_2", 2019) # Nid_seedling 9. Nid_seedling is eventually removed, so we don't do anything

# Gud_7_1  2019
find_subplot_species(community, "Gud_7_1", 2019) # Tha_alp 9, 11, 13
turfplot_year(community, "Gud_7_1", 2019) # They're shifted to the right. And 7 is actually 14

# Lav_2_2  2021
find_subplot_species(community, "Lav_2_2", 2021) # Vio_bif 9, 11
turfplot_year(community, "Lav_2_2", 2021) # They're shifted to the left. And 14 is actually 16

# Lav_2_5  2021
find_subplot_species(community, "Lav_2_5", 2021) # Agr_cap 9, 11, 13
turfplot_year(community, "Lav_2_5", 2021) # They're shifted to the left, and I delete 13
# We found out that moss, lichen and litter are shifted one column for the whole plot

# Lav_2_6  2021
find_subplot_species(community, "Lav_2_6", 2021) # Cer_cer 9
turfplot_year(community, "Lav_2_6", 2021) # 9 is actually 10. And 3 and 4 are also shifted (they're 4 and 5)
# We found out that Vac_myr was not typed in. Luckily, Agr_cap is found in all the plots were Vac_myr exists, so we use it
vac_myr_lav_2_6_2021 <- community |>
  filter(plotID == "Lav_2_6" & year == 2021 & subPlot %in% c(3, 4, 5, 10, 19, 21, 24, 29, 32) & species == "Agr_cap") |> 
  mutate(species = "Vac_myr",
         functional_group = "Deciduous_shrubs")

# Lav_5_3  2021
find_subplot_species(community, "Lav_5_3", 2021) # Agr_cap 13
turfplot_year(community, "Lav_5_3", 2021) # I delete 13. We're missing 16 and 17. And we are missing moss, lichen... for all
agr_cap_lav_5_3_2021 <- community |>
  filter(plotID == "Lav_5_3" & year == 2021 & subPlot %in% c(16, 17) & species == "Epi_ana") |> 
  mutate(species = "Agr_cap",
         functional_group = "Graminoids")

# Lav_7_1  2021
find_subplot_species(community, "Lav_7_1", 2021) # Sib_pro 9
turfplot_year(community, "Lav_7_1", 2021) # 9 is actually 8

# Lav_7_3  2021
find_subplot_species(community, "Lav_7_3", 2021) # Ant_odo 13
turfplot_year(community, "Lav_7_3", 2021) # Some columns have shifted left: I change 13 to 15 and 16 to 18

# Skj_1_3  2021
find_subplot_species(community, "Skj_1_3", 2021) # Agr_cap 11
turfplot_year(community, "Skj_1_3", 2021) # 11 is 12, and 14 is 15. 17 should be empty (only logger)

# Skj_1_5  2021
find_subplot_species(community, "Skj_1_5", 2021) # Tha_alp 23, 25, 27
turfplot_year(community, "Skj_1_5", 2021) # Have been shifter to the right. 23 is 22, 25 is 24 and 27 is 26

# Skj_4_3  2021
find_subplot_species(community, "Skj_4_3", 2021) # Cam_rot 23, 25
turfplot_year(community, "Skj_4_3", 2021) # We are missing 19 and 24, we change them

# Gud_2_2  2021
find_subplot_species(community, "Gud_2_2", 2021) # Alc_alp and Ave_fle 23
turfplot_year(community, "Gud_2_2", 2021) # Alc_alp: 20 for 23. Ave_fle: 24 for 23, 19 for 22

# Gud_3_5  2021
find_subplot_species(community, "Gud_3_5", 2021) # Vio_bif 9
turfplot_year(community, "Gud_3_5", 2021) # We remove it

# Gud_4_4  2021
find_subplot_species(community, "Gud_4_4", 2021) # Nar_str 13
turfplot_year(community, "Gud_4_4", 2021) # Some columns have shifted right: 13 for 12, 17 for 14. We create 18
nar_str_gud_4_4_18_2021 <- community |>
  filter(plotID == "Gud_4_4" & year == 2021 & subPlot == 18 & species == "Car_big") |> 
  mutate(species = "Nar_str")

# Gud_5_2  2021
find_subplot_species(community, "Gud_5_2", 2021) # Many species. Looking at the scan, they were also recorded in these subplots
turfplot_year(community, "Gud_5_2", 2021) # We remove them

# Gud_5_4  2021
find_subplot_species(community, "Gud_5_4", 2021) # Many species. Looking at the scan, they were also recorded in these subplots
turfplot_year(community, "Gud_5_4", 2021) # We remove them

# Ulv_1_1  2021
find_subplot_species(community, "Ulv_1_1", 2021) # Agr_cap 13
turfplot_year(community, "Ulv_1_1", 2021) # We remove it

# Ulv_3_4  2021
find_subplot_species(community, "Ulv_3_4", 2021) # Many species. Looking at the scan, they were also recorded in subplot 9
turfplot_year(community, "Ulv_3_4", 2021) # We remove it

# Ulv_6_4  2022
find_subplot_species(community, "Ulv_6_4", 2022) # Eup_wet 11
turfplot_year(community, "Ulv_6_4", 2022) # It's 10, not 11

# Ulv_7_2  2022
find_subplot_species(community, "Ulv_7_2", 2022) # Bis_viv and Tar_sp 25
turfplot_year(community, "Ulv_7_2", 2022) # Bis_viv: it's not 25, but 24. Tar_sp is wrong, I write it again
tar_sp_ulv_7_2_2022 <- community |> 
  filter( plotID == "Ulv_7_2" & year == 2022 & subPlot %in% c(10, 12, 16, 17, 18, 24) & species == "Agr_cap") |> 
  mutate(species = "Tar_sp", 
         functional_group = "Forbs") |> 
  mutate(value = ifelse(subPlot == 12, 1, value), 
         fertile = ifelse(subPlot == 12, FALSE, fertile)) |> 
  mutate(value = ifelse(subPlot == 18, "J", value), 
         juvenile = ifelse(subPlot == 18, TRUE, juvenile))

# Skj_6_6  2022
find_subplot_species(community, "Skj_6_6", 2022) # Alc_alp 25
turfplot_year(community, "Skj_6_6", 2022) # It's 24, not 25

# Gud_4_6  2023
find_subplot_species(community, "Gud_4_6", 2023) # Tha_alp 9, 11
turfplot_year(community, "Gud_4_6", 2023) # They've been shifted left. 9 is 10 and 11 is 12

# Ulv_2_1  2023
find_subplot_species(community, "Ulv_2_1", 2023) # Nar_str 27
turfplot_year(community, "Ulv_2_1", 2023) # 27 is 28

# Lav_3_6  2021
find_subplot_species(community, "Lav_3_6", 2021) # Phl_alp 27 27
turfplot_year(community, "Lav_3_6", 2021) # We remove it

community_subplot <- community |> 
  filter(!(plotID == "Skj_7_1" & year == 2018 & subPlot == 23)) |> 
  mutate(subPlot = ifelse(plotID == "Skj_3_1" & year == 2019 & subPlot == 13, 14, subPlot), 
         moss = ifelse(plotID == "Skj_3_1" & year == 2019 & subPlot == 14, 70, moss), 
         litter = ifelse(plotID == "Skj_3_1" & year == 2019 & subPlot == 14, 5, litter), 
         bare_ground = ifelse(plotID == "Skj_3_1" & year == 2019 & subPlot == 14, 2, bare_ground)) |> 
  mutate(subPlot = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 9, 10, subPlot), 
         moss = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 10, 25, moss), 
         litter = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 10, 30, litter), 
         vegetation_height_mm = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 10, 60, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 10, 15, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 11, 12, subPlot), 
         moss = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 12, 20, moss), 
         lichen = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 12, 3, lichen), 
         litter = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 12, 50, litter), 
         vegetation_height_mm = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 12, 65, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 12, 19, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 13 & species == "Sal_her", 14, subPlot), 
         moss = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 14, 10, moss), 
         lichen = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 14, 25, lichen), 
         litter = ifelse(plotID == "Skj_6_6" & year == 2019 & subPlot == 14, 25, litter)) |> 
  bind_rows(sal_her_skj_6_6_7_2019) |> 
  filter(!(plotID == "Skj_6_6" & year == 2019 & subPlot == 13 & species == "Oma_sup")) |> 
  filter(!(plotID == "Skj_7_1" & year == 2019 & subPlot %in% c(9, 11, 13, 23, 25, 27))) |> 
  filter(!(plotID == "Skj_7_5" & year == 2019 & subPlot %in% c(11, 13, 23, 25, 27))) |> 
  filter(!(plotID == "Gud_1_2" & year == 2019 & subPlot == 23)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_1_3" & year == 2019 & subPlot == 27, 26, subPlot), 
         moss = ifelse(plotID == "Gud_1_3" & year == 2019 & subPlot == 26, 1, moss), 
         litter = ifelse(plotID == "Gud_1_3" & year == 2019 & subPlot == 26, 40, litter), 
         vegetation_height_mm = ifelse(plotID == "Gud_1_3" & year == 2019 & subPlot == 26, 120, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Gud_1_3" & year == 2019 & subPlot == 26, 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_1_4" & year == 2019 & subPlot == 9, 8, subPlot), 
         moss = ifelse(plotID == "Gud_1_4" & year == 2019 & subPlot == 8, 1, moss), 
         litter = ifelse(plotID == "Gud_1_4" & year == 2019 & subPlot == 8, 20, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 9, 8, subPlot), 
         moss = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 8, 15, moss), 
         litter = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 8, 50, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 11, 10, subPlot), 
         moss = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 10, 5, moss), 
         litter = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 10, 30, litter), 
         bare_ground = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 10, 15, bare_ground), 
         vegetation_height_mm = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 10, 90, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 10, 20, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 13, 12, subPlot), 
         litter = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 12, 25, litter), 
         bare_ground = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 12, 10, bare_ground), 
         logger = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 12, 10, logger), 
         vegetation_height_mm = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 12, 100, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 12, 25, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 7 & species == "Tha_alp", 14, subPlot), 
         litter = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 14, 25, litter), 
         bare_ground = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 14, 15, bare_ground)) |> 
  mutate(subPlot = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 9, 10, subPlot), 
         moss = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 10, 40, moss), 
         litter = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 10, 20, litter), 
         logger = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 10, 40, logger), 
         vegetation_height_mm = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 10, 60, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 10, 20, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 11, 12, subPlot), 
         moss = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 12, 85, moss), 
         litter = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 12, 20, litter), 
         vegetation_height_mm = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 12, 80, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 12, 30, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 14 & species == "Vio_bif", 16, subPlot), 
         moss = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 16, 25, moss), 
         lichen = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 16, NA, lichen), 
         logger = ifelse(plotID == "Lav_2_2" & year == 2021 & subPlot == 16, 40, logger)) |> 
  mutate(subPlot = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 9, 10, subPlot), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 10, 2, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 10, NA, lichen), 
         logger = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 10, 70, logger), 
         vegetation_height_mm = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 10, 160, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 10, 20, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 11, 12, subPlot), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 12, 15, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 12, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 12, 15, litter), 
         vegetation_height_mm = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 12, 70, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 12, 30, moss_depth_mm)) |> 
  filter(!(plotID == "Lav_2_5" & year == 2021 & subPlot == 13)) |> 
  mutate(lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 1, 10, lichen),  
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 1, 5, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 2, 5, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 2, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 2, 10, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 3, 5, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 3, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 3, 10, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 4, 10, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 4, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 4, 50, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 5, 20, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 5, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 5, 30, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 6, 10, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 6, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 6, 15, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 7, 15, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 7, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 7, 10, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 8, 5, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 8, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 8, 15, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 14, 15, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 14, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 14, 35, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 15, 5, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 15, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 15, 5, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 16, 2, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 16, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 16, 5, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 17, 10, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 17, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 17, 5, litter), 
         vegetation_height_mm = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 17, NA, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 17, NA, moss_depth_mm), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 18, 5, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 18, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 18, 10, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 19, 15, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 19, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 19, 15, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 20, 10, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 20, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 20, 50, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 21, 10, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 21, 15, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 21, 50, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 22, 5, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 22, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 22, 2, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 24, 1, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 24, 10, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 24, 40, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 26, 15, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 26, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 26, 30, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 28, 5, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 28, 20, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 28, 40, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 29, 10, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 29, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 29, 50, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 30, 5, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 30, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 30, 10, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 31, 5, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 31, 20, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 31, 30, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 32, 2, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 32, 5, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 32, 15, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 33, 10, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 33, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 33, 15, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 34, 10, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 34, 5, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 34, 25, litter), 
         moss = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 35, 20, moss), 
         lichen = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 35, NA, lichen), 
         litter = ifelse(plotID == "Lav_2_5" & year == 2021 & subPlot == 35, 15, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Lav_2_6" & year == 2021 & subPlot == 3 & species == "Cer_cer", 5, subPlot), 
         moss = ifelse(plotID == "Lav_2_6" & year == 2021 & subPlot == 5, 5, moss), 
         litter = ifelse(plotID == "Lav_2_6" & year == 2021 & subPlot == 5, 15, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Lav_2_6" & year == 2021 & subPlot == 9, 10, subPlot), 
         litter = ifelse(plotID == "Lav_2_6" & year == 2021 & subPlot == 10, 5, litter), 
         logger = ifelse(plotID == "Lav_2_6" & year == 2021 & subPlot == 10, 50, logger), 
         vegetation_height_mm = ifelse(plotID == "Lav_2_6" & year == 2021 & subPlot == 10, 160, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Lav_2_6" & year == 2021 & subPlot == 10, 25, moss_depth_mm)) |> 
  bind_rows(vac_myr_lav_2_6_2021) |> 
  filter(!(plotID == "Lav_5_3" & year == 2021 & subPlot == 13)) |> 
  bind_rows(agr_cap_lav_5_3_2021) |> 
  mutate(moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 1, 30, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 1, 15, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 2, 25, moss), 
         lichen = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 2, 10, lichen), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 2, 25, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 3, 10, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 3, 30, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 4, 5, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 4, 25, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 5, 30, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 5, 30, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 6, 30, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 6, 15, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 7, 20, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 7, 20, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 8, 10, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 8, 25, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 10, 20, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 10, 25, litter), 
         vegetation_height_mm = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 10, 50, vegetation_height_mm),
         moss_depth_mm = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 10, 20, moss_depth_mm),
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 12, 15, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 12, 30, litter), 
         vegetation_height_mm = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 12, 100, vegetation_height_mm),
         moss_depth_mm = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 12, 25, moss_depth_mm),
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 14, 30, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 14, 20, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 15, 15, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 15, 30, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 16, 25, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 16, 30, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 17, 25, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 17, 15, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 18, 25, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 18, 20, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 19, 10, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 19, 20, litter), 
         logger = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 19, 55, logger), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 20, 35, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 20, 15, litter), 
         logger = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 20, 30, logger), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 21, 40, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 21, 25, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 22, 20, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 22, 25, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 24, 20, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 24, 25, litter), 
         vegetation_height_mm = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 24, 70, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 24, 30, moss_depth_mm), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 26, 15, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 26, 10, litter), 
         logger = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 26, 70, logger), 
         vegetation_height_mm = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 26, 70, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 26, 30, moss_depth_mm), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 28, 30, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 28, 15, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 29, 30, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 29, 25, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 30, 15, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 30, 40, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 31, 10, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 31, 30, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 32, 10, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 32, 80, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 33, 25, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 33, 30, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 34, 15, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 34, 50, litter), 
         moss = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 35, 5, moss), 
         litter = ifelse(plotID == "Lav_5_3" & year == 2021 & subPlot == 35, 50, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Lav_7_1" & year == 2021 & subPlot == 9 , 8, subPlot), 
         moss = ifelse(plotID == "Lav_7_1" & year == 2021 & subPlot == 8, 35, moss), 
         litter = ifelse(plotID == "Lav_7_1" & year == 2021 & subPlot == 8, 5, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Lav_7_3" & year == 2021 & subPlot == 13 , 15, subPlot), 
         moss = ifelse(plotID == "Lav_7_3" & year == 2021 & subPlot == 15, 35, moss), 
         litter = ifelse(plotID == "Lav_7_3" & year == 2021 & subPlot == 15, 15, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Lav_7_3" & year == 2021 & subPlot == 16 & species == "Ant_odo", 18, subPlot), 
         moss = ifelse(plotID == "Lav_7_3" & year == 2021 & subPlot == 18, 25, moss), 
         litter = ifelse(plotID == "Lav_7_3" & year == 2021 & subPlot == 18, 25, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 11 , 12, subPlot), 
         moss = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 12, 25, moss), 
         litter = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 12, 15, litter), 
         vegetation_height_mm = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 12, 32, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 12, 12, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 14 & species == "Agr_cap", 15, subPlot), 
         moss = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 15, 35, moss), 
         litter = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 15, 10, litter)) |> 
  mutate(functional_group = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 17 , NA, functional_group), 
         species = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 17, NA, species), 
         value = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 17, 0, value), 
         presence = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 17, 0, presence), 
         seedling = ifelse(plotID == "Skj_1_3" & year == 2021 & subPlot == 17, FALSE, seedling)) |> 
  mutate(subPlot = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 23, 22, subPlot), 
         moss = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 22, 10, moss), 
         litter = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 22, 20, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 25, 24, subPlot), 
         moss = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 24, 5, moss), 
         litter = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 24, 15, litter), 
         vegetation_height_mm = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 24, 75, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 24, 17, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 27, 26, subPlot), 
         moss = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 26, 60, moss), 
         litter = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 26, 10, litter), 
         vegetation_height_mm = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 26, 85, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Skj_1_5" & year == 2021 & subPlot == 26, 27, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Skj_4_3" & year == 2021 & subPlot == 23, 19, subPlot), 
         moss = ifelse(plotID == "Skj_4_3" & year == 2021 & subPlot == 19, 5, moss), 
         litter = ifelse(plotID == "Skj_4_3" & year == 2021 & subPlot == 19, 60, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Skj_4_3" & year == 2021 & subPlot == 25, 24, subPlot), 
         moss = ifelse(plotID == "Skj_4_3" & year == 2021 & subPlot == 24, 5, moss), 
         litter = ifelse(plotID == "Skj_4_3" & year == 2021 & subPlot == 24, 40, litter), 
         vegetation_height_mm = ifelse(plotID == "Skj_4_3" & year == 2021 & subPlot == 24, 100, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Skj_4_3" & year == 2021 & subPlot == 24, 10, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_2_2" & year == 2021 & subPlot == 23 & species == "Alc_alp", 20, subPlot), 
         moss = ifelse(plotID == "Gud_2_2" & year == 2021 & subPlot == 20, NA, moss), 
         litter = ifelse(plotID == "Gud_2_2" & year == 2021 & subPlot == 20, 70, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_2_2" & year == 2021 & subPlot == 19 & species == "Ave_fle", 22, subPlot), 
         moss = ifelse(plotID == "Gud_2_2" & year == 2021 & subPlot == 22, NA, moss), 
         litter = ifelse(plotID == "Gud_2_2" & year == 2021 & subPlot == 22, 70, litter)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_2_2" & year == 2021 & subPlot == 23 & species == "Ave_fle", 24, subPlot), 
         moss = ifelse(plotID == "Gud_2_2" & year == 2021 & subPlot == 24, 10, moss), 
         litter = ifelse(plotID == "Gud_2_2" & year == 2021 & subPlot == 24, 60, litter), 
         vegetation_height_mm = ifelse(plotID == "Gud_2_2" & year == 2021 & subPlot == 24, 75, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Gud_2_2" & year == 2021 & subPlot == 24, 13, moss_depth_mm)) |> 
  filter(!(plotID == "Gud_3_5" & year == 2021 & subPlot == 9)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_4_4" & year == 2021 & subPlot == 13, 12, subPlot), 
         moss = ifelse(plotID == "Gud_4_4" & year == 2021 & subPlot == 12, 5, moss), 
         litter = ifelse(plotID == "Gud_4_4" & year == 2021 & subPlot == 12, 70, litter), 
         vegetation_height_mm = ifelse(plotID == "Gud_4_4" & year == 2021 & subPlot == 12, 70, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Gud_4_4" & year == 2021 & subPlot == 12, 5, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_4_4" & year == 2021 & subPlot == 17 & species == "Nar_str", 14, subPlot), 
         moss = ifelse(plotID == "Gud_4_4" & year == 2021 & subPlot == 14, 1, moss), 
         litter = ifelse(plotID == "Gud_4_4" & year == 2021 & subPlot == 14, 85, litter)) |> 
  bind_rows(nar_str_gud_4_4_18_2021) |> 
  filter(!(plotID == "Gud_5_2" & year == 2021 & subPlot %in% c(9, 11, 13, 23, 25, 27))) |> 
  filter(!(plotID == "Gud_5_4" & year == 2021 & subPlot %in% c(9, 11, 13, 23, 25, 27))) |> 
  filter(!(plotID == "Ulv_1_1" & year == 2021 & subPlot == 13)) |> 
  filter(!(plotID == "Ulv_3_4" & year == 2021 & subPlot == 9)) |> 
  mutate(subPlot = ifelse(plotID == "Ulv_6_4" & year == 2022 & subPlot == 11, 10, subPlot), 
         litter = ifelse(plotID == "Ulv_6_4" & year == 2022 & subPlot == 10, 50, litter), 
         vegetation_height_mm = ifelse(plotID == "Ulv_6_4" & year == 2022 & subPlot == 10, 110, vegetation_height_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Ulv_7_2" & year == 2022 & subPlot == 25, 24, subPlot), 
         moss = ifelse(plotID == "Ulv_7_2" & year == 2022 & subPlot == 24, 10, moss), 
         litter = ifelse(plotID == "Ulv_7_2" & year == 2022 & subPlot == 24, 50, litter), 
         vegetation_height_mm = ifelse(plotID == "Ulv_7_2" & year == 2022 & subPlot == 24, 80, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Ulv_7_2" & year == 2022 & subPlot == 24, 15, moss_depth_mm)) |> 
  filter(!(plotID == "Ulv_7_2" & year == 2022 & species == "Tar_sp")) |> 
  bind_rows(tar_sp_ulv_7_2_2022) |> 
  mutate(subPlot = ifelse(plotID == "Skj_6_6" & year == 2022 & subPlot == 25, 24, subPlot), 
         moss = ifelse(plotID == "Skj_6_6" & year == 2022 & subPlot == 24, 10, moss), 
         lichen = ifelse(plotID == "Skj_6_6" & year == 2022 & subPlot == 24, 15, lichen), 
         litter = ifelse(plotID == "Skj_6_6" & year == 2022 & subPlot == 24, 10, litter), 
         vegetation_height_mm = ifelse(plotID == "Skj_6_6" & year == 2022 & subPlot == 24, 35, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Skj_6_6" & year == 2022 & subPlot == 24, 17, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_4_6" & year == 2023 & subPlot == 9 , 10, subPlot), 
         litter = ifelse(plotID == "Gud_4_6" & year == 2023 & subPlot == 10, 80, litter), 
         vegetation_height_mm = ifelse(plotID == "Gud_4_6" & year == 2023 & subPlot == 10, 100, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Gud_4_6" & year == 2023 & subPlot == 10, 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Gud_4_6" & year == 2023 & subPlot == 11 , 12, subPlot), 
         moss = ifelse(plotID == "Gud_4_6" & year == 2023 & subPlot == 12, 5, moss), 
         litter = ifelse(plotID == "Gud_4_6" & year == 2023 & subPlot == 12, 70, litter), 
         vegetation_height_mm = ifelse(plotID == "Gud_4_6" & year == 2023 & subPlot == 12, 60, vegetation_height_mm), 
         moss_depth_mm = ifelse(plotID == "Gud_4_6" & year == 2023 & subPlot == 12, 20, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(plotID == "Ulv_2_1" & year == 2023 & subPlot == 27 , 28, subPlot), 
         moss = ifelse(plotID == "Ulv_2_1" & year == 2023 & subPlot == 28, 20, moss), 
         litter = ifelse(plotID == "Ulv_2_1" & year == 2023 & subPlot == 28, 10, litter)) |> 
  filter(!(plotID == "Lav_3_6" & year == 2021 & subPlot == 27))

# Some individuals were a bit uncertain (suffix _cf in the file)----

levels(as.factor(grep("_cf$", community_subplot$species, value = TRUE)))
# Agr_cap_cf, Car_cap_cf, Car_nig_cf, Car_nor_cf, Epi_ana_cf, Ran_acr_cf and Vio_can_cf

find_plot_year(community_subplot, "Agr_cap_cf") # Lav_4_1 and Lav_5_2 in 2021, Gud_7_3 in 2023
turfplot(community_subplot, "Gud_7_3") # Seems it is indeed Agr_cap
turfplot(community_subplot, "Lav_4_1") # Seems it is indeed Agr_cap
turfplot(community_subplot, "Lav_5_2") # Seems it is indeed Agr_cap

find_plot_year(community_subplot, "Car_cap_cf") # Gud_4_3 and Gud_4_4 in 2021, Gud_6_3 in 2023
turfplot(community_subplot, "Gud_4_3") # Seems it is actually Car_fla
turfplot(community_subplot, "Gud_4_4") # Seems it is indeed Car_cap
turfplot(community_subplot, "Gud_6_3") # Seems it is indeed Car_cap

find_plot_year(community_subplot, "Car_nig_cf") # Skj_3_1 in 2023
turfplot(community_subplot, "Skj_3_1") # Seems it is actually Car_big

find_plot_year(community_subplot, "Car_nor_cf") # Lav_3_3 and Skj_1_1 in 2023
turfplot(community_subplot, "Lav_3_3") # Seems it is indeed Car_nor
turfplot(community_subplot, "Skj_1_1") # Seems it is actually Car_cap

find_plot_year(community_subplot, "Epi_ana_cf") # Lav_2_4
turfplot(community_subplot, "Lav_2_4") # Seems it is indeed Epi_ana

find_plot_year(community_subplot, "Ran_acr_cf") # Gud_2_2
turfplot(community_subplot, "Gud_2_2") # The scans says Rum_ace. But neither of the species grow in this block. I remove it

find_plot_year(community_subplot, "Vio_can_cf") # Ulv_7_4
turfplot(community_subplot, "Ulv_7_4") # Seems it is actually a juvenile Vio_bif

community_subplot_cf <- community_subplot |>
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
  
# For some individuals we know the genus but not the species (_sp)----

levels(as.factor(grep("_sp", community_subplot_cf$species, value = TRUE)))
# Alc_sp and Tar_sp are species by themselves, we keep them. We check the rest

# Ant_sp
find_plot_year(community_subplot_cf, "Ant_sp") # Skj_6_3 in 2019
turfplot(community_subplot_cf, "Skj_6_3") # Not really possible to tell, I drop it

# Car_sp
find_plot_year(community_subplot_cf, "Car_sp") |> print(n = 41) # Many cases. I create a file only with Carex
community_carex <- community_subplot_cf |> filter(grepl("Car_", species))
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
carex_double <- filter(community_subplot_cf, species == "Car_sp" & plotID == "Gud_7_2" & subPlot == 8 & year == 2018) # Create a duplicate
turfplot(community_carex, "Gud_7_3") # One Car_big (subplot 30). All other Carex in the other subplot
turfplot(community_carex, "Gud_7_4") # Seems it is Car_big in all cases. Duplicate with unique()
turfplot(community_carex, "Gud_7_6") # Car_vag: 1, 24, 31. Car_big: 5. Car_fla: 8. Remove: 2
turfplot(community_carex, "Lav_2_2") # Seems it is Car_pil. No duplicates
turfplot(community_carex, "Lav_4_3") # Seems it is Car_vag. Duplicate with unique()
turfplot(community_carex, "Skj_5_3") # Seems it is Car_cap. No duplicates
turfplot(community_carex, "Ulv_3_3") # It is probably Car_pal. No duplicates
turfplot(community_carex, "Ulv_3_5") # It is probably Car_big. No duplicates
turfplot(community_carex, "Ulv_6_3") # Not enough information to decide. I drop it

# Epi_sp
find_plot_year(community_subplot_cf, "Epi_sp") # Skj_2_5 and Gud_1_5 in 2018, Lav_5_3 and Skj_3_4 in 2021
turfplot(community_subplot_cf, "Gud_1_5")
filter(community_subplot_cf, grepl("Epi_", species) & grepl("Gud_1", plotID)) |> select(plotID, year, species)
turfplot(community_subplot_cf, "Lav_5_3") # It is Epi_ana
turfplot(community_subplot_cf, "Skj_2_5") # Seems it is Epi_ana
turfplot(community_subplot_cf, "Skj_3_4")
filter(community_subplot_cf, grepl("Epi_", species) & grepl("Skj_3", plotID)) |>
  select(plotID, year, species) |>
  print(n = 32)
# Gud_1_5 and Skj_3_4 are difficult to tell. But small Epilobium can be confounded with Veronica alpina. In both cases Ver_alp is found within the same subplot, so we remove them and edit the values of Ver_alp (if needed)

# Equ_sp
find_plot_year(community_subplot_cf, "Equ_sp") # Skj_5_2 in 2021
# I have checked the scan, this is actually Eup_sp, there was a typo
turfplot(community_subplot_cf, "Skj_5_2") # It is probably Eup_wet

# Eri_sp
find_plot_year(community_subplot_cf, "Eri_sp") # Skj_1_4 in 2018 and 2021
turfplot(community_subplot_cf, "Skj_1_4")
filter(community_subplot_cf, grepl("Eri_", species)) |> select(plotID, year, species) # We do not have enough information, we keep it as Eri_sp

# Fes_sp
find_plot_year(community_subplot_cf, "Fes_sp") # Ulv_1_4 in 2018, Gud_3_5 and Skj_2_6 in 2023
turfplot(community_subplot_cf, "Gud_3_5") # Seems it is Fes_rub
turfplot(community_subplot_cf, "Skj_2_6") # Difficult to tell
filter(community_subplot_cf, grepl("Fes_", species) & grepl("Skj_2", plotID)) |> select(plotID, year, species) # It is probably Fes_rub
turfplot(community_subplot_cf, "Ulv_1_4") # It is not Fes_rub
filter(community_subplot_cf, species == "Fes_ovi" & grepl("Ulv_", plotID)) # It is probably Fes_ovi
filter(community_subplot_cf, species == "Fes_viv" & grepl("Ulv_", plotID)) # And not Fes_viv

# Gal_sp
find_plot_year(community_subplot_cf, "Gal_sp") # Ulv_5_5 and Gud_3_6 in 2022, Gud_3_6 and Gud_6_4 in 2023
turfplot(community_subplot_cf, "Gud_3_6") # Difficult to tell
turfplot(community_subplot_cf, "Gud_6_4") # Difficult to tell
filter(community_subplot_cf, site == "Gudmedalen" & grepl("Gal_", species)) |> select(plotID, year, species) # We do not have enough information, we keep all in Gudmedalen as Gal_sp
turfplot(community_subplot_cf, "Ulv_5_5") # Seems it is Gal_bor

# Gen_sp
find_plot_year(community_subplot_cf, "Gen_sp") # Gud_7_3 in 2019
turfplot(community_subplot_cf, "Gud_7_3") # Seems it is Gen_niv

# Hie_sp
find_plot_year(community_subplot_cf, "Hie_sp") # Gud_4_3 in 2018; Gud_4_3, Gud_7_6, Ulv_1_4, Ulv_1_3 and Ulv_6_3 in 2019; Gud_4_1, Gud_4_3 and Ulv_6_4 in 2021; Skj_2_5 in 2022; Gud_7_3 in 2023
turfplot(community_subplot_cf, "Gud_4_1") # Difficult to tell
turfplot(community_subplot_cf, "Gud_4_3") # Difficult to tell
filter(community_subplot_cf, grepl("Hie", species) & grepl("Gud_4", plotID)) |> select(plotID, year, species) # Seems they might be Hie_pil, but only found in 2018
turfplot(community_subplot_cf, "Gud_7_3") # Difficult to tell
turfplot(community_subplot_cf, "Gud_7_6") # Difficult to tell
filter(community_subplot_cf, grepl("Hie", species) & species != "Hie_sp" & grepl("Gud_", plotID)) |>
  select(plotID, year, species) |>
  print(n = 44) # Not possible to say
turfplot(community_subplot_cf, "Skj_2_5") # Difficult to tell
filter(community_subplot_cf, grepl("Hie", species) & grepl("Skj_", plotID)) |>
  select(plotID, year, species) |>
  print(n = 33) # Not possible to say
turfplot(community_subplot_cf, "Ulv_1_3") # Difficult to tell
turfplot(community_subplot_cf, "Ulv_1_4") # Difficult to tell
turfplot(community_subplot_cf, "Ulv_6_3") # Difficult to tell
turfplot(community_subplot_cf, "Ulv_6_4") # Difficult to tell
filter(community_subplot_cf, grepl("Hie", species) & grepl("Ulv_", plotID)) |>
  select(plotID, year, species) |>
  print(n = 27) # Not possible to say
# It is best to keep it as _sp rather than remove it

# Hyp_sp
find_plot_year(community_subplot_cf, "Hyp_sp") # Skj_3_3 in 2021
turfplot(community_subplot_cf, "Skj_3_3") # Seems it is Hyp_mac

# Leo_sp
find_plot_year(community_subplot_cf, "Leo_sp") # Ulv_6_1 in 2021
turfplot(community_subplot_cf, "Ulv_6_1") # Seems it is Leo_aut

# Oma_sp
find_plot_year(community_subplot_cf, "Oma_sp") # Lav_7_1 in 2021 and Skj_7_1 in 2023
turfplot(community_subplot_cf, "Lav_7_1") # Seems it is Oma_sup
turfplot(community_subplot_cf, "Skj_7_1") # Might be Oma_sup
filter(community_subplot_cf, grepl("Oma_", species) & grepl("Skj_7", plotID)) |> select(plotID, year, species) # It is probably Oma_sup

# Pyr_sp
find_plot_year(community_subplot_cf, "Pyr_rot") # Many cases. Since it is difficult to distinguish Pyr_min and Pyr_rot, we group all of them under the name Pyr_sp

# Ran_sp
find_plot_year(community_subplot_cf, "Ran_sp") # Lav_2_3 in 2018
turfplot(community_subplot_cf, "Lav_2_3") # Seems it is Ran_pyg

# Rhi_sp
find_plot_year(community_subplot_cf, "Rhi_sp") # Skj_1_4 in 2023
turfplot(community_subplot_cf, "Skj_1_4") # Difficult to tell
filter(community_subplot_cf, grepl("Rhi_", species) & grepl("Skj_1", plotID)) |> select(plotID, year, species) # It is probably Rhi_min

# Sag_sp
find_plot_year(community_subplot_cf, "Sag_sp") # Skj_4_1 in 2021
turfplot(community_subplot_cf, "Skj_4_1") # Seems it is Sag_sag

# Sal_sp
find_plot_year(community_subplot_cf, "Sal_sp") # Lav_2_2, Lav_3_3 and Gud_5_1 in 2021
turfplot(community_subplot_cf, "Gud_5_1") # Seems it is Sal_lan
turfplot(community_subplot_cf, "Lav_2_2") # Sal_sp does not exist in the scan. The values are not from another species. I remove it
turfplot(community_subplot_cf, "Lav_3_3") # Sal_sp does not exist in the scan. The values are not from another species. I remove it

# Sel_sp
find_plot_year(community_subplot_cf, "Sel_sp") # Skj_6_4 in 2019
turfplot(community_subplot_cf, "Skj_6_4") # Difficult to tell
filter(community_subplot_cf, grepl("Sel_", species) & grepl("Skj_6", plotID)) |>
  select(plotID, year, species) |>
  print(n = 22) # It is probably Sel_sel

# Tri_sp
find_plot_year(community_subplot_cf, "Tri_sp") # Lav_2_5 in 2021
turfplot(community_subplot_cf, "Lav_2_5") # Difficult to tell. The scan says Trifolium
filter(community_subplot_cf, grepl("Tri", species) & species != "Tri_ces" & site == "Lavisdalen") |> select(plotID, year, species) # It could be Tri_pra

# Vio_sp
find_plot_year(community_subplot_cf, "Vio_sp") # Ulv_1_5 in 2021
turfplot(community_subplot_cf, "Ulv_1_5") # It is probably Vio_bif

community_subplot_cf_sp <- community_subplot_cf |> 
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

find_plot_year(community_subplot_cf_sp, "Ver_cha_eller_Hyp_mac") # Skj_2_1 in 2023
turfplot(community_subplot_cf_sp, "Skj_2_1") # After some discussion we have agreed this is Hyp_mac

find_plot_year(community_subplot_cf_sp, "Unknown") |> print(n = 21) # After looking at the scans, we might be able to fix: Lav_1_3, Lav_3_3 and Gud_5_5 in 2021. Cannot do anything about the rest
turfplot(community_subplot_cf_sp, "Lav_1_3") # The scans says Van_atr, it is Val_atr
turfplot(community_subplot_cf_sp, "Lav_3_3") # The scans says Suc_vul, it is Suc_pra
turfplot(community_subplot_cf_sp, "Gud_5_5") # The scans says Fjelljamne, it is Dip_alp

community_subplot_cf_sp_unknown <- community_subplot_cf_sp |>
  mutate(species = ifelse(species == "Ver_cha_eller_Hyp_mac", "Hyp_mac", species)) |>
  mutate(species = ifelse(species == "Unknown" & plotID == "Lav_1_3" & year == 2021, "Val_atr", species)) |>
  mutate(species = ifelse(species == "Unknown" & plotID == "Lav_3_3" & year == 2021, "Suc_pra", species)) |>
  mutate(species = ifelse(species == "Unknown" & plotID == "Gud_5_5" & year == 2021, "Dip_alp", species)) |>
  filter(!(species %in% c("Nid_juvenile", "Nid_seedling", "Poaceae_sp", "Unknown"))) |>
  unique()

# We correct some last few errors----

turfplot(community_subplot_cf_sp_unknown, "Lav_2_2") # There are a few mistakes in 2021: Ant_alp is actually Alc_alp, the values of Tar_sp belong to Bis_viv, Ver_alp is missing in subplot 1
# Since all values for Tar_sp are 1, we can create a tibble from the original one, choosing the correct subplots and changing the functional group and species
tar_sp_lav_2_2_2021 <- community_subplot_cf_sp_unknown |>
  filter(plotID == "Lav_2_2" & year == 2021 & subPlot %in% c(8, 10, 19, 20, 26, 32, 33) & value == 1) |>
  mutate(functional_group = "Forbs") |>
  mutate(species = "Tar_sp") |>
  unique()
ver_alp_lav_2_2_2021 <- community_subplot_cf_sp_unknown |>
  filter(plotID == "Lav_2_2" & year == 2021 & subPlot == 1 & functional_group == "Forbs" & value == 1) |>
  mutate(species = "Ver_alp") |>
  unique()

turfplot(community_subplot_cf_sp_unknown, "Lav_3_3") # Agr_cap missing in subplot 10
agr_cap_lav_3_3_2021 <- community_subplot_cf_sp_unknown |>
  filter(plotID == "Lav_3_3" & year == 2021 & subPlot == 10 & functional_group == "Graminoids" & value == 1) |>
  mutate(species = "Agr_cap")

community_clean <- community_subplot_cf_sp_unknown |>
  mutate(species = ifelse(species == "Ant_alp" & plotID == "Lav_2_2", "Alc_alp", species)) |>
  filter(!(species == "Tar_sp" & plotID == "Lav_2_2" & year == 2021)) |>
  bind_rows(tar_sp_lav_2_2_2021) |>
  bind_rows(ver_alp_lav_2_2_2021) |>
  bind_rows(agr_cap_lav_3_3_2021)

write.csv(community_clean, file = "data/INCLINE_community_subplot_clean.csv", row.names= FALSE)

# There is one case of Pyr_rot with flower, Gud_7_4 in 2018. In 2023 it was called Pyr_rot, the other years Pyr_sp. I still call all Pyrola, Pyr_sp, but we can discuss