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
    name = sal_her_skj_6_6_7_2019,
    command = community |> 
      filter(plotID == "Skj_6_6" & year == 2019 & subPlot == 7 & species == "Ant_odo") |> 
      mutate(species = "Sal_her", 
             functional_group = "Deciduous_shrubs", 
             value = "D", 
             dominance = "D")
  ),
  tar_target(
    name = vac_myr_lav_2_6_2021,
    command = community |>
      filter(plotID == "Lav_2_6" & year == 2021 & subPlot %in% c(3, 4, 5, 10, 19, 21, 24, 29, 32) & species == "Agr_cap") |> 
      mutate(species = "Vac_myr",
             functional_group = "Deciduous_shrubs")
  ),
  tar_target(
    name = agr_cap_lav_5_3_2021,
    command = community |>
      filter(plotID == "Lav_5_3" & year == 2021 & subPlot %in% c(16, 17) & species == "Epi_ana") |> 
      mutate(species = "Agr_cap",
             functional_group = "Graminoids")
  ),
  tar_target(
    name = nar_str_gud_4_4_18_2021,
    command = community |>
      filter(plotID == "Gud_4_4" & year == 2021 & subPlot == 18 & species == "Car_big") |> 
      mutate(species = "Nar_str")
  ),
  tar_target(
    name = tar_sp_ulv_7_2_2022,
    command = community |> 
      filter( plotID == "Ulv_7_2" & year == 2022 & subPlot %in% c(10, 12, 16, 17, 18, 24) & species == "Agr_cap") |> 
      mutate(species = "Tar_sp", 
             functional_group = "Forbs") |> 
      mutate(value = ifelse(subPlot == 12, 1, value), 
             fertile = ifelse(subPlot == 12, FALSE, fertile)) |> 
      mutate(value = ifelse(subPlot == 18, "J", value), 
             juvenile = ifelse(subPlot == 18, TRUE, juvenile))
  ),
  tar_target(
    name = community_subplot,
    command = community |> 
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
  ),
  tar_target(
    name = community_subplot_cf,
    community_subplot |>
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
    name = carex_double,
    command = filter(community_subplot_cf, species == "Car_sp" & plotID == "Gud_7_2" & subPlot == 8 & year == 2018)
  ),
  tar_target(
    name = community_subplot_cf_sp,
    command = community_subplot_cf |> 
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
  ),
  tar_target(
    name = community_subplot_cf_sp_unknown,
    command = community_subplot_cf_sp |>
      mutate(species = ifelse(species == "Ver_cha_eller_Hyp_mac", "Hyp_mac", species)) |>
      mutate(species = ifelse(species == "Unknown" & plotID == "Lav_1_3" & year == 2021, "Val_atr", species)) |>
      mutate(species = ifelse(species == "Unknown" & plotID == "Lav_3_3" & year == 2021, "Suc_pra", species)) |>
      mutate(species = ifelse(species == "Unknown" & plotID == "Gud_5_5" & year == 2021, "Dip_alp", species)) |>
      filter(!(species %in% c("Nid_juvenile", "Nid_seedling", "Poaceae_sp", "Unknown"))) |>
      unique()
  ),
  tar_target(
    name = tar_sp_lav_2_2_2021,
    command = community_subplot_cf_sp_unknown |>
      filter(plotID == "Lav_2_2" & year == 2021 & subPlot %in% c(8, 10, 19, 20, 26, 32, 33) & value == 1) |>
      mutate(functional_group = "Forbs") |>
      mutate(species = "Tar_sp") |>
      unique()
  ),
  tar_target(
    name = ver_alp_lav_2_2_2021,
    command = community_subplot_cf_sp_unknown |>
      filter(plotID == "Lav_2_2" & year == 2021 & subPlot == 1 & functional_group == "Forbs" & value == 1) |>
      mutate(species = "Ver_alp") |>
      unique()
  ),
  tar_target(
    name = agr_cap_lav_3_3_2021,
    command = community_subplot_cf_sp_unknown |>
      filter(plotID == "Lav_3_3" & year == 2021 & subPlot == 10 & functional_group == "Graminoids" & value == 1) |>
      mutate(species = "Agr_cap")
  ),
  tar_target(
    name = community_clean,
    command = community_subplot_cf_sp_unknown |>
      mutate(species = ifelse(species == "Ant_alp" & plotID == "Lav_2_2", "Alc_alp", species)) |>
      filter(!(species == "Tar_sp" & plotID == "Lav_2_2" & year == 2021)) |>
      bind_rows(tar_sp_lav_2_2_2021) |>
      bind_rows(ver_alp_lav_2_2_2021) |>
      bind_rows(agr_cap_lav_3_3_2021)
  )
)
