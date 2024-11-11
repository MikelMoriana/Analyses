# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)


# Set target options:
tar_option_set(
  packages = c("tidyverse", "vegan"),
  format = "rds", 
  seed = 811
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("Scripts/Functions.R")

# Replace the target list below with your own:
list(
  tar_target(
    name = file,
    command = "data_cleaned/INCLINE_community_plotlevel_info.csv",
    format = "file"
  ),
  tar_target(
    name = plotlv,
    command = read.csv(file)
  ),
  tar_target(
    name = plotlv_wide,
    command = plotlv |> 
      filter(year != 2022) |> 
      mutate(s_year = scale(year)) |> 
      relocate(s_year, .after = year) |> 
      mutate(site = factor(site, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) |> 
      select(-c("recorder", "writer", "date")) |> 
      pivot_wider(names_from = name, values_from = value) |> 
      select(-c("total_poo_cover", "total_fungus_cover")) |> 
      mutate(vascular_biomass = vegetation_cover * vegetation_height_mean) |> 
      relocate(vegetation_height_mean, .after = vegetation_cover) |> 
      relocate(vascular_biomass, .after = vegetation_height_mean) |> 
      mutate(moss_biomass = total_moss_cover * moss_depth_mean) |> 
      relocate(moss_depth_mean, .after = total_moss_cover) |> 
      relocate(moss_biomass, .after = moss_depth_mean) |> 
      na.omit()
  ), 
  tar_target(
    name = model_factors, 
    command = c("s_year", "site", "warming", "treatment")
  ), 
  tar_target(
    name = plotlv_metadata, 
    command = plotlv_wide |> 
      select(year:treatment),
  ), 
  tar_target(
    name = plotlv_measured, 
    command = plotlv_wide |> 
      select(vegetation_cover:total_rock_cover),
  ), 
  tar_target(
    name = plotlv_nmds2, 
    command = metaMDS(plotlv_measured, k = 2, distance = "bray", trymax = 1000)
  ), 
  tar_target(
    name = plotlv_nmds3, 
    command = metaMDS(plotlv_measured, k = 3, distance = "bray", trymax = 1000)
  ), 
  tar_target(
    name = plotlv_nmds4, 
    command = metaMDS(plotlv_measured, k = 4, distance = "bray", trymax = 1000)
  )
    
)
