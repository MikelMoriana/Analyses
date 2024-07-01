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

community_old <- read_delim("Data_old/INCLINE_community_2018_2019_2021_2022_2023.csv", col_types = cols(.default = col_character()))
grid <- turfmapper::make_grid(ncol = 7, nrow = 5) |> 
  rename(subPlot = subturf)
  
community_long <- community_old |> 
  rename(Cer_Sag_cf = "Cer/sag_cf", Cer_sp = "Cer _sp", Nid_seedling = "Nid seedling", block = Block, measure = Measure, site = Site, treatment = Treatment, weather = Weather, vegetation_cover = Veg_cover, vegetation_height_mm = Veg_height_mm, moss_depth_mm = Moss_depth_mm)|> 
  filter(!(subPlot == "cover")) |> 
  mutate(plotID = paste0(str_sub(site, 1,3), "_", block, "_", plot)) |> 
  pivot_longer(Ach_mil:Nid_seedling, values_drop_na = TRUE)|> 
  rename(species = name)|> 
  mutate(subPlot = as.numeric(subPlot, na.rm = TRUE)) |> 
  unique()

turfplot(community_long, "Lav_2_2")
