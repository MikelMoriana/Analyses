# Libraries and data----

library(tidyverse)

library(ggpubr)
library(lme4)

community_cover <- read.delim("data/INCLINE_community_species_cover_fixed.csv", sep = ",")
cover_4years <- community_cover |> 
  filter(treatment != "R" & year != 2022) # We are not interested in Removal plots. In 2022 only the center subplots were studied
blank_cover <- community_cover |> filter(is.na(cover))

# Looking at when the cover is missing

community_old <- read_delim("Data_old/INCLINE_community_2018_2019_2021_2022_2023.csv", col_types = cols(.default = col_character()))



# Functional group cover----

cover_4years_functional <- cover_4years |> 
  group_by(site, warming, treatment, year, functional_group) |> 
  summarise(mean = mean(cover))
