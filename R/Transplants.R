# Libraries and data files----

library(tidyverse)
library(turfmapper)

community_clean <- targets::tar_read(community_clean)
community_4years <- community_clean |> filter(year != 2022) # In 2022 only the center subplots were studied

# We remove the columns we are not interested in, and make it wide format
community_4years_presence <- pivot_wider(
  community_4years,
  id_cols = c(site, plotID, warming, treatment, year, subPlot, moss, lichen, litter, rock, poo, fungus, bare_ground, logger),
  names_from = species,
  values_from = presence,
  names_sort = TRUE,
) |> 
  mutate(across(everything(), ~ replace_na(.x, 0)))

colnames(community_4years_presence)
richness_4years <- community_4years_presence |> 
  select(Ach_mil:Vio_tri) |> # We choose only the species. Double-check that these are the first and last species in the tibble
  mutate(across(everything(), ~ replace_na(.x, 0))) |> # We need 0s instead of NAs
  rowSums()

community_4years_richness <- community_4years_presence |> 
  cbind(richness_4years) |> 
  rename(richness = richness_4years) |> 
  relocate(richness, .after = subPlot)

test <- community_4years_richness |> 
  select(site:richness)

grid <- make_grid(ncol = 7, nrow = 5) # We create the 5x7 grid for the turfmapper

richnessplot(test, "Gudmedalen")

