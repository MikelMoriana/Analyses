# Libraries and data files----

library(tidyverse)
library(lme4)

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
  mutate(across(everything(), ~ replace_na(.x, 0))) # We need 0s instead of NAs

colnames(community_4years_presence)
richness_4years <- community_4years_presence |> 
  select(Ach_mil:Vio_tri) |> # We choose only the species. Double-check that these are the first and last species in the tibble
  rowSums()

community_4years_richness <- community_4years_presence |> 
  cbind(richness_4years) |> 
  rename(richness = richness_4years) |> 
  relocate(richness, .after = subPlot)

# We perform the analysis by site
gudmedalen_4years <- community_4years_richness |> filter(site == "Gudmedalen")
lavisdalen_4years <- community_4years_richness |> filter(site == "Lavisdalen")
skjellingahaugen_4years <- community_4years_richness |> filter(site == "Skjellingahaugen")
ulvehaugen_4years <- community_4years_richness |> filter(site == "Ulvehaugen")

# Gudmedalen----

#richness_gudmedalen_model <- glmer(log(richness) ~ warming + treatment + year + (plotID|subPlot), 
#                       data = gudmedalen_4years)
#saveRDS(richness_gudmedalen_model, "data/richness_gudmedalen_model.rda")
readRDS("data/richness_gudmedalen_model.rda")
summary(richness_gudmedalen_model)
