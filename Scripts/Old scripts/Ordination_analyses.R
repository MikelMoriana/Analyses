# Libraries and data files----

library(tidyverse)
library(vegan)

community_clean <- targets::tar_read(community_clean)

community_4years <- community_clean |> filter(year != 2022) # In 2022 only the center subplots were studied

# We remove the columns we are not interested in, and make it wide format
community_presence <- pivot_wider(
  community_clean,
  id_cols = c(site, plotID, warming, treatment, year, subPlot, moss, lichen, litter, rock, poo, fungus, bare_ground, logger),
  names_from = species,
  values_from = presence,
  names_sort = TRUE,
)

colnames(community_presence)
community_species <- community_presence |>
  select(Ach_mil:Vio_tri) |> # We choose only the species. Double-check that these are the first and last species in the tibble
  mutate(across(everything(), ~ replace_na(.x, 0))) # We need 0s instead of NAs
community_richness <- rowSums(community_species)
community_info <- community_presence |>
  select(site:logger)

# We perform the ordination analysis----

set.seed(811)
nmds_community <- metaMDS(community_species, k=2, distance="jaccard", trymax = 1000)
# This gives an error because of missing values. It seems there is one subplot that needs to be removed (Gud_2_2 2019 subplot 9, and for 10 there is only logger, no species). Same as for the latter happens for Skj_1_3 17 2021.
saveRDS(nmds_community)
