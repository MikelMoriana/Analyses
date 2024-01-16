# Libraries and data files----

library(tidyverse)
library(vegan)

community_clean <- targets::tar_read(community_clean)

gudmedalen <- community_clean |> filter(site == "Gudmedalen")
lavisdalen <- community_clean |> filter(site == "Lavisdalen")
skjellingahaugen <- community_clean |> filter(site == "Skjellingahaugen")
ulvehaugen <- community_clean |> filter(site == "Ulvehaugen")

# Gudmedalen----

# We remove the columns we are not interested in, and make it wide format
gudmedalen_presence <- pivot_wider(
  gudmedalen,
  id_cols = c(site, plotID, warming, treatment, year, subPlot, moss, lichen, litter, rock, poo, fungus, bare_ground, logger),
  names_from = species,
  values_from = presence,
  names_sort = TRUE,
)

colnames(gudmedalen_presence)
gudmedalen_species <- gudmedalen_presence |>
  select(Ach_mil:Vio_tri) |> # We choose only the species. Double-check that these are the first and last species in the tibble
  mutate(across(everything(), ~ replace_na(.x, 0))) # We need 0s instead of NAs
gudmedalen_richness <- rowSums(gudmedalen_species)
gudmedalen_info <- gudmedalen_presence |>
  select(site:logger)

set.seed(811)
nmds_gudmedalen <- metaMDS(gudmedalen_species, k=2, distance="jaccard", trymax = 1000)
saveRDS(nmds_gudmedalen)

# Lavisdalen----

# We remove the columns we are not interested in, and make it wide format
lavisdalen_presence <- pivot_wider(
  lavisdalen,
  id_cols = c(site, plotID, warming, treatment, year, subPlot, moss, lichen, litter, rock, poo, fungus, bare_ground, logger),
  names_from = species,
  values_from = presence,
  names_sort = TRUE,
)

colnames(lavisdalen_presence)
lavisdalen_species <- lavisdalen_presence |>
  select(Ach_mil:Vio_tri) |> # We choose only the species. Double-check that these are the first and last species in the tibble
  mutate(across(everything(), ~ replace_na(.x, 0))) # We need 0s instead of NAs
lavisdalen_richness <- rowSums(lavisdalen_species)
lavisdalen_info <- lavisdalen_presence |>
  select(site:logger)

set.seed(811)
nmds_lavisdalen <- metaMDS(lavisdalen_species, k=2, distance="jaccard", trymax = 1000)
saveRDS(nmds_lavisdalen)

# Skjellingahaugen----

# We remove the columns we are not interested in, and make it wide format
skjellingahaugen_presence <- pivot_wider(
  skjellingahaugen,
  id_cols = c(site, plotID, warming, treatment, year, subPlot, moss, lichen, litter, rock, poo, fungus, bare_ground, logger),
  names_from = species,
  values_from = presence,
  names_sort = TRUE,
)

colnames(skjellingahaugen_presence)
skjellingahaugen_species <- skjellingahaugen_presence |>
  select(Ach_mil:Vio_tri) |> # We choose only the species. Double-check that these are the first and last species in the tibble
  mutate(across(everything(), ~ replace_na(.x, 0))) # We need 0s instead of NAs
skjellingahaugen_richness <- rowSums(skjellingahaugen_species)
skjellingahaugen_info <- skjellingahaugen_presence |>
  select(site:logger)

set.seed(811)
nmds_skjellingahaugen <- metaMDS(skjellingahaugen_species, k=2, distance="jaccard", trymax = 1000)
saveRDS(nmds_skjellingahaugen)

# Ulvehaugen----

# We remove the columns we are not interested in, and make it wide format
ulvehaugen_presence <- pivot_wider(
  ulvehaugen,
  id_cols = c(site, plotID, warming, treatment, year, subPlot, moss, lichen, litter, rock, poo, fungus, bare_ground, logger),
  names_from = species,
  values_from = presence,
  names_sort = TRUE,
)

colnames(ulvehaugen_presence)
ulvehaugen_species <- ulvehaugen_presence |>
  select(Ach_mil:Vio_tri) |> # We choose only the species. Double-check that these are the first and last species in the tibble
  mutate(across(everything(), ~ replace_na(.x, 0))) # We need 0s instead of NAs
ulvehaugen_richness <- rowSums(ulvehaugen_species)
ulvehaugen_info <- ulvehaugen_presence |>
  select(site:logger)

set.seed(811)
nmds_ulvehaugen <- metaMDS(ulvehaugen_species, k=2, distance="jaccard", trymax = 1000)
saveRDS(nmds_ulvehaugen)
