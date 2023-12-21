# We remove the columns we are not interested in, and make it wide format
community_wide <- pivot_wider(
  community,
  id_cols = c(site:moss_depth_mm),
  names_from = species,
  values_from = presence,
  names_sort = TRUE,
)

community_species <- community_wide |> 
  select(matches("[A-Za-z]{3}_[A-Za-z]{3}$")) |> # We use only those species we are sure of
  mutate(across(everything(), ~replace_na(.x, 0)))
community_richness <- rowSums(community_species)
community_info <- community |> 
  select(site, plotID, warming, treatment, subPlot, year)

set.seed(811)
nmds_community <- metaMDS(community_species, k=2, distance="jaccard", trymax = 1000)
# This gives an error because of missing values. It seems there is one subplot that needs to be removed (Gud_2_2 2019 subplot 9, and for 10 there is only logger, no species). Same as for the latter happens for Skj_1_3 17 2021. I cannot use the 3 letters _ 3 letters, since I remove Alc_sp. Find another solution