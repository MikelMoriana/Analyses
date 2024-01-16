# Libraries and data files----

library(tidyverse)

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
