# Libraries and data files----

library(tidyverse)
library(lme4)

community_clean <- targets::tar_read(community_clean)

# We remove the columns we are not interested in, and make it wide format
community_presence <-
  pivot_wider(
    community_clean,
    id_cols = c(site, plotID, warming, treatment, year, subPlot, moss, lichen, litter, rock, poo, fungus, bare_ground, logger),
    names_from = species,
    values_from = presence,
    names_sort = TRUE
  ) |>
  mutate(across(everything(), ~ replace_na(.x, 0)))

colnames(community_presence)
community_richness <- community_presence %>% 
  mutate(richness = rowSums(select(., Ach_mil:Vio_tri)), .after = subPlot)

model_richness <- glmer(richness ~ site * warming * treatment + year + (plotID | subPlot), data = community_richness)
