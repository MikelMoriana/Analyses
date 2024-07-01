# Libraries and data----

library(tidyverse)
library(vegan)

plot_info <- read.csv("data/INCLINE_community_plotlevel_info.csv") |> 
  filter(year != 2022)
plot_wide <- plot_info |> 
  pivot_wider(names_from = name, values_from = value)

hist(plot_wide$vegetation_cover)
hist(plot_wide$total_moss_cover)
hist(plot_wide$total_lichen_cover)
hist(plot_wide$total_litter_cover)
hist(plot_wide$vegetation_height_mean)
hist(plot_wide$moss_depth_mean)

set.seed(811)
nmds_plotinfo <- metaMDS((plot_wide |> select(11:20)), k = 2, distance = "bray", trymax = 1000)
