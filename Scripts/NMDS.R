# Libraries and files----

library(tidyverse)
library(vegan)

community_clean <- read.csv("data/INCLINE_community_subplot.csv") |> 
  mutate(block = as.factor(block)) |> 
  mutate(plot = as.factor(plot)) |> 
  mutate(subPlot = as.factor(subPlot))

# We don't use 2022, since only the inner subplot were studied. This removes Oxa_ace from the dataset
# In 2023 the OTC was placed in the wrong plots in block 5 in Skjellingahauge. We remove this block from the analysis. this removes Phe_con from the dataset
# We reckon some of the species appearing in 2019-2023 came with the transplants, we remove them (and the transplants) for the ordination analyses

community_use <- community_clean |> 
  filter(year != 2022) |> 
  filter(!(site == "Skjellingahaugen" & block == 5))  |> 
  filter(!(treatment == "E" & species %in% c("Car_pil", "Ver_off", "Vio_can"))) |>  
  filter(!(treatment == "N" & species %in% c("Car_pal", "Hyp_mac", "Suc_pra"))) |> 
  filter(!(site == "Gudmedalen" & species %in% c("Tri_pra", "Vio_tri"))) |> 
  filter(!(site == "Lavisdalen" & species %in% c("Ach_mil", "Lys_eur", "Pot_ere", "Pru_vul", "Tri_pra", "Tri_rep"))) |> 
  filter(!(site == "Skjellingahaugen" & species %in% c("Ach_mil", "Ane_nem", "Pru_vul", "Sil_vul", "Ste_gra"))) |> 
  filter(!(site == "Ulvehaugen" & species %in% c("Gal_bor", "Lot_cor", "Pot_ere", "Pru_vul", "Ran_aur", "Tri_pra", "Tri_rep")))


# Plot level----

community_plot <- community_use |> 
  group_by(year, site, plotID, block, plot, warming, treatment, species) |> 
  summarise(abundance = sum(presence)) |> 
  ungroup() |> 
  arrange(species)

plot_abundance <- community_plot |> 
  pivot_wider(names_from = species, values_from = abundance, values_fill = 0) |> 
  arrange(year, plotID)


# For the ordination analyses we divide the dataset by year

plot_2018 <- plot_abundance |> 
  filter(year == 2018)
plot_2019 <- plot_abundance |> 
  filter(year == 2019)
plot_2021 <- plot_abundance |> 
  filter(year == 2021)
plot_2023 <- plot_abundance |> 
  filter(year == 2023)

plot_change18_19 <- (plot_2019 |> select(8:115)) - (plot_2018 |> select(8:115))
plot_change19_21 <- (plot_2021 |> select(8:115)) - (plot_2019 |> select(8:115))
plot_change21_23 <- (plot_2023 |> select(8:115)) - (plot_2021 |> select(8:115))

set.seed(811)
plot18_nmds2 <- metaMDS((plot_2018 |> select(8:115)), k = 2, distance = "bray", trymax = 1000)
saveRDS(plot18_nmds2, file = "Objects/plot18_nmds2.rds")
plot18_nmds2 <- readRDS(file = "Objects/plot18_nmds2.rds")

plot19_nmds2 <- metaMDS((plot_2019 |> select(8:115)), k = 2, distance = "bray", trymax = 1000)
saveRDS(plot19_nmds2, file = "Objects/plot19_nmds2.rds")
plot19_nmds2 <- readRDS(file = "Objects/plot19_nmds2.rds")

plot21_nmds2 <- metaMDS((plot_2021 |> select(8:115)), k = 2, distance = "bray", trymax = 1000)
saveRDS(plot21_nmds2, file = "Objects/plot21_nmds2.rds")
plot21_nmds2 <- readRDS(file = "Objects/plot21_nmds2.rds")

plot23_nmds2 <- metaMDS((plot_2023 |> select(8:115)), k = 2, distance = "bray", trymax = 1000)
saveRDS(plot23_nmds2, file = "Objects/plot23_nmds2.rds")
plot23_nmds2 <- readRDS(file = "Objects/plot23_nmds2.rds")


plot18_nmds2_scores <- plot_2018 |> 
  select(1:7) |> 
  mutate(NMDS1 = as_tibble(scores(plot18_nmds2, display = "sites"))$NMDS1) |> 
  mutate(NMDS2 = as_tibble(scores(plot18_nmds2, display = "sites"))$NMDS2)
plot19_nmds2_scores <- plot_2019 |> 
  select(1:7) |> 
  mutate(NMDS1 = as_tibble(scores(plot19_nmds2, display = "sites"))$NMDS1) |> 
  mutate(NMDS2 = as_tibble(scores(plot19_nmds2, display = "sites"))$NMDS2)
plot21_nmds2_scores <- plot_2021 |> 
  select(1:7) |> 
  mutate(NMDS1 = as_tibble(scores(plot21_nmds2, display = "sites"))$NMDS1) |> 
  mutate(NMDS2 = as_tibble(scores(plot21_nmds2, display = "sites"))$NMDS2)
plot23_nmds2_scores <- plot_2023 |> 
  select(1:7) |> 
  mutate(NMDS1 = as_tibble(scores(plot23_nmds2, display = "sites"))$NMDS1) |> 
  mutate(NMDS2 = (as_tibble(scores(plot23_nmds2, display = "sites"))$NMDS2)) # When plotting change it seemed the second axis might be rotated in 2023 compared to 2018

plot_nmds2_scores <- rbind(plot18_nmds2_scores, plot19_nmds2_scores, plot21_nmds2_scores, plot23_nmds2_scores)

plot(plot_nmds2_scores$NMDS1, plot_nmds2_scores$NMDS2, type = "n")
points(plot18_nmds2_scores$NMDS1, plot18_nmds2_scores$NMDS2, pch = 1)
points(plot23_nmds2_scores$NMDS1, plot23_nmds2_scores$NMDS2, pch = 16)
for(i in 1:114) {
  arrows(x0 = plot18_nmds2_scores$NMDS1, y0 = plot18_nmds2_scores$NMDS2,
         x1 = plot23_nmds2_scores$NMDS1, y1 = plot23_nmds2_scores$NMDS2,
         length = 0.05)
}


# Subplot level----

subplot_presence <- community_use |> 
  select(year, site, plotID, block, plot, warming, treatment, subPlot, species, presence) |> 
  pivot_wider(names_from = species, values_from = presence, values_fill = 0) |> 
  arrange(year, plotID)


# For the ordination analyses we divide the dataset by year

subplot_2018 <- subplot_presence |> 
  filter(year == 2018)
subplot_2019 <- subplot_presence |> 
  filter(year == 2019)
subplot_2021 <- subplot_presence |> 
  filter(year == 2021)
subplot_2023 <- subplot_presence |> 
  filter(year == 2023)

#subplot_change18_19 <- (subplot_2019 |> select(9:116)) - (subplot_2018 |> select(9:116))
#subplot_change19_21 <- (subplot_2021 |> select(9:116)) - (subplot_2019 |> select(9:116))
#subplot_change21_23 <- (subplot_2023 |> select(9:116)) - (subplot_2021 |> select(9:116))

set.seed(811)
subplot18_nmds2 <- metaMDS((subplot_2018 |> select(9:116)), k = 2, distance = "jaccard", trymax = 1000)
saveRDS(subplot18_nmds2, file = "Objects/subplot18_nmds2.rds")
subplot18_nmds2 <- readRDS(file = "Objects/subplot18_nmds2.rds")

subplot19_nmds2 <- metaMDS((subplot_2019 |> select(9:116)), k = 2, distance = "jaccard", trymax = 1000)
saveRDS(subplot19_nmds2, file = "Objects/subplot19_nmds2.rds")
subplot19_nmds2 <- readRDS(file = "Objects/subplot19_nmds2.rds")

subplot21_nmds2 <- metaMDS((subplot_2021 |> select(9:116)), k = 2, distance = "jaccard", trymax = 1000)
saveRDS(subplot21_nmds2, file = "Objects/subplot21_nmds2.rds")
subplot21_nmds2 <- readRDS(file = "Objects/subplot21_nmds2.rds")

subplot23_nmds2 <- metaMDS((subplot_2023 |> select(9:116)), k = 2, distance = "jaccard", trymax = 1000)
saveRDS(subplot23_nmds2, file = "Objects/subplot23_nmds2.rds")
subplot23_nmds2 <- readRDS(file = "Objects/subplot23_nmds2.rds")


plot18_nmds2_scores <- subplot_2018 |> 
  select(1:7) |> 
  mutate(NMDS1 = as_tibble(scores(plot18_nmds2, display = "sites"))$NMDS1) |> 
  mutate(NMDS2 = as_tibble(scores(plot18_nmds2, display = "sites"))$NMDS2)
plot19_nmds2_scores <- subplot_2019 |> 
  select(1:7) |> 
  mutate(NMDS1 = as_tibble(scores(plot19_nmds2, display = "sites"))$NMDS1) |> 
  mutate(NMDS2 = as_tibble(scores(plot19_nmds2, display = "sites"))$NMDS2)
plot21_nmds2_scores <- subplot_2021 |> 
  select(1:7) |> 
  mutate(NMDS1 = as_tibble(scores(plot21_nmds2, display = "sites"))$NMDS1) |> 
  mutate(NMDS2 = as_tibble(scores(plot21_nmds2, display = "sites"))$NMDS2)
plot23_nmds2_scores <- subplot_2023 |> 
  select(1:7) |> 
  mutate(NMDS1 = as_tibble(scores(plot23_nmds2, display = "sites"))$NMDS1) |> 
  mutate(NMDS2 = (as_tibble(scores(plot23_nmds2, display = "sites"))$NMDS2)*(-1)) # When plotting change it seemed the second axis might be rotated in 2023 compared to 2018

subplot_nmds2_scores <- rbind(plot18_nmds2_scores, plot19_nmds2_scores, plot21_nmds2_scores, plot23_nmds2_scores)

plot(subplot_nmds2_scores$NMDS1, subplot_nmds2_scores$NMDS2, type = "n")
points(plot18_nmds2_scores$NMDS1, plot18_nmds2_scores$NMDS2, pch = 1)
points(plot23_nmds2_scores$NMDS1, plot23_nmds2_scores$NMDS2, pch = 16)
for(i in 1:114) {
  arrows(x0 = plot18_nmds2_scores$NMDS1, y0 = plot18_nmds2_scores$NMDS2,
         x1 = plot23_nmds2_scores$NMDS1, y1 = plot23_nmds2_scores$NMDS2,
         length = 0.05)
}

#
#### Trying some DCA stuff----
plot_dca18 <- plot_2018 |> 
  select(8:125) |> 
  decorana()
summary(plot_dca18)

# Extracting DCA-axes for plot scores:
plot_dca18_1_sites <- scores(plot_dca18, display = "sites", origin = TRUE)[, 1]
# Note that origin=FALSE implies that origo of the ordination diagram is moved from the centroid to the lower end of each axis
plot_dca18_2_sites <- scores(plot_dca18, display = "sites", origin = TRUE)[, 2]

plot_dca18_1__species <- scores(plot_dca18, display = "species", origin = TRUE)[, 1]
plot_dca18_2__species <- scores(plot_dca18, display = "species", origin = TRUE)[, 2]

# Plotting DCA - with points:
plot(plot_dca18_1_sites, plot_dca18_2_sites, xlab = "DCA1", ylab = "DCA2", type = "n")
points(plot_dca18_1_sites, plot_dca18_2_sites, xlab = "DCA1", ylab = "DCA2", pch = 16)

# Subsets as different shapes:
plot(plot_dca18_1_sites, plot_dca18_2_sites, xlab = "DCA1", ylab = "DCA2", type = "n")
points(plot_dca18_1_sites[plot_2018$site == "Gudmedalen"], plot_dca18_2_sites[plot_2018$site == "Gudmedalen"], pch = 1)
points(plot_dca18_1_sites[plot_2018$site == "Lavisdalen"], plot_dca18_2_sites[plot_2018$site == "Lavisdalen"], pch = 2)
points(plot_dca18_1_sites[plot_2018$site == "Skjellingahaugen"], plot_dca18_2_sites[plot_2018$site == "Skjellingahaugen"], pch = 3)
points(plot_dca18_1_sites[plot_2018$site == "Ulvehaugen"], plot_dca18_2_sites[plot_2018$site == "Ulvehaugen"], pch = 4)
text(plot_dca18_1__species, plot_dca18_2__species, labels = names(plot_2018 |> select(8:125)), col = 2, cex = 0.75)
text(plot_dca18_1_sites, plot_dca18_2_sites, labels = plot_2018$plotID, cex = 0.75)
# Lav_5_5 and Lav_5_6 look different


