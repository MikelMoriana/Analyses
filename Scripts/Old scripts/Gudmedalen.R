# Libraries and data----

library(tidyverse)
library(vegan)

gudmedalen <- read.csv("data/Gudmedalen.csv") |> select(-1)
gudmedalen <- gudmedalen |> filter(year != 2022)
gudmedalen <- gudmedalen |> 
  mutate(presence = str_detect(value, "(?i)[1234odsjf]"), 
         presence = case_when(presence == TRUE ~ 1)) |> 
  relocate(presence, .after = species)

gud_wide <- pivot_wider(
  gudmedalen, 
  id_cols = c(year, site, plotID, block, plot, warming, treatment, subPlot), 
  names_from = species, 
  values_from = presence, 
  names_sort = TRUE
) |> 
  mutate(across(everything(), ~ replace_na(.x, 0)))

# All years----

gud_species <- gud_wide |> select(Ach_mil:Vio_tri)
gud_info <- gud_wide |> select(year:subPlot)


nmds_gud_sub2 <- metaMDS(gud_species, k = 2, distance = "jaccard")
nmds_gud_sub3 <- metaMDS(gud_species, k = 3, distance = "jaccard")


nmds_gud_sub2_scores <- as.data.frame(scores(nmds_gud_sub2)$sites)
nmds_gud_sub2_scores$year <- factor(gud_info$year)
nmds_gud_sub2_scores$plotID <- factor(gud_info$plotID)
nmds_gud_sub2_scores$block <- factor(gud_info$block)
nmds_gud_sub2_scores$plot <- factor(gud_info$plot)
nmds_gud_sub2_scores$subPlot <- factor(gud_info$subPlot)
nmds_gud_sub2_scores$warming <- factor(gud_info$warming)
nmds_gud_sub2_scores$treatment <- factor(gud_info$treatment)

nmds_gud_sub2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = year), size = 3) + 
  stat_ellipse(aes(color = year)) + 
  theme_bw()

nmds_gud_sub2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = block), size = 2) + 
  stat_ellipse(aes(color = block), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_sub2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = warming), size = 2) + 
  stat_ellipse(aes(color = warming), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_sub2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = treatment), size = 2) + 
  stat_ellipse(aes(color = treatment), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()


# 2023----

gud23 <- gud_wide |> filter(year == 2023)

gud23_species <- gud23 |> select(Ach_mil:Vio_tri)
gud23_info <- gud23 |> select(year:subPlot)

nmds_gud23_sub1 <- metaMDS(gud23_species, k = 1, distance = "jaccard", trymax = 1000)
nmds_gud23_sub2 <- metaMDS(gud23_species, k = 2, distance = "jaccard", trymax = 1000)
nmds_gud23_sub3 <- metaMDS(gud23_species, k = 3, distance = "jaccard", trymax = 1000)
nmds_gud23_sub4 <- metaMDS(gud23_species, k = 4, distance = "jaccard", trymax = 1000)
nmds_gud23_sub5 <- metaMDS(gud23_species, k = 5, distance = "jaccard", trymax = 1000)
saveRDS(nmds_gud23_sub1, "Objects/NMDS_Gudmedalen_2023_Subplot_1k.rds")
saveRDS(nmds_gud23_sub2, "Objects/NMDS_Gudmedalen_2023_Subplot_2k.rds")
saveRDS(nmds_gud23_sub3, "Objects/NMDS_Gudmedalen_2023_Subplot_3k.rds")
saveRDS(nmds_gud23_sub4, "Objects/NMDS_Gudmedalen_2023_Subplot_4k.rds")
saveRDS(nmds_gud23_sub5, "Objects/NMDS_Gudmedalen_2023_Subplot_5k.rds")

nmds_gud23_sub2_scores <- as.data.frame(scores(nmds_gud23_sub2)$sites)
nmds_gud23_sub2_scores$plotID <- factor(gud23_info$plotID)
nmds_gud23_sub2_scores$block <- factor(gud23_info$block)
nmds_gud23_sub2_scores$plot <- factor(gud23_info$plot)
nmds_gud23_sub2_scores$subPlot <- factor(gud23_info$subPlot)
nmds_gud23_sub2_scores$warming <- factor(gud23_info$warming)
nmds_gud23_sub2_scores$treatment <- factor(gud23_info$treatment)

nmds_gud23_sub2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = block), size = 2) + 
  stat_ellipse(aes(color = block), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud23_sub2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = warming), size = 2) + 
  stat_ellipse(aes(color = warming), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud23_sub2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = treatment), size = 2) + 
  stat_ellipse(aes(color = treatment), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()



# Plot level----

gud_plot <- gudmedalen |> 
  mutate(warming = ifelse(warming == "C", "F", warming)) |> 
  group_by(year, site, plotID, block, plot, warming, treatment, species, cover) |> 
  replace_na(list(moss = 0, lichen = 0, litter = 0, bare_ground = 0, rock = 0, poo = 0, fungus = 0, logger = 0)) |> 
  summarise(abundance = sum(presence))


# Abundances

gud_abund <- gud_plot |> 
  pivot_wider(id_cols = c(year, site, plotID, block, plot, warming, treatment), 
              names_from = species, 
              values_from = abundance, 
              names_sort = TRUE) |> 
  mutate(across(everything(), ~ replace_na(.x, 0)))

gud_abund_species <- gud_abund |> ungroup() |> select(Ach_mil:Vio_tri)
gud_abund_info <- gud_abund |> select(year:treatment)

nmds_gud_abund1 <- metaMDS(gud_abund_species, k = 1, distance = "jaccard", trymax = 1000)
nmds_gud_abund2 <- metaMDS(gud_abund_species, k = 2, distance = "jaccard", trymax = 1000)
nmds_gud_abund3 <- metaMDS(gud_abund_species, k = 3, distance = "jaccard", trymax = 1000)
nmds_gud_abund4 <- metaMDS(gud_abund_species, k = 4, distance = "jaccard", trymax = 1000)
nmds_gud_abund5 <- metaMDS(gud_abund_species, k = 5, distance = "jaccard", trymax = 1000)
# It seems two dimensions is enough. Lowers the stress quite a lot in regards to 1 dimension, and a stable solution is reached

nmds_gud_abund2_scores <- as.data.frame(scores(nmds_gud_abund2)$sites)
nmds_gud_abund2_scores$year <- factor(gud_abund_info$year)
nmds_gud_abund2_scores$plotID <- factor(gud_abund_info$plotID)
nmds_gud_abund2_scores$block <- factor(gud_abund_info$block)
nmds_gud_abund2_scores$plot <- factor(gud_abund_info$plot)
nmds_gud_abund2_scores$warming <- factor(gud_abund_info$warming)
nmds_gud_abund2_scores$treatment <- factor(gud_abund_info$treatment)
nmds_gud_abund2_scores <- nmds_gud_abund2_scores |> 
  mutate(warming = ifelse(warming == "F", "Control", "OTC")) |> 
  mutate(treatment = ifelse(treatment == "C", "Control", 
                            ifelse(treatment == "E", "Extant traits", "Novel traits")))

nmds_gud_abund2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = year), size = 2) + 
  stat_ellipse(aes(color = year), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_abund2_scores |> ggplot(aes(x = NMDS1, y = NMDS2, label = year)) + 
  geom_point(aes(color = block), size = 2) + 
  geom_text() + 
  stat_ellipse(aes(color = block), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_abund2_scores |> ggplot(aes(x = NMDS1, y = NMDS2, colour = warming)) + 
  geom_point(size = 2) + 
  stat_ellipse(lwd = 2) + 
  scale_color_viridis_d() + 
  labs(colour = "Warming") + 
  theme(legend.text = element_text(size = 15)) +
  theme_bw()

nmds_gud_abund2_scores |> ggplot(aes(x = NMDS1, y = NMDS2, colour = treatment)) + 
  geom_point(size = 2) + 
  stat_ellipse(aes(), lwd = 2) + 
  scale_colour_viridis_d() + 
  labs(colour = "Treatment") + 
  theme_bw()





# Cover

gud_cover <- gud_plot |> 
  pivot_wider(id_cols = c(year, site, plotID, block, plot, warming, treatment), 
              names_from = species, 
              values_from = cover, 
              names_sort = TRUE) |> 
  mutate(across(everything(), ~ replace_na(.x, 0)))

gud_cover_species <- gud_cover |> ungroup() |> select(Ach_mil:Vio_tri)
gud_cover_info <- gud_cover |> select(year:treatment)

nmds_gud_cover1 <- metaMDS(gud_cover_species, k = 1, distance = "jaccard", trymax = 1000)
nmds_gud_cover2 <- metaMDS(gud_cover_species, k = 2, distance = "jaccard", trymax = 1000)
nmds_gud_cover3 <- metaMDS(gud_cover_species, k = 3, distance = "jaccard", trymax = 1000)
nmds_gud_cover4 <- metaMDS(gud_cover_species, k = 4, distance = "jaccard", trymax = 1000)
nmds_gud_cover5 <- metaMDS(gud_cover_species, k = 5, distance = "jaccard", trymax = 1000)
# It seems two dimensions is enough. Lowers the stress quite a lot in regards to 1 dimension, and a stable solution is reached

nmds_gud_cover2_scores <- as.data.frame(scores(nmds_gud_cover2)$sites)
nmds_gud_cover2_scores$year <- factor(gud_cover_info$year)
nmds_gud_cover2_scores$plotID <- factor(gud_cover_info$plotID)
nmds_gud_cover2_scores$block <- factor(gud_cover_info$block)
nmds_gud_cover2_scores$plot <- factor(gud_cover_info$plot)
nmds_gud_cover2_scores$warming <- factor(gud_cover_info$warming)
nmds_gud_cover2_scores$treatment <- factor(gud_cover_info$treatment)

nmds_gud_cover2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = year), size = 2) + 
  stat_ellipse(aes(color = year), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_cover2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = block), size = 2) + 
  stat_ellipse(aes(color = block), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_cover2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = warming), size = 2) + 
  stat_ellipse(aes(color = warming), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_cover2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = treatment), size = 2) + 
  stat_ellipse(aes(color = treatment), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

# Plot level without transplants----

gud_wt_plot <- gud_plot |> 
  filter(!(treatment == "E" & species %in% c("Car_pil", "Ver_off", "Vio_can"))) |> 
  filter(!(treatment == "N" & species %in% c("Car_pal", "Hyp_mac", "Suc_pra")))

gud_t_plot <- gud_plot |> 
  filter((treatment == "E" & species %in% c("Car_pil", "Ver_off", "Vio_can")) | 
           (treatment == "N" & species %in% c("Car_pal", "Hyp_mac", "Suc_pra")))


# Abundances

gud_wt_abund <- gud_wt_plot |> 
  pivot_wider(id_cols = c(year, site, plotID, block, plot, warming, treatment), 
              names_from = species, 
              values_from = abundance, 
              names_sort = TRUE) |> 
  mutate(across(everything(), ~ replace_na(.x, 0)))

gud_wt_abund_species <- gud_wt_abund |> ungroup() |> select(Ach_mil:Vio_tri)
gud_wt_abund_info <- gud_wt_abund |> select(year:treatment)

# CANNOT DO THIS, SINCE IT ONLY APPLIES TO CERTAIN PLOTS. I COULD MAYBE LOOK ONLY AT TREATMENTS, SEE HOW THEY HAVE CHANGED WITH YEAR
gud_t_abund <- gud_t_plot |> 
  pivot_wider(id_cols = c(year, site, plotID, block, plot, warming, treatment), 
              names_from = species, 
              values_from = abundance, 
              names_sort = TRUE) |> 
  mutate(across(everything(), ~ replace_na(.x, 0)))

gud_t_abund_species <- gud_t_abund |> ungroup() |> select(Car_pal:Vio_can)
gud_t_abund_info <- gud_t_abund |> select(year:treatment)


nmds_gud_wt_abund1 <- metaMDS(gud_wt_abund_species, k = 1, distance = "jaccard", trymax = 1000)
nmds_gud_wt_abund2 <- metaMDS(gud_wt_abund_species, k = 2, distance = "jaccard", trymax = 1000)
nmds_gud_wt_abund3 <- metaMDS(gud_wt_abund_species, k = 3, distance = "jaccard", trymax = 1000)
nmds_gud_wt_abund4 <- metaMDS(gud_wt_abund_species, k = 4, distance = "jaccard", trymax = 1000)
nmds_gud_wt_abund5 <- metaMDS(gud_wt_abund_species, k = 5, distance = "jaccard", trymax = 1000)
# It seems two dimensions is enough. Lowers the stress quite a lot in regards to 1 dimension, and a stable solution is reached

nmds_gud_wt_abund2_scores <- as.data.frame(scores(nmds_gud_wt_abund2)$sites)
nmds_gud_wt_abund2_scores$year <- factor(gud_wt_abund_info$year)
nmds_gud_wt_abund2_scores$plotID <- factor(gud_wt_abund_info$plotID)
nmds_gud_wt_abund2_scores$block <- factor(gud_wt_abund_info$block)
nmds_gud_wt_abund2_scores$plot <- factor(gud_wt_abund_info$plot)
nmds_gud_wt_abund2_scores$warming <- factor(gud_wt_abund_info$warming)
nmds_gud_wt_abund2_scores$treatment <- factor(gud_wt_abund_info$treatment)

nmds_gud_wt_abund2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = year), size = 2) + 
  stat_ellipse(aes(color = year), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_wt_abund2_scores |> ggplot(aes(x = NMDS1, y = NMDS2, label = year)) + 
  geom_point(aes(color = block), size = 2) + 
  geom_text() + 
  stat_ellipse(aes(color = block), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_wt_abund2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = warming), size = 2) + 
  stat_ellipse(aes(color = warming), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_wt_abund2_scores |> ggplot(aes(x = NMDS1, y = NMDS2, colour = warming)) + 
  geom_point(aes(color = warming), size = 2) + 
  stat_ellipse(aes(colour = treatment), lwd = 2) + 
  scale_colour_viridis_d() +  
  theme_bw()





# Cover

gud_wt_cover <- gud_wt_plot |> 
  pivot_wider(id_cols = c(year, site, plotID, block, plot, warming, treatment), 
              names_from = species, 
              values_from = cover, 
              names_sort = TRUE) |> 
  mutate(across(everything(), ~ replace_na(.x, 0)))

gud_wt_cover_species <- gud_wt_cover |> ungroup() |> select(Ach_mil:Vio_tri)
gud_wt_cover_info <- gud_wt_cover |> select(year:treatment)

nmds_gud_wt_cover1 <- metaMDS(gud_wt_cover_species, k = 1, distance = "jaccard", trymax = 1000)
nmds_gud_wt_cover2 <- metaMDS(gud_wt_cover_species, k = 2, distance = "jaccard", trymax = 1000)
nmds_gud_wt_cover3 <- metaMDS(gud_wt_cover_species, k = 3, distance = "jaccard", trymax = 1000)
nmds_gud_wt_cover4 <- metaMDS(gud_wt_cover_species, k = 4, distance = "jaccard", trymax = 1000)
nmds_gud_wt_cover5 <- metaMDS(gud_wt_cover_species, k = 5, distance = "jaccard", trymax = 1000)
# It seems two dimensions is enough. Lowers the stress quite a lot in regards to 1 dimension, and a stable solution is reached

nmds_gud_wt_cover2_scores <- as.data.frame(scores(nmds_gud_wt_cover2)$sites)
nmds_gud_wt_cover2_scores$year <- factor(gud_wt_cover_info$year)
nmds_gud_wt_cover2_scores$plotID <- factor(gud_wt_cover_info$plotID)
nmds_gud_wt_cover2_scores$block <- factor(gud_wt_cover_info$block)
nmds_gud_wt_cover2_scores$plot <- factor(gud_wt_cover_info$plot)
nmds_gud_wt_cover2_scores$warming <- factor(gud_wt_cover_info$warming)
nmds_gud_wt_cover2_scores$treatment <- factor(gud_wt_cover_info$treatment)

nmds_gud_wt_cover2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = year), size = 2) + 
  stat_ellipse(aes(color = year), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_wt_cover2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = block), size = 2) + 
  stat_ellipse(aes(color = block), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_wt_cover2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = warming), size = 2) + 
  stat_ellipse(aes(color = warming), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

nmds_gud_wt_cover2_scores |> ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(color = treatment), size = 2) + 
  stat_ellipse(aes(color = treatment), lwd = 2) + 
  scale_color_viridis_d() + 
  theme_bw()

