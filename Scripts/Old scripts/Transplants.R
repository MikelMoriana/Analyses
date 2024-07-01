# Libraries and data files----

library(tidyverse)
library(turfmapper)
library(pipebind)
library(ggpubr)
library(lme4)

community_clean <- read.delim("data/INCLINE_community_subplot_fixed.csv", sep = ",")
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

grid <- turfmapper::make_grid(ncol = 7, nrow = 5) # We create the 5x7 grid for the turfmapper

# We look at the two blocks where all 6 plots were kept (across all study sites)----

ggplot_skj_5_1 <- richnessplot(community_4years_richness, "Skj_5_1")
ggplot_skj_5_2 <- richnessplot(community_4years_richness, "Skj_5_2")
ggplot_skj_5_3 <- richnessplot(community_4years_richness, "Skj_5_3")
ggplot_skj_5_4 <- richnessplot(community_4years_richness, "Skj_5_4")
ggplot_skj_5_5 <- richnessplot(community_4years_richness, "Skj_5_5")
ggplot_skj_5_6 <- richnessplot(community_4years_richness, "Skj_5_6")
ggplot_skj_5 <- ggarrange(ggplot_skj_5_1, ggplot_skj_5_2, ggplot_skj_5_3, ggplot_skj_5_4, ggplot_skj_5_5, ggplot_skj_5_6, 
                          ncol = 1, nrow = 6)
ggplot_skj_5

ggplot_ulv_6_1 <- richnessplot(community_4years_richness, "Ulv_6_1")
ggplot_ulv_6_2 <- richnessplot(community_4years_richness, "Ulv_6_2")
ggplot_ulv_6_3 <- richnessplot(community_4years_richness, "Ulv_6_3")
ggplot_ulv_6_4 <- richnessplot(community_4years_richness, "Ulv_6_4")
ggplot_ulv_6_5 <- richnessplot(community_4years_richness, "Ulv_6_5")
ggplot_ulv_6_6 <- richnessplot(community_4years_richness, "Ulv_6_6")
ggplot_ulv_6 <- ggarrange(ggplot_ulv_6_1, ggplot_ulv_6_2, ggplot_ulv_6_3, ggplot_ulv_6_4, ggplot_ulv_6_5, ggplot_ulv_6_6, 
                          ncol = 1, nrow = 6)
ggplot_ulv_6

# We look at richness across sites, warming and treatment----
# Looking at actual richness

richness_summary <- community_4years_richness |> 
  group_by(site, warming, treatment) |> 
  summarise(mean = mean(richness), sd = sd(richness))

richness_ggplot <- richness_summary |> 
  mutate(warming = ifelse(warming == "C", "Control", "OTC"),
         treatment = ifelse(treatment == "C", "Control", ifelse(treatment == "E", "Extant", "Novel"))) |> 
  ggplot(aes(x = warming, y = mean, fill = treatment)) + 
  geom_col(position = "dodge",
           width = 0.6) + 
  scale_x_discrete(expand = c(0.4, 0.4)) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  facet_wrap(~site, strip.position = "top") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.spacing = unit(0, "cm"), 
        strip.text = element_text(size = 14, face = "bold")) +
  labs(x = "Warming", y = "Average richness, including transplants") +
  guides(fill = guide_legend(title = "Transplant treatment"))

richness_model <- glmer(log(richness) ~ site + warming + treatment + year + (plotID|subPlot), 
                       data = community_4years_richness)

# Looking at richness if we don't consider the introduced species

community_original <- community_clean |> 
  filter(!(treatment == "E" & species %in% c("Car_pil", "Ver_off", "Vio_can"))) |> 
  filter(!(treatment == "N" & species %in% c("Car_pal", "Hyp_mac", "Suc_pra")))

community_original_4years <- community_original |> filter(year != 2022) # In 2022 only the center subplots were studied
community_original_4years_presence <- pivot_wider(
  community_original_4years,
  id_cols = c(site, plotID, warming, treatment, year, subPlot, moss, lichen, litter, rock, poo, fungus, bare_ground, logger),
  names_from = species,
  values_from = presence,
  names_sort = TRUE,
) |> 
  mutate(across(everything(), ~ replace_na(.x, 0)))

richness_original_4years <- community_original_4years_presence |> 
  select(Ach_mil:Vio_tri) |> # We choose only the species. Double-check that these are the first and last species in the tibble
  rowSums()

community_original_4years_richness <- community_original_4years_presence |> 
  cbind(richness_original_4years) |> 
  rename(richness = richness_original_4years) |> 
  relocate(richness, .after = subPlot)

richness_original_summary <- community_original_4years_richness |> 
  group_by(site, warming, treatment) |> 
  summarise(mean = mean(richness), sd = sd(richness))

richness_original_ggplot <- richness_original_summary |> 
  mutate(warming = ifelse(warming == "C", "Control", "OTC"),
         treatment = ifelse(treatment == "C", "Control", ifelse(treatment == "E", "Extant", "Novel"))) |> 
  ggplot(aes(x = warming, y = mean, fill = treatment)) + 
  geom_col(position = "dodge",
           width = 0.6) + 
  scale_x_discrete(expand = c(0.4, 0.4)) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  facet_wrap(~site, strip.position = "top") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.spacing = unit(0, "cm"), 
        strip.text = element_text(size = 14, face = "bold")) +
  labs(x = "Warming", y = "Average richness without transplants") +
  guides(fill = guide_legend(title = "Transplant treatment"))

richness_ggplot
richness_original_ggplot
