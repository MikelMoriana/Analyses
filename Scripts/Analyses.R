## Libraries and files----

library(tidyverse)
library(ggplot2)
library(lme4)


community_clean <- read.csv("data/INCLINE_community_subplot.csv") |> 
  filter(!(year == 2022)) |> 
  mutate(block = as.factor(block)) |> 
  mutate(plot = as.factor(plot)) |> 
  mutate(subPlot = as.factor(subPlot)) |> 
  mutate(presence = as.factor(presence))


# In some analyses I want to remove the transplant species to see what the community looks like. 
community_clean |> filter(year == 2018 & ((treatment == "E" & species %in% c("Car_pil", "Ver_off", "Vio_can")) | (treatment == "N" & species %in% c("Car_pal", "Hyp_mac", "Suc_pra"))))
# None of the transplant species were present in their corresponding plots in 2018, so we assume all occurrence of the species are due to them having been transplanted there, with not natural occurrences

# There are some species we think may have come with the transplants
new_species <- anti_join(
  (community_clean |> filter(year != 2018 & treatment != "C" & !(species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Ver_off", "Vio_can"))) |> 
     select(site, species) |> distinct()), 
  (community_clean |> filter(year == 2018) |> select(site, species) |> distinct())
) |> 
  arrange(site, species)

# By looking at the list, we reckon the following species may have come with the transplants
# Gudmedalen: Tri_pra and Vio_tri
# Lavisdalen: Ach_mil, Lys_eur, Pot_ere, Pru_vul, Tri_pra and Tri_rep
# Skjellingahaugen: Ach_mil, Ane_nem, Pru_vul, Sil_vul and Ste_gra
# Ulvehaugen: Gal_bor, Lot_cor, Pot_ere, Pru_vul, Ran_aur, Tri_pra and Tri_rep

community_clean |> filter(treatment == "C" & ((site == "Gudmedalen" & species %in% c("Tri_pra", "Vio_tri")) | 
                            (site == "Lavisdalen" & species %in% c("Ach_mil", "Lys_eur", "Pot_ere", "Pru_vul", "Tri_pra", "Tri_rep")) | 
                            (site == "Skjellingahaugen" & species %in% c("Ach_mil", "Ane_nem", "Pru_vul", "Sil_vul", "Ste_gra")) | 
                            (site == "Ulvehaugen" & species %in% c("Gal_bor", "Lot_cor", "Pot_ere", "Pru_vul", "Ran_aur", "Tri_pra", "Tri_rep"))))
# None of the species appear in the control plots. It may be because they are not abundant, but we believe they came with the transplants, so we remove them from the dataset

community_clean_w <- community_clean |> 
  filter(!(treatment == "E" & species %in% c("Car_pil", "Ver_off", "Vio_can"))) |>  
  filter(!(treatment == "N" & species %in% c("Car_pal", "Hyp_mac", "Suc_pra"))) |> 
  filter(!(site == "Gudmedalen" & species %in% c("Tri_pra", "Vio_tri"))) |> 
  filter(!(site == "Lavisdalen" & species %in% c("Ach_mil", "Lys_eur", "Pot_ere", "Pru_vul", "Tri_pra", "Tri_rep"))) |> 
  filter(!(site == "Skjellingahaugen" & species %in% c("Ach_mil", "Ane_nem", "Pru_vul", "Sil_vul", "Ste_gra"))) |> 
  filter(!(site == "Ulvehaugen" & species %in% c("Gal_bor", "Lot_cor", "Pot_ere", "Pru_vul", "Ran_aur", "Tri_pra", "Tri_rep")))


## Functions----
gg_histo <- function(dat, xaxis, yaxis, rows, columns) {
  ggplot(data = dat, aes(x = {{xaxis}}, y = {{yaxis}}, group = {{xaxis}})) + 
    geom_jitter() + 
    geom_boxplot(alpha = 0.02) + 
    facet_grid(rows = vars({{rows}}), cols = vars({{columns}}), scales = "free") + 
    theme_bw()
}


## Plot level. Backwards selection----

plot_richness <- community_clean |> 
  select(year, site, plotID, block, plot, warming, treatment, transplant, species) |> 
  distinct() |> 
  group_by(year, site, plotID, block, plot, warming, transplant, treatment) |> 
  summarise(richness = n())

plot_richness |> gg_histo(year, richness, warming, treatment) + 
  ylim(14, 38) + 
  labs(x = "Year", y = "Richness")

model_plot_richness <- glmer(richness ~ year * site * warming * treatment + (1|plotID), family = poisson, data = plot_richness) # Need to scale year


#### Rescaling year
# Since year will appear in many intereactions through the process, it is easier to create a new dataframe with year scaled
plot_y_richness <- plot_richness
plot_y_richness$year <- scale(plot_y_richness$year)

model_plot_y_richness <- glmer(richness ~ year * site * warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)


# 2. Remove the four-way interaction
model_plot_y_richness2 <- glmer(richness ~ year * site * warming + year * site * treatment + year * warming * treatment + site * warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness2, model_plot_y_richness)
summary(model_plot_y_richness) # We remove the four-way interaction


# 3. Remove year:site:warming, year:site:treatment, year:warming:treatment, site:warming:treatment
model_plot_y_richness3a <- glmer(richness ~ year * site * treatment + year * warming * treatment + site * warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness3b <- glmer(richness ~ year * site * warming + year * warming * treatment + site * warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness3c <- glmer(richness ~ year * site * warming + year * site * treatment + site * warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness3d <- glmer(richness ~ year * site * warming + year * site * treatment + year * warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness3a, model_plot_y_richness2)
anova(model_plot_y_richness3b, model_plot_y_richness2)
anova(model_plot_y_richness3c, model_plot_y_richness2)
anova(model_plot_y_richness3d, model_plot_y_richness2)
summary(model_plot_y_richness2) # We remove year:site:warming (3a)


# 4. Remove year:site:treatment, year:warming:treatment, site:warming:treatment
model_plot_y_richness4a <- glmer(richness ~ year * site + year * warming * treatment + site * warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness4b <- glmer(richness ~ year * site * treatment + year * warming + site * warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness4c <- glmer(richness ~ year * site * treatment + year * warming * treatment + site * warming + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness4a, model_plot_y_richness3a)
anova(model_plot_y_richness4b, model_plot_y_richness3a)
anova(model_plot_y_richness4c, model_plot_y_richness3a)
summary(model_plot_y_richness3a) # We remove year:site:treatment (4a)


# 5. Remove year:warming:treatment, site:warming:treatment,  year:site
model_plot_y_richness5a <- glmer(richness ~ year * site + year * warming + year * treatment + site * warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness5b <- glmer(richness ~ year * site + year * warming * treatment + site * warming + site * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness5c <- glmer(richness ~ year * warming * treatment + site * warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness5a, model_plot_y_richness4a)
anova(model_plot_y_richness5b, model_plot_y_richness4a)
anova(model_plot_y_richness5c, model_plot_y_richness4a)
summary(model_plot_y_richness4a) # We remove site:warming:treatment (5b)


# 6. Remove year:warming:treatment, year:site, site:warming, site:treatment
model_plot_y_richness6a <- glmer(richness ~ year * site + year * warming + year * treatment + site * warming + site * treatment + warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness6b <- glmer(richness ~ year * warming * treatment + site * warming + site * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness6c <- glmer(richness ~ year * site + year * warming * treatment + site * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness6d <- glmer(richness ~ year * site + year * warming * treatment + site * warming + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness6a, model_plot_y_richness5b)
anova(model_plot_y_richness6b, model_plot_y_richness5b)
anova(model_plot_y_richness6c, model_plot_y_richness5b)
anova(model_plot_y_richness6d, model_plot_y_richness5b)
summary(model_plot_y_richness5b) # We remove site:treatment (6d)


# 7. Remove year:warming:treatment, year:site, site:warming
model_plot_y_richness7a <- glmer(richness ~ year * site + year * warming + year * treatment + site * warming + warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness7b <- glmer(richness ~ year * warming * treatment + site * warming + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness7c <- glmer(richness ~ year * site + year * warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness7a, model_plot_y_richness6d)
anova(model_plot_y_richness7b, model_plot_y_richness6d)
anova(model_plot_y_richness7c, model_plot_y_richness6d)
summary(model_plot_y_richness6d) # We remove year:warming:treatment (7a)


# 8. Remove year:site, year:warming, year:treatment, site:warming, warming:treatment
model_plot_y_richness8a <- glmer(richness ~ year * warming + year * treatment + site * warming + warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness8b <- glmer(richness ~ year * site + year * treatment + site * warming + warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness8c <- glmer(richness ~ year * site + year * warming + site * warming + warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness8d <- glmer(richness ~ year * site + year * warming + year * treatment + warming * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness8e <- glmer(richness ~ year * site + year * warming + year * treatment + site * warming + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness8a, model_plot_y_richness7a)
anova(model_plot_y_richness8b, model_plot_y_richness7a)
anova(model_plot_y_richness8c, model_plot_y_richness7a)
anova(model_plot_y_richness8d, model_plot_y_richness7a)
anova(model_plot_y_richness8e, model_plot_y_richness7a)
summary(model_plot_y_richness7a) # We remove warming:treatment (8e)


# 9. Remove year:site, year:warming, year:treatment, site:warming
model_plot_y_richness9a <- glmer(richness ~ year * warming + year * treatment + site * warming + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness9b <- glmer(richness ~ year * site + year * treatment + site * warming + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness9c <- glmer(richness ~ year * site + year * warming + site * warming + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness9d <- glmer(richness ~ year * site + year * warming + year * treatment + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness9a, model_plot_y_richness8e)
anova(model_plot_y_richness9b, model_plot_y_richness8e)
anova(model_plot_y_richness9c, model_plot_y_richness8e)
anova(model_plot_y_richness9d, model_plot_y_richness8e)
summary(model_plot_y_richness8e) # We remove year:warming (9b)


# 10. Remove year:site, year:treatment, site:warming
model_plot_y_richness10a <- glmer(richness ~ year * treatment + site * warming + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness10b <- glmer(richness ~ year * site + site * warming + treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness10c <- glmer(richness ~ year * site + warming + year * treatment + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness10a, model_plot_y_richness9b)
anova(model_plot_y_richness10b, model_plot_y_richness9b)
anova(model_plot_y_richness10c, model_plot_y_richness9b)
summary(model_plot_y_richness9b) # We remove site:warming (10c)


# 11. Remove year:site, year:treatment, warming
model_plot_y_richness11a <- glmer(richness ~ year + site + warming + year * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness11b <- glmer(richness ~ year * site + warming + treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness11c <- glmer(richness ~ year * site + year * treatment + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness11a, model_plot_y_richness10c)
anova(model_plot_y_richness11b, model_plot_y_richness10c)
anova(model_plot_y_richness11c, model_plot_y_richness10c)
summary(model_plot_y_richness10c) # We remove warming (11c)


# 12. Remove year:site, year:treatment
model_plot_y_richness12a <- glmer(richness ~ year + site + year * treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness12b <- glmer(richness ~ year * site + year + treatment + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness12a, model_plot_y_richness11c)
anova(model_plot_y_richness12b, model_plot_y_richness11c)
summary(model_plot_y_richness11c) # We remove year:site (12a). There might some interaction between year and site in Skjellingahaugen, but not clear


# 13. Remove year:treatment, site
model_plot_y_richness13a <- glmer(richness ~ year + site + treatment + (1|plotID), family = poisson, data = plot_y_richness)
model_plot_y_richness13b <- glmer(richness ~ year * treatment + (1|plotID), family = poisson, data = plot_y_richness)

anova(model_plot_y_richness13a, model_plot_y_richness12a)
anova(model_plot_y_richness13b, model_plot_y_richness12a)
summary(model_plot_y_richness12a) # We do not remove any more terms. We keep the model: richness ~ year * treatment + site

plot_richness |> gg_histo(year, richness, site, treatment) + 
  ylim(14, 38) + 
  labs(x = "Year", y = "Richness")


## Plot level. Forwards selection----

model_plot_richness_f <- glmer(richness ~ (1|plotID), family = poisson, data = plot_richness)

# 2. We add year, site, warming, treatment
model_plot_richness_f2a <- glmer(richness ~ scale(year) + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f2b <- glmer(richness ~ site + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f2c <- glmer(richness ~ warming + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f2d <- glmer(richness ~ treatment + (1|plotID), family = poisson, data = plot_richness)

anova(model_plot_richness_f2a, model_plot_richness_f)
anova(model_plot_richness_f2b, model_plot_richness_f) # We add site
anova(model_plot_richness_f2c, model_plot_richness_f)
anova(model_plot_richness_f2d, model_plot_richness_f)


# 3. We add year, warming, treatment
model_plot_richness_f3a <- glmer(richness ~ scale(year) + site + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f3b <- glmer(richness ~ site + warming + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f3c <- glmer(richness ~ site + treatment + (1|plotID), family = poisson, data = plot_richness)

anova(model_plot_richness_f3a, model_plot_richness_f2b)
anova(model_plot_richness_f3b, model_plot_richness_f2b)
anova(model_plot_richness_f3c, model_plot_richness_f2b) # We add treatment


# 4. We add year, warming, site:treatment
model_plot_richness_f4a <- glmer(richness ~ scale(year) + site + treatment + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f4b <- glmer(richness ~ site + warming + treatment + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f4c <- glmer(richness ~ site * treatment + (1|plotID), family = poisson, data = plot_richness)

anova(model_plot_richness_f4a, model_plot_richness_f3c) # We add year
anova(model_plot_richness_f4b, model_plot_richness_f3c)
anova(model_plot_richness_f4c, model_plot_richness_f3c)


# 5. We add warming, year:site, year:treatment, site:treatment
model_plot_richness_f5a <- glmer(richness ~ scale(year) + site + warming + treatment + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f5b <- glmer(richness ~ scale(year) * site + treatment + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f5c <- glmer(richness ~ scale(year) * treatment + site + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f5d <- glmer(richness ~ scale(year) + site * treatment + (1|plotID), family = poisson, data = plot_richness)

anova(model_plot_richness_f5a, model_plot_richness_f4a)
anova(model_plot_richness_f5b, model_plot_richness_f4a)
anova(model_plot_richness_f5c, model_plot_richness_f4a) # We add year:treatment
anova(model_plot_richness_f5d, model_plot_richness_f4a)


# 6. We add warming, year:site, site:treatment
model_plot_richness_f6a <- glmer(richness ~ scale(year) * treatment + site + warming + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f6b <- glmer(richness ~ scale(year) * site + scale(year) * treatment + (1|plotID), family = poisson, data = plot_richness)
model_plot_richness_f6c <- glmer(richness ~ scale(year) * treatment + site * treatment + (1|plotID), family = poisson, data = plot_richness)

anova(model_plot_richness_f6a, model_plot_richness_f5c)
anova(model_plot_richness_f6b, model_plot_richness_f5c)
anova(model_plot_richness_f6c, model_plot_richness_f5c)
summary(model_plot_richness_f5c) # We do not add any more terms, we keep the model: richness ~ year * treatment + site


# We get the same model as with backwards selection


## Plot level, without considering the transplant species----

w_plot_richness <- community_clean_w |> 
  select(year, site, plotID, block, plot, warming, treatment, species) |> 
  distinct() |> 
  group_by(year, site, plotID, block, plot, warming, treatment) |> 
  summarise(richness = n())

w_plot_richness |> gg_histo(year, richness, warming, treatment) + 
  ylim(14, 38) + 
  labs(x = "Year", y = "Richness")

model_w_plot_richness <- glmer(richness ~ year * site * warming * treatment + (1|plotID), family = poisson, data = w_plot_richness) # Need to rescale year


#### Rescaling year

w_plot_y_richness <- w_plot_richness
w_plot_y_richness$year <- scale(w_plot_y_richness$year)

model_w_plot_y_richness <- glmer(richness ~ year * site * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)


# 2. Remove the four-way interaction
model_w_plot_y_richness2 <- glmer(richness ~ year * site * warming + year * site * treatment + year * warming * treatment + site * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness2, model_w_plot_y_richness)
summary(model_w_plot_y_richness) # We remove the four-way interaction


# 3. Remove year:site:warming, year:site:treatment, year:warming:treatment, site:warming:treatment
model_w_plot_y_richness3a <- glmer(richness ~ year * site * treatment + year * warming * treatment + site * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness3b <- glmer(richness ~ year * site * warming + year * warming * treatment + site * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness3c <- glmer(richness ~ year * site * warming + year * site * treatment + site * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness3d <- glmer(richness ~ year * site * warming + year * site * treatment + year * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness3a, model_w_plot_y_richness2)
anova(model_w_plot_y_richness3b, model_w_plot_y_richness2)
anova(model_w_plot_y_richness3c, model_w_plot_y_richness2)
anova(model_w_plot_y_richness3d, model_w_plot_y_richness2)
summary(model_w_plot_y_richness2) # We remove year:site:warming (3a)


# 4. Remove year:site:treatment, year:warming:treatment, site:warming:treatment
model_w_plot_y_richness4a <- glmer(richness ~ year * site + year * warming * treatment + site * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness4b <- glmer(richness ~ year * site * treatment + year * warming + site * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness4c <- glmer(richness ~ year * site * treatment + site * warming + year * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness4a, model_w_plot_y_richness3a)
anova(model_w_plot_y_richness4b, model_w_plot_y_richness3a)
anova(model_w_plot_y_richness4c, model_w_plot_y_richness3a)
summary(model_w_plot_y_richness3a) # We remove year:site:treatment (4a)


# 5. Remove year:site, year:warming:treatment, site:warming:treatment
model_w_plot_y_richness5a <- glmer(richness ~ year * warming * treatment + site * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness5b <- glmer(richness ~ year * site + year * warming + year * treatment + site * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness5c <- glmer(richness ~ year * site + year * warming * treatment + site * warming + site * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness5a, model_w_plot_y_richness4a)
anova(model_w_plot_y_richness5b, model_w_plot_y_richness4a)
anova(model_w_plot_y_richness5c, model_w_plot_y_richness4a)
summary(model_w_plot_y_richness4a) # We remove site:warming:treatment (5c)


# 6. Remove year:site, year:warming:treatment, site:warming, site:treatment
model_w_plot_y_richness6a <- glmer(richness ~ year * warming * treatment + site * warming + site * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness6b <- glmer(richness ~ year * site + year * warming + year * treatment + site * warming + site * treatment + warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness6c <- glmer(richness ~ year * site + year * warming * treatment + site * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness6d <- glmer(richness ~ year * site + year * warming * treatment + site * warming + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness6a, model_w_plot_y_richness5c)
anova(model_w_plot_y_richness6b, model_w_plot_y_richness5c)
anova(model_w_plot_y_richness6c, model_w_plot_y_richness5c)
anova(model_w_plot_y_richness6d, model_w_plot_y_richness5c)
summary(model_w_plot_y_richness5c) # We remove site:treatment (6d)


# 7. Remove year:warming:treatment, year:site, site:warming
model_w_plot_y_richness7a <- glmer(richness ~ year * site + year * warming + year * treatment + site * warming + warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness7b <- glmer(richness ~ year * warming * treatment + site * warming + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness7c <- glmer(richness ~ year * site + year * warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness7a, model_w_plot_y_richness6d)
anova(model_w_plot_y_richness7b, model_w_plot_y_richness6d)
anova(model_w_plot_y_richness7c, model_w_plot_y_richness6d)
summary(model_w_plot_y_richness6d) # We remove year:warming:treatment (7a)


# 8. Remove year:site, year:warming, year:treatment, site:warming, warming:treatment
model_w_plot_y_richness8a <- glmer(richness ~ year * warming + year * treatment + site * warming + warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness8b <- glmer(richness ~ year * site + year * treatment + site * warming + warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness8c <- glmer(richness ~ year * site + year * warming + site * warming + warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness8d <- glmer(richness ~ year * site + year * warming + year * treatment + warming * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness8e <- glmer(richness ~ year * site + year * warming + year * treatment + site * warming + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness8a, model_w_plot_y_richness7a)
anova(model_w_plot_y_richness8b, model_w_plot_y_richness7a)
anova(model_w_plot_y_richness8c, model_w_plot_y_richness7a)
anova(model_w_plot_y_richness8d, model_w_plot_y_richness7a)
anova(model_w_plot_y_richness8e, model_w_plot_y_richness7a)
summary(model_w_plot_y_richness7a) # We remove warming:treatment (8e)


# 9. Remove year:site, year:warming, year:treatment, site:warming
model_w_plot_y_richness9a <- glmer(richness ~ year * warming + year * treatment + site * warming + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness9b <- glmer(richness ~ year * site + year * treatment + site * warming + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness9c <- glmer(richness ~ year * site + year * warming + site * warming + treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness9d <- glmer(richness ~ year * site + year * warming + year * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness9a, model_w_plot_y_richness8e)
anova(model_w_plot_y_richness9b, model_w_plot_y_richness8e)
anova(model_w_plot_y_richness9c, model_w_plot_y_richness8e)
anova(model_w_plot_y_richness9d, model_w_plot_y_richness8e)
summary(model_w_plot_y_richness8e) # We remove year:warming (9b)


# 10. Remove year:site, year:treatment, site:warming
model_w_plot_y_richness10a <- glmer(richness ~ year * treatment + site * warming + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness10b <- glmer(richness ~ year * site + site * warming + treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness10c <- glmer(richness ~ year * site + warming + year * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness10a, model_w_plot_y_richness9b)
anova(model_w_plot_y_richness10b, model_w_plot_y_richness9b)
anova(model_w_plot_y_richness10c, model_w_plot_y_richness9b)
summary(model_w_plot_y_richness9b) # We remove year:site (10a)


# 11. Remove year:treatment, site:warming
model_w_plot_y_richness11a <- glmer(richness ~ year + site * warming + treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness11b <- glmer(richness ~ year * treatment + site + warming + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness11a, model_w_plot_y_richness10a)
anova(model_w_plot_y_richness11b, model_w_plot_y_richness10a)
summary(model_w_plot_y_richness10a) # We remove site:warming (11b)


# 12. Remove year:treatment, site, warming
model_w_plot_y_richness12a <- glmer(richness ~ year + treatment + site + warming + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness12b <- glmer(richness ~ year * treatment + warming + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness12c <- glmer(richness ~ year * treatment + site + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness12a, model_w_plot_y_richness11b)
anova(model_w_plot_y_richness12b, model_w_plot_y_richness11b)
anova(model_w_plot_y_richness12c, model_w_plot_y_richness11b)
summary(model_w_plot_y_richness11b) # We remove warming (12c)


# 13. Remove year:treatment, site
model_w_plot_y_richness13a <- glmer(richness ~ year + site + treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness13b <- glmer(richness ~ year * treatment + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness13a, model_w_plot_y_richness12c)
anova(model_w_plot_y_richness13b, model_w_plot_y_richness12c)
summary(model_w_plot_y_richness12c) # Year and treatment E may have some interaction, but not clear. We remove year:treatment (13a)

w_plot_richness |> 
  ggplot(aes(x = year, y = richness, group = year)) + 
  geom_jitter() + 
  geom_boxplot(alpha = 0.02) + 
  facet_grid(rows = vars(site), cols = vars(treatment), scales = "free") + 
  theme_bw()


# If we removed the interaction
# 14. Remove year, site, treatment
model_w_plot_y_richness14a <- glmer(richness ~ site + treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness14b <- glmer(richness ~ year + treatment + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness14c <- glmer(richness ~ year + site + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness14a, model_w_plot_y_richness13a)
anova(model_w_plot_y_richness14b, model_w_plot_y_richness13a)
anova(model_w_plot_y_richness14c, model_w_plot_y_richness13a)
summary(model_w_plot_y_richness13a) # We remove treatment (14c)


# 15. Remove year, site
model_w_plot_y_richness15a <- glmer(richness ~ site + (1|plotID), family = poisson, data = w_plot_y_richness)
model_w_plot_y_richness15b <- glmer(richness ~ year + (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness15a, model_w_plot_y_richness14c)
anova(model_w_plot_y_richness15b, model_w_plot_y_richness14c)
summary(model_w_plot_y_richness14c) # We remove year (15a)


# 16. Remove site
model_w_plot_y_richness16 <- glmer(richness ~ (1|plotID), family = poisson, data = w_plot_y_richness)

anova(model_w_plot_y_richness16, model_w_plot_y_richness15a)
summary(model_w_plot_y_richness15a) # We don not remove any more terms. We keep the model richness ~ site (15a)

w_plot_richness |> 
  ggplot(aes(x = site, y = richness)) + 
  geom_jitter() + 
  geom_boxplot(alpha = 0.02) + 
  theme_bw()


## Plot level, without considering the transplant species. Forwards selection----

model_w_plot_richness_f <- glmer(richness ~ (1|plotID), family = poisson, data = w_plot_richness)

# 2. We add year, site, warming, treatment
model_w_plot_richness_f2a <- glmer(richness ~ year + (1|plotID), family = poisson, data = w_plot_richness)
model_w_plot_richness_f2b <- glmer(richness ~ site + (1|plotID), family = poisson, data = w_plot_richness)
model_w_plot_richness_f2c <- glmer(richness ~ warming + (1|plotID), family = poisson, data = w_plot_richness)
model_w_plot_richness_f2d <- glmer(richness ~ treatment + (1|plotID), family = poisson, data = w_plot_richness)

anova(model_w_plot_richness_f2a, model_w_plot_richness_f)
anova(model_w_plot_richness_f2b, model_w_plot_richness_f)# We add site
anova(model_w_plot_richness_f2c, model_w_plot_richness_f)
anova(model_w_plot_richness_f2d, model_w_plot_richness_f)


# 2. We add year, warming, treatment
model_w_plot_richness_f3a <- glmer(richness ~ year + site + (1|plotID), family = poisson, data = w_plot_richness)
model_w_plot_richness_f3b <- glmer(richness ~ site + warming + (1|plotID), family = poisson, data = w_plot_richness)
model_w_plot_richness_f3c <- glmer(richness ~ site + treatment + (1|plotID), family = poisson, data = w_plot_richness)

anova(model_w_plot_richness_f3a, model_w_plot_richness_f2b)
anova(model_w_plot_richness_f3b, model_w_plot_richness_f2b)
anova(model_w_plot_richness_f3c, model_w_plot_richness_f2b)
# We do not add any more terms

summary(model_w_plot_richness_f2b)
# This is different from the backward-selected model. However, in that one we kept the interaction between year and treatment, even though it seemed to have just a small effect. The forward model suggests that interaction is not relevant and we can remove it. Doing so in the backward-selected model ends up with only site as model predictor


## Subplot level. Backwards selection----

subplot_richness <- community_clean |> 
  select(year, site, plotID, block, plot, warming, treatment, subPlot, species) |> 
  distinct() |> 
  group_by(year, site, plotID, block, plot, warming, treatment, subPlot) |> 
  summarise(richness = n())

subplot_richness |> gg_histo(year, richness, warming, treatment) + 
  ylim(0, 20) + 
  labs(x = "Year", y = "Richness")

model_subplot_richness <- glmer(richness ~ year * site * warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_richness)


#### Rescaling year

subplot_y_richness <- subplot_richness
subplot_y_richness$year <- scale(subplot_y_richness$year)

model_subplot_y_richness <- glmer(richness ~ year * site * warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)


# 2. Remove the four-way interaction
model_subplot_y_richness2 <- glmer(richness ~ year * site * warming + year * site * treatment + year * warming * treatment + site * warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)

anova(model_subplot_y_richness2, model_subplot_y_richness) # It seems the full model might be best
summary(model_subplot_y_richness) # The four-way interaction does not seem that relevant. We continue and see what we get


# 3. Remove year:site:warming, year:site:treatment, year:warming:treatment, site:warming:treatment
model_subplot_y_richness3a <- glmer(richness ~ year * site * treatment + year * warming * treatment + site * warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness3b <- glmer(richness ~ year * site * warming + year * warming * treatment + site * warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness3c <- glmer(richness ~ year * site * warming + year * site * treatment + site * warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness3d <- glmer(richness ~ year * site * warming + year * site * treatment + year * warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)

anova(model_subplot_y_richness3a, model_subplot_y_richness2)
anova(model_subplot_y_richness3b, model_subplot_y_richness2)
anova(model_subplot_y_richness3c, model_subplot_y_richness2)
anova(model_subplot_y_richness3d, model_subplot_y_richness2)
summary(model_subplot_y_richness2) # We remove year:site:treatment (3b)


# 4. Remove year:site:warming, year:warming:treatment, site:warming:treatment
model_subplot_y_richness4a <- glmer(richness ~ year * site + year * warming * treatment + site * warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness4b <- glmer(richness ~ year * site * warming + year * treatment + site * warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness4c <- glmer(richness ~ year * site * warming + year * warming * treatment + site * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)

anova(model_subplot_y_richness4a, model_subplot_y_richness3b)
anova(model_subplot_y_richness4b, model_subplot_y_richness3b)
anova(model_subplot_y_richness4c, model_subplot_y_richness3b)
summary(model_subplot_y_richness3b) # We remove site:warming:treatment (4c) (Lavisdalen might interact with warming and treatment N, but not clearly)


# 5. Remove year:site:warming, year:warming:treatment,  site:treatment
model_subplot_y_richness5a <- glmer(richness ~ year * site + year * warming * treatment + site * warming + site * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness5b <- glmer(richness ~ year * site * warming + year * treatment + site * treatment + warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness5c <- glmer(richness ~ year * site * warming + year * warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)

anova(model_subplot_y_richness5a, model_subplot_y_richness4c)
anova(model_subplot_y_richness5b, model_subplot_y_richness4c)
anova(model_subplot_y_richness5c, model_subplot_y_richness4c)
summary(model_subplot_y_richness4c) # We remove site:treatment (5c)


# 6. Remove year:site:warming, year:warming:treatment
model_subplot_y_richness6a <- glmer(richness ~ year * site + year * warming * treatment + site * warming + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness6b <- glmer(richness ~ year * site * warming + year * treatment + warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)

anova(model_subplot_y_richness6a, model_subplot_y_richness5c) # Both three-way interactions might be relevant
anova(model_subplot_y_richness6b, model_subplot_y_richness5c) # Both three-way interactions might be relevant
summary(model_subplot_y_richness5c) # The year:site:warming does not seem relevant. We remove it (6a)


# 7. Remove year:warming:treatment, year:site, site:warming
model_subplot_y_richness7a <- glmer(richness ~ year * site + year * warming + year * treatment + site * warming + warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness7b <- glmer(richness ~ year * warming * treatment + site * warming + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness7c <- glmer(richness ~ year * site + year * warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)

anova(model_subplot_y_richness7a, model_subplot_y_richness6a) # The three-way interaction may not be relevant
anova(model_subplot_y_richness7b, model_subplot_y_richness6a)
anova(model_subplot_y_richness7c, model_subplot_y_richness6a)
summary(model_subplot_y_richness6a) # The three-way interaction might be relevant. We remove year:warming:treatment (7a) and see what we get


# 8. Remove year:site, year:warming, year:treatment, site:warming, warming:treatment
model_subplot_y_richness8a <- glmer(richness ~ year * warming + year * treatment + site * warming + warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness8b <- glmer(richness ~ year * site + year * treatment + site * warming + warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness8c <- glmer(richness ~ year * site + year * warming + site * warming + warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness8d <- glmer(richness ~ year * site + year * warming + year * treatment + warming * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness8e <- glmer(richness ~ year * site + year * warming + year * treatment + site * warming + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)

anova(model_subplot_y_richness8a, model_subplot_y_richness7a)
anova(model_subplot_y_richness8b, model_subplot_y_richness7a)
anova(model_subplot_y_richness8c, model_subplot_y_richness7a)
anova(model_subplot_y_richness8d, model_subplot_y_richness7a)
anova(model_subplot_y_richness8e, model_subplot_y_richness7a)
summary(model_subplot_y_richness7a) # We remove warming:treatment (8e)


# 9. Remove year:site, year:warming, year:treatment, site:warming
model_subplot_y_richness9a <- glmer(richness ~ year * warming + year * treatment + site * warming + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness9b <- glmer(richness ~ year * site + year * treatment + site * warming + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness9c <- glmer(richness ~ year * site + year * warming + site * warming + treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness9d <- glmer(richness ~ year * site + year * warming + year * treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)

anova(model_subplot_y_richness9a, model_subplot_y_richness8e)
anova(model_subplot_y_richness9b, model_subplot_y_richness8e)
anova(model_subplot_y_richness9c, model_subplot_y_richness8e)
anova(model_subplot_y_richness9d, model_subplot_y_richness8e)
summary(model_subplot_y_richness8e) # We remove year:warming (9b)


# 10. Remove year:site, year:treatment, site:warming
model_subplot_y_richness10a <- glmer(richness ~ year * treatment + site * warming + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness10b <- glmer(richness ~ year * site + site * warming + treatment + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)
model_subplot_y_richness10c <- glmer(richness ~ year * site + year * treatment + warming + (1|plotID/subPlot), family = poisson, data = subplot_y_richness)

anova(model_subplot_y_richness10a, model_subplot_y_richness9b)
anova(model_subplot_y_richness10b, model_subplot_y_richness9b)
anova(model_subplot_y_richness10c, model_subplot_y_richness9b)
summary(model_subplot_y_richness9b) # We don't remove any more terms. We keep the model richness ~ year*site + year*warming + year*treatment

subplot_richness |> gg_histo(year, richness, site, treatment) + 
  ylim(0, 15) + 
  labs(x = "Year", y = "Richness")








### Any species found only in any of the treatments, only in treatments, only in controls?






## Subplot level. Forwards selection----

model_subplot_richness_f <- glmer(richness ~ (1|plotID/subPlot), family = poisson, data = subplot_richness)

# 2. We add year, site, warming, treatment
model_subplot_richness_f2a <- glmer(richness ~ year + (1|plotID/subPlot), family = poisson, data = subplot_richness)
model_subplot_richness_f2b <- glmer(richness ~ site + (1|plotID/subPlot), family = poisson, data = subplot_richness)
model_subplot_richness_f2c <- glmer(richness ~ warming + (1|plotID/subPlot), family = poisson, data = subplot_richness)
model_subplot_richness_f2d <- glmer(richness ~ treatment + (1|plotID/subPlot), family = poisson, data = subplot_richness)

anova(model_subplot_richness_f2a, model_subplot_richness_f)
anova(model_subplot_richness_f2b, model_subplot_richness_f)
anova(model_subplot_richness_f2c, model_subplot_richness_f)
anova(model_subplot_richness_f2d, model_subplot_richness_f)
summary(model_subplot_richness_f2a) # We add year (2a)


# 3. We add site, warming, treatment
model_subplot_richness_f3a <- glmer(richness ~ year + site + (1|plotID/subPlot), family = poisson, data = subplot_richness)
model_subplot_richness_f3b <- glmer(richness ~ year + warming + (1|plotID/subPlot), family = poisson, data = subplot_richness)
model_subplot_richness_f3c <- glmer(richness ~ year + treatment + (1|plotID/subPlot), family = poisson, data = subplot_richness)

anova(model_subplot_richness_f3a, model_subplot_richness_f2a)
anova(model_subplot_richness_f3b, model_subplot_richness_f2a)
anova(model_subplot_richness_f3c, model_subplot_richness_f2a)
summary(model_subplot_richness_f3a) # We add site (3a)


# 4. We add warming, treatment, year:site
model_subplot_richness_f4a <- glmer(richness ~ year + site + warming + (1|plotID/subPlot), family = poisson, data = subplot_richness)
model_subplot_richness_f4b <- glmer(richness ~ year + site + treatment + (1|plotID/subPlot), family = poisson, data = subplot_richness)
model_subplot_richness_f4c <- glmer(richness ~ year * site + (1|plotID/subPlot), family = poisson, data = subplot_richness)

anova(model_subplot_richness_f4a, model_subplot_richness_f3a)
anova(model_subplot_richness_f4b, model_subplot_richness_f3a)
anova(model_subplot_richness_f4c, model_subplot_richness_f3a)
summary(model_subplot_richness_f4c) # We add year:site (4c)


# 5. We add warming, treatment
model_subplot_richness_f5a <- glmer(richness ~ year * site + warming + (1|plotID/subPlot), family = poisson, data = subplot_richness)
model_subplot_richness_f5b <- glmer(richness ~ year * site + treatment + (1|plotID/subPlot), family = poisson, data = subplot_richness)

anova(model_subplot_richness_f5a, model_subplot_richness_f4c)
anova(model_subplot_richness_f5b, model_subplot_richness_f4c)
summary(model_subplot_richness_f5b) # We add treatment (5b)


# 6. We add warming, year:treatment, site:treatment
model_subplot_richness_f6a <- glmer(richness ~ year * site + warming + treatment + (1|plotID/subPlot), family = poisson, data = subplot_richness)
model_subplot_richness_f6b <- glmer(richness ~ year * site + treatment + year:treatment + (1|plotID/subPlot), family = poisson, data = subplot_richness)
model_subplot_richness_f6c <- glmer(richness ~ year * site + treatment + site:treatment + (1|plotID/subPlot), family = poisson, data = subplot_richness)

anova(model_subplot_richness_f6a, model_subplot_richness_f5b)
anova(model_subplot_richness_f6b, model_subplot_richness_f5b)
anova(model_subplot_richness_f6c, model_subplot_richness_f5b)
summary(model_subplot_richness_f6b) # We add year:treatment (6b)


# 7. We add warming, site:treatment
model_subplot_richness_f7a <- glmer(richness ~ year * site + warming + treatment + year:treatment + (1|plotID/subPlot), family = poisson, data = subplot_richness)
model_subplot_richness_f7b <- glmer(richness ~ year * site + treatment + year:treatment + site:treatment + (1|plotID/subPlot), family = poisson, data = subplot_richness)

anova(model_subplot_richness_f7a, model_subplot_richness_f6b)
anova(model_subplot_richness_f7b, model_subplot_richness_f6b)
# We stop here. We keep the model richness ~ year * site + treatment + year:treatment









# Subplots with succisa----

subplot_richness_novel <- community_clean |> 
  filter(treatment == "N") |> 
  mutate(succisa = ifelse(species == "Suc_pra", "Yes", "No")) |> 
  select(year, site, plotID, block, plot, warming, treatment, subPlot, species, succisa) |> 
  distinct() |> 
  group_by(year, site, plotID, block, plot, warming, treatment, subPlot) |> 
  summarise(richness = n(), succisa = ifelse(any(succisa == "Yes"), "Yes", "No"))

subplot_richness_novel |> gg_histo(richness) + 
  ylim(0, 20) + 
  labs(x = "Year", y = "Richness")


subplot_richness_succisa <- subplot_richness_novel |> 
  group_by(year, warming, treatment, succisa) |> 
  summarise(mean_richness = mean(richness), sd_richness = sd(richness))

subplot_richness_succisa |> gg_bar(mean_richness) + 
  ylim(0, 12) + 
  labs(x = "Year", y = "Average Richness")


model_subplot_succisa_richness <- glmer(richness ~ year + warming + succisa + (1|site/block/plot/subPlot), family = poisson, data = subplot_richness_novel)
summary(model_subplot_succisa_richness)
# This seems to indicate that subplots with succisa have larger richness. But it could be that succisa is crawling into species-rich subplots. How to account for that?
# Maybe do the same removing succisa from the data set, while keeping the "succisa" tag?