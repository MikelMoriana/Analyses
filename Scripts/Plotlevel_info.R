## Libraries and data----

library(tidyverse)
library(vegan)
library(lme4)
library(glmmTMB)
library(lmerTest)


plotlv <- read.csv("data_cleaned/INCLINE_community_plotlevel_info.csv")
plotlv_wide <- plotlv |> 
  filter(year != 2022) |> 
  mutate(s_year = scale(year)) |> 
  relocate(s_year, .after = year) |> 
  mutate(site = factor(site, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) |> 
  select(-c("recorder", "writer", "date")) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  select(-c("total_poo_cover", "total_fungus_cover")) |> 
  mutate(vascular_biomass = vegetation_cover * vegetation_height_mean) |> 
  relocate(vegetation_height_mean, .after = vegetation_cover) |> 
  relocate(vascular_biomass, .after = vegetation_height_mean) |> 
  mutate(moss_biomass = total_moss_cover * moss_depth_mean) |> 
  relocate(moss_depth_mean, .after = total_moss_cover) |> 
  relocate(moss_biomass, .after = moss_depth_mean) |> 
  na.omit()


hist(plotlv_wide$vegetation_cover)
hist(plotlv_wide$vegetation_height_mean)
hist(plotlv_wide$vascular_biomass)
hist(plotlv_wide$total_moss_cover)
hist(plotlv_wide$moss_depth_mean)
hist(plotlv_wide$moss_biomass)
hist(plotlv_wide$total_lichen_cover)
hist(plotlv_wide$total_litter_cover)

model_factors <- c("s_year", "site", "warming", "treatment")
model_factors_r <- c("s_year", "warming", "treatment")

## Functions----

gg_boxplot <- function(dat, xaxis, yaxis, rows, columns) {
  ggplot(data = dat, aes(x = {{xaxis}}, y = {{yaxis}}, group = {{xaxis}})) + 
    geom_jitter() + 
    geom_boxplot(alpha = 0.02) + 
    facet_grid(rows = vars({{rows}}), cols = vars({{columns}}), scales = "free") + 
    theme_bw()
}


backwards_selection <- function(model) {
  original_formula <- formula(model)
  original_formula_str <- paste(deparse(original_formula), collapse = " ")
  if ("lmerModLmerTest" %in% class(model)) {
    data <- model.frame(model)
  } else {
    data <- model$frame
  }
  terms <- attr(terms(original_formula), "term.labels")
  terms <- terms[!grepl("\\|", terms)] # Exclude random effects
  # Function to check if all components of a term appear in other terms
  is_part_of_other_term <- function(term, terms) {
    if (length(terms) == 0) {
      return(FALSE)
    } else {
      if (length(terms) == 1) {
        return(FALSE)
      } else {
        components <- unlist(strsplit(term, split = ":"))
        matches <- sapply(components, function(comp) grepl(paste0("\\b", comp, "\\b"), terms))
        return(apply(matches, 1, all) %>% any)
      }
    }
  }
  terms_update <- terms[!sapply(seq_along(terms), function(i) is_part_of_other_term(terms[i], terms[-i]))]
  anova_results <- list()
  for (term in terms_update) {
    formula_string <- paste0(original_formula_str, " - ", term)
    new_formula <- as.formula(formula_string, env = environment(original_formula))
    new_model <- update(model, formula = new_formula)
    anova_result <- anova(model, new_model)
    anova_results[[term]] <- list(anova = anova_result)
  }
  list(anova = anova_results, summary = summary(model))
}


forwards_selection <- function(model, factors) {
  original_formula <- formula(model)
  original_formula_str <- paste(deparse(original_formula), collapse = " ")
  original_formula_str <- paste(deparse(original_formula), collapse = " ")
  if ("lmerModLmerTest" %in% class(model)) {
    data <- model.frame(model)
  } else {
    data <- model$frame
  }
  terms <- attr(terms(original_formula), "term.labels")
  terms <- terms[!grepl("\\|", terms)] # Exclude random effects
  # Generate all possible combinations of factors not already in the model
  all_combinations <- unlist(lapply(seq_along(factors), function(n) combn(factors, n, paste, collapse = ":")), recursive = FALSE)
  all_combinations <- all_combinations[!all_combinations %in% terms]
  # Function to add new terms
  adding_new_terms <- function(all_combinations, terms) {
    # Remove terms from all_combinations
    new_combinations <- sapply(all_combinations, function(x) {
      for (term in terms) {
        x <- gsub(term, "", x)
      }
      return(x)
    })
    # Keep only strings with no colons, one colon and nothing else, or exactly one colon with non-colon characters on both sides
    valid_combinations <- new_combinations[grepl("^$|^[^:]*$|^:$", new_combinations)]
    # Create a data frame that links the original terms to the remaining terms
    terms_df <- data.frame(original = all_combinations, remaining = new_combinations, stringsAsFactors = FALSE)
    # Find the original terms that correspond to the remaining valid combinations
    original_terms <- terms_df$original[terms_df$remaining %in% valid_combinations]
    return(original_terms)
  }
  terms_update <- adding_new_terms(all_combinations, terms)
  anova_results <- list()
  for (term in terms_update) {
    formula_string <- paste0(original_formula_str, " + ", term)
    new_formula <- as.formula(formula_string, env = environment(original_formula))
    new_model <- update(model, formula = new_formula)
    anova_result <- anova(model, new_model)
    anova_results[[term]] <- list(formula = formula_string, anova = anova_result, summary = summary(new_model))
  }
  anova_results
}


## Ordination----

plotlv_metadata <- plotlv_wide |> select(year:treatment)
plotlv_measured <- plotlv_wide |> select(vegetation_cover:total_rock_cover)

set.seed(811)
plotlv_nmds2 <- metaMDS(plotlv_measured, k = 2, distance = "bray", trymax = 1000)
saveRDS(nmds_plot_measured, "Objects/plotlv_nmds2.rds")
plotlv_nmds2 <- readRDS("Objects/plotlv_nmds2.rds")

tar_load("plotlv_nmds2")
tar_load("plotlv_nmds3")
tar_load("plotlv_nmds4")

plotlv_nmds2_sites <- plotlv_nmds2 |> scores("sites") |> as.data.frame() |> cbind2(x = plotlv_metadata)
plotlv_nmds2_species <- plotlv_nmds2 |> scores("species") |> as.data.frame()
plotlv_nmds2_species <- plotlv_nmds2 |> mutate(species = rownames(plotlv_nmds2_species))


# Years

nmds_plot_year_hulls <- nmds_plot_sites %>%
  split(.$year) %>%
  lapply(function(df) df[chull(df[c("NMDS1", "NMDS2")]), ]) %>%
  do.call(rbind, .)

ggplot() + 
  geom_polygon(data = nmds_plot_year_hulls, aes(x = NMDS1, y = NMDS2, fill = factor(year), group = factor(year)), alpha = 0.30) + 
  geom_text(data = nmds_plot_species, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5) +  
  geom_point(data = nmds_plot_sites, aes(x = NMDS1, y = NMDS2, shape = factor(year), colour = factor(year)), size = 4) + 
  coord_equal() +
  theme_bw()

## TESTING----

ggplot() + 
  geom_polygon(data = nmds_plot_year_hulls, aes(x = NMDS1, y = NMDS2, fill = factor(year), group = factor(year)), alpha = 0.30) + 
  geom_text(data = nmds_plot_species, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5) +  
  geom_point(data = nmds_plot_sites, aes(x = NMDS1, y = NMDS2, shape = factor(year), colour = factor(year)), size = 4) + 
  facet_wrap(warming ~ site, nrow = 2) + 
  coord_equal() +
  theme_bw()




# Sites

nmds_plot_site_hulls <- nmds_plot_sites %>%
  split(.$site) %>%
  lapply(function(df) df[chull(df[c("NMDS1", "NMDS2")]), ]) %>%
  do.call(rbind, .)

ggplot() + 
  geom_polygon(data = nmds_plot_site_hulls, aes(x = NMDS1, y = NMDS2, fill = site, group = site), alpha = 0.30) + 
  geom_text(data = nmds_plot_species, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5) +  
  geom_point(data = nmds_plot_sites, aes(x = NMDS1, y = NMDS2, shape = site, colour = site), size = 4) + 
  coord_equal() +
  theme_bw()


# Warming

nmds_plot_warming_hulls <- nmds_plot_sites %>% 
  split(.$warming) %>%
  lapply(function(df) df[chull(df[c("NMDS1", "NMDS2")]), ]) %>%
  do.call(rbind, .)

ggplot() + 
  geom_polygon(data = nmds_plot_warming_hulls, aes(x = NMDS1, y = NMDS2, fill = warming, group = warming), alpha = 0.30) + 
  geom_text(data = nmds_plot_species, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5) +  
  geom_point(data = nmds_plot_sites, aes(x = NMDS1, y = NMDS2, shape = warming, colour = warming), size = 4) + 
  scale_colour_manual(values=c("W" = "red", "C" = "blue")) +
  coord_equal() +
  theme_bw()


# Transplants

nmds_plot_transplant_hulls <- nmds_plot_sites %>% 
  split(.$treatment) %>%
lapply(function(df) df[chull(df[c("NMDS1", "NMDS2")]), ]) %>%
  do.call(rbind, .)

ggplot() + 
  geom_polygon(data = nmds_plot_transplant_hulls, aes(x = NMDS1, y = NMDS2, fill = treatment, group = treatment), alpha = 0.30) + 
  geom_text(data = nmds_plot_species, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5) +  
  geom_point(data = nmds_plot_sites, aes(x = NMDS1, y = NMDS2, shape = treatment, colour = treatment), size = 4) + 
  coord_equal() +
  theme_bw()


#### NMDS1 Backwards selection----

nmds_plot_sites <- nmds_plot_sites |> 
  mutate(s_year = scale(year)) |> 
  mutate(o_year = year - min(year))



# We start with the full model and remove terms gradually

test_1 <- lmer(NMDS1 ~ site * warming * treatment + (1 | site:block), data =  nmds_plot_sites[nmds_plot_sites$year == 2018,])
summary(test_1)

model_plot_ordination1_1 <- lmer(NMDS1 ~ o_year * site * warming * treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination1_1) # We remove the four-way interaction (s_year:Skjellingahaugen:W:N??, s_year:Skjellingahaugen:W:E?)

model_plot_ordination1_2 <- lmer(NMDS1 ~ (s_year + site + warming + treatment)^3 + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination1_2) # We remove year:warming:treatment

model_plot_ordination1_3 <- lmer(NMDS1 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:warming + s_year:site:treatment + site:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination1_3) # We remove site:warming:treatment

model_plot_ordination1_4 <- lmer(NMDS1 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:warming + s_year:site:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination1_4) # We remove s_year:site:treatment

model_plot_ordination1_5 <- lmer(NMDS1 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:warming + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination1_5) # We remove s_year:treatment

model_plot_ordination1_6 <- lmer(NMDS1 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + site:warming + site:treatment + warming:treatment + s_year:site:warming + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination1_6) # We remove s_year:site:warming (s_year:Gudmedalen:W?)

model_plot_ordination1_7 <- lmer(NMDS1 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + site:warming + site:treatment + warming:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination1_7) # We remove site:warming

model_plot_ordination1_8 <- lmer(NMDS1 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + site:treatment + warming:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination1_8) # We stop


#### NMDS1 Forwards selection----

# We start with the null model and add terms gradually

model_plot_ordination1_f1 <- lmer(NMDS1 ~ (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination1_f1, model_factors) # We add s_year

model_plot_ordination1_f2 <- lmer(NMDS1 ~ s_year + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination1_f2, model_factors) # We add site

model_plot_ordination1_f3 <- lmer(NMDS1 ~ s_year + site + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination1_f3, model_factors) # We add s_year:site

model_plot_ordination1_f4 <- lmer(NMDS1 ~ s_year + site + s_year:site + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination1_f4, model_factors) # We stop

nmds_plot_sites |> gg_boxplot(year, NMDS1, treatment, site) + 
  ylim(-0.8, 0.6) +
  labs(x = "Year", y = "NMDS1")


#### NMDS2 Backwards selection----

# We start with the full model and remove terms gradually

model_plot_ordination2_1 <- lmer(NMDS2 ~ s_year * site * warming * treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_1) # We remove the four-way interaction

model_plot_ordination2_2 <- lmer(NMDS2 ~ (s_year + site + warming + treatment)^3 + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_2) # We remove s_year:site:warming

model_plot_ordination2_3 <- lmer(NMDS2 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + s_year:warming:treatment + site:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_3) # We remove site:warming:treatment

model_plot_ordination2_4 <- lmer(NMDS2 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + s_year:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_4) # We remove site:warming

model_plot_ordination2_5 <- lmer(NMDS2 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:treatment + warming:treatment + s_year:site:treatment + s_year:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_5) # We remove s_year:site:treatment (s_year:Lavisdalen:N??)

model_plot_ordination2_6 <- lmer(NMDS2 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:treatment + warming:treatment + s_year:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_6) # We remove site:treatment (Lavisdalen:N??)

model_plot_ordination2_7 <- lmer(NMDS2 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + warming:treatment + s_year:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_7) # We remove s_year:warming:treatment (s_year:W:N??)

model_plot_ordination2_8 <- lmer(NMDS2 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + warming:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_8) # We remove s_year:treatment

model_plot_ordination2_9 <- lmer(NMDS2 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + warming:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_9) # We remove warming:treatment

model_plot_ordination2_10 <- lmer(NMDS2 ~ s_year + site + warming + treatment + s_year:site + s_year:warming + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_10) # We remove s_year:warming

model_plot_ordination2_11 <- lmer(NMDS2 ~ s_year + site + warming + treatment + s_year:site + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_11) # We remove warming

model_plot_ordination2_12 <- lmer(NMDS2 ~ s_year + site + treatment + s_year:site + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_12) # We remove treatment

model_plot_ordination2_13 <- lmer(NMDS2 ~ s_year + site + s_year:site + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_13) # We stop
# It seems Lavisdalen N might be peculiar


#### NMDS2 Forwards selection----

# We start with the null model and add terms gradually

model_plot_ordination2_f1 <- lmer(NMDS2 ~ (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination2_f1, model_factors) # We add s_year

model_plot_ordination2_f2 <- lmer(NMDS2 ~ s_year + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination2_f2, model_factors) # We add site

model_plot_ordination2_f3 <- lmer(NMDS2 ~ s_year + site + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination2_f3, model_factors) # We add s_year:site

model_plot_ordination2_f4 <- lmer(NMDS2 ~ s_year + site + s_year:site + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination2_f4, model_factors) # We add treatment

model_plot_ordination2_f5 <- lmer(NMDS2 ~ s_year + site + treatment + s_year:site + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination2_f5, model_factors) # We could maybe add site:treatment

model_plot_ordination2_f6 <- lmer(NMDS2 ~ s_year + site + treatment + s_year:site + site:treatment + (1 | site:block) + (1 | site:block:plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination2_f6, model_factors) # We stop

nmds_plot_sites |> gg_boxplot(year, NMDS2, treatment, site) + 
  ylim(-0.5, 0.5) +
  labs(x = "Year", y = "NMDS2")


## Model Vegetation cover----

plot_vegetation_cover <- plotlv_wide |> select(1:8) |> na.omit()

plot_vegetation_cover |> gg_boxplot(year, vegetation_cover, site, warming) + 
  ylim(24, 101) +
  labs(x = "Year", y = "Vegetation cover")

# Year is in a very large scale, I use scale() to reduce it. And I use beta regression, so I adjust the percentages to the (0, 1) range. Since it's an open range, I cannot include 1, so I reduce the 1 values by a small amount. I also give it a shorter name
plot_vegcov <- plot_vegetation_cover |> 
  mutate(adjusted_vegetation_cover = pmin(pmax(plot_vegetation_cover$vegetation_cover/100, 0.001), 1 - 0.001)) |> 
  mutate(s_year = scale(year))


#### Backwards selection----

# We start with the full model and remove terms gradually

model_vegetation_cover1 <- glmmTMB(adjusted_vegetation_cover ~ s_year * site * warming * treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover1) # We remove the four-way interaction

model_vegetation_cover2 <- glmmTMB(adjusted_vegetation_cover ~ (s_year + site + warming + treatment)^3 + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover2) # We remove site:Warming:treatment

model_vegetation_cover3 <- glmmTMB(adjusted_vegetation_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:warming + s_year:site:treatment + s_year:warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover3) # We remove s_year:warming:treatment

model_vegetation_cover4 <- glmmTMB(adjusted_vegetation_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:warming + s_year:site:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover4) # We remove s_year:site:warming

model_vegetation_cover5 <- glmmTMB(adjusted_vegetation_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover5) # We remove s_year:warming

model_vegetation_cover6 <- glmmTMB(adjusted_vegetation_cover ~ s_year + site + warming + treatment + s_year:site + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover6) # We remove s_year:site:treatment (s_year:Skjellingahaugen:E?)

model_vegetation_cover7 <- glmmTMB(adjusted_vegetation_cover ~ s_year + site + warming + treatment + s_year:site + s_year:treatment + site:warming + site:treatment + warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover7) # We remove site:treatment

model_vegetation_cover8 <- glmmTMB(adjusted_vegetation_cover ~ s_year + site + warming + treatment + s_year:site + s_year:treatment + site:warming + warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover8) # We remove s_year:treatment

model_vegetation_cover9 <- glmmTMB(adjusted_vegetation_cover ~ s_year + site + warming + treatment + s_year:site + site:warming + warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover9) # The interaction warming:treatment might be relevant. I continue removing it and see where we get

model_vegetation_cover10 <- glmmTMB(adjusted_vegetation_cover ~ s_year + site + warming + treatment + s_year:site + site:warming + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover10) # We remove treatment

model_vegetation_cover11 <- glmmTMB(adjusted_vegetation_cover ~ s_year + site + warming + s_year:site + site:warming + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover11) # We stop
# There have been two points where we could have stopped instead of removed more interactions. We compare it with the forwards selected model


#### Forwards selection----

# We start with the null model and add terms gradually

model_vegetation_cover_f1 <- glmmTMB(adjusted_vegetation_cover ~ (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
forwards_selection(model_vegetation_cover_f1, model_factors) # We add s_year

model_vegetation_cover_f2 <- glmmTMB(adjusted_vegetation_cover ~ s_year + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
forwards_selection(model_vegetation_cover_f2, model_factors) # We add site

model_vegetation_cover_f3 <- glmmTMB(adjusted_vegetation_cover ~ s_year + site + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
forwards_selection(model_vegetation_cover_f3, model_factors) # We add s_year:site

model_vegetation_cover_f4 <- glmmTMB(adjusted_vegetation_cover ~ s_year + site + s_year:site + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_vegcov)
forwards_selection(model_vegetation_cover_f4, model_factors) # We stop

# It seems warming and treatment are not relevant. It seems the interactions site:warming and warming:treatment might be relevant, but not the factors by themselves

plot_vegetation_cover |> gg_boxplot(year, vegetation_cover, treatment, site) + 
  ylim(24, 101) +
  labs(x = "Year", y = "Vegetation cover")


## Model Vegetation height----

plot_vegetation_height <- plotlv_wide |> select(1:7, 16) |> na.omit()

plot_vegetation_height |> gg_boxplot(year, vegetation_height_mean, site, warming) + 
  ylim(10, 225) +
  labs(x = "Year", y = "Vegetation height")

# Year is in a very large scale, I use scale() to reduce it. Since height is a continuous, not bound variable, I use a linear  model
plot_veghei <- plot_vegetation_height |> 
  mutate(s_year = scale(year))


#### Backwards selection----

# We start with the full model and remove terms gradually

model_vegetation_height1 <- lmer(vegetation_height_mean ~ s_year * site * warming * treatment + (1 | site:block) + (1 | site:block:plot), data = plot_veghei)
backwards_selection(model_vegetation_height1) # We remove the four-way interaction (s_year:Gudmedalen:W:N?)

model_vegetation_height2 <- lmer(vegetation_height_mean ~ (s_year + site + warming + treatment)^3 + (1 | site:block) + (1 | site:block:plot), data = plot_veghei)
backwards_selection(model_vegetation_height2) # We remove s_year:site:treatment

model_vegetation_height3 <- lmer(vegetation_height_mean ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:warming + s_year:warming:treatment + site:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_veghei)
backwards_selection(model_vegetation_height3) # We remove s_year:site:warming

model_vegetation_height4 <- lmer(vegetation_height_mean ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:warming:treatment + site:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_veghei)
backwards_selection(model_vegetation_height4) # We remove s_year:warming:treatment

model_vegetation_height5 <- lmer(vegetation_height_mean ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + site:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_veghei)
backwards_selection(model_vegetation_height5) # We remove s_year:treatment

model_vegetation_height6 <- lmer(vegetation_height_mean ~ s_year + site + warming + treatment + s_year:site + s_year:warming + site:warming + site:treatment + warming:treatment + site:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_veghei)
backwards_selection(model_vegetation_height6) # We remove site:warming:treatment

model_vegetation_height7 <- lmer(vegetation_height_mean ~ s_year + site + warming + treatment + s_year:site + s_year:warming + site:warming + site:treatment + warming:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_veghei)
backwards_selection(model_vegetation_height7) # We stop


#### Forwards selection----

# We start with the null model and add terms gradually

model_vegetation_height_f1 <- lmer(vegetation_height_mean ~ (1 | site:block) + (1 | site:block:plot), data = plot_veghei)
forwards_selection(model_vegetation_height_f1, model_factors) # We add s_year

model_vegetation_height_f2 <- lmer(vegetation_height_mean ~ s_year + (1 | site:block) + (1 | site:block:plot), data = plot_veghei)
forwards_selection(model_vegetation_height_f2, model_factors) # We add site

model_vegetation_height_f3 <- lmer(vegetation_height_mean ~ s_year + site + (1 | site:block) + (1 | site:block:plot), data = plot_veghei)
forwards_selection(model_vegetation_height_f3, model_factors) # We may add s_year:site

model_vegetation_height_f4 <- lmer(vegetation_height_mean ~ s_year + site + s_year:site + (1 | site:block) + (1 | site:block:plot), data = plot_veghei)
forwards_selection(model_vegetation_height_f4, model_factors) # We stop
# It seems warming and treatment are not relevant. It seems the interactions s_year:warming, site:warming, site:treatment and warming:treatment might be relevant, but not the factors by themselves

plot_vegetation_height |> gg_boxplot(warming, vegetation_height_mean, treatment, site) + 
  ylim(9, 225) +
  labs(x = "Year", y = "Vegetation height")


## Model Moss cover----

plot_moss_cover <- plotlv_wide |> select(1:7, 9) |> na.omit()

plot_moss_cover |> gg_boxplot(year, total_moss_cover, site, warming) + 
  ylim(-1, 87) +
  labs(x = "Year", y = "Moss cover")

# Year is in a very large scale, I use scale() to reduce it. And I use beta regression, so I adjust the percentages to the (0, 1) range. Since it's an open range, I cannot include 0, so I increase the 0 values by a small amount. I also give it a shorter name
plot_moscov <- plot_moss_cover |> 
  mutate(adjusted_moss_cover = pmin(pmax(plot_moss_cover$total_moss_cover/100, 0.001), 1 - 0.001)) |> 
  mutate(s_year = scale(year))


#### Backwards selection----

# We start with the full model and remove terms gradually

model_moss_cover1 <- glmmTMB(adjusted_moss_cover ~ s_year * site * warming * treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover1) # We remove the four-way interaction (s_year:Skjellingahaugen:W:N??)

model_moss_cover2 <- glmmTMB(adjusted_moss_cover ~ (s_year + site + warming + treatment)^3 + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover2) # We remove s_year:site:warming

model_moss_cover3 <- glmmTMB(adjusted_moss_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + s_year:warming:treatment + site:warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover3) # We remove site:warming:treatment

model_moss_cover4 <- glmmTMB(adjusted_moss_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + s_year:warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover4) # We remove s_year:warming:treatment

model_moss_cover5 <- glmmTMB(adjusted_moss_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover5) # We remove s_year:site:treatment (s_year:Lavisdalen:N??, s_year:Skjellingahaugen:N?)

model_moss_cover6 <- glmmTMB(adjusted_moss_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover6) # We remove s_year:treatment

model_moss_cover7 <- glmmTMB(adjusted_moss_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + site:warming + site:treatment + warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover7) # We remove site:treatment

model_moss_cover8 <- glmmTMB(adjusted_moss_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + site:warming + warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover8) # We remove s_year:site

model_moss_cover9 <- glmmTMB(adjusted_moss_cover ~ s_year + site + warming + treatment + s_year:warming + site:warming + warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover9) # We stop. Maybe could continue with site:warming
# There was one point where we could have stopped instead of removed more interactions. We compare with the forwards selected model


#### Forwards selection----

# We start with the null model and add terms gradually

model_moss_cover_f1 <- glmmTMB(adjusted_moss_cover ~ (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
forwards_selection(model_moss_cover_f1, model_factors) # We add s_year

model_moss_cover_f2 <- glmmTMB(adjusted_moss_cover ~ s_year + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
forwards_selection(model_moss_cover_f2, model_factors) # We add site

model_moss_cover_f3 <- glmmTMB(adjusted_moss_cover ~ s_year + site + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_moscov)
forwards_selection(model_moss_cover_f3, model_factors) # We stop
# It seems warming and treatment are not relevant. It seems the interactions s_year:warming, site:warming and warming:treatment might be relevant, but not the factors by themselves

plot_moss_cover |> gg_boxplot(year, total_moss_cover, site, treatment) + 
  ylim(-2, 87) +
  labs(x = "Site", y = "Moss cover")


## Model Moss depth----

plot_moss_depth <- plotlv_wide |> select(1:7, 17) |> na.omit()

plot_moss_depth |> gg_boxplot(year, moss_depth_mean, site, warming) + 
  ylim(-2, 51) +
  labs(x = "Year", y = "Moss depth")

# Year is in a very large scale, I use scale() to reduce it. And I use beta regression, so I adjust the percentages to the (0, 1) range. Since it's an open range, I cannot include 0, so I increase the 0 values by a small amount. I also give it a shorter name
plot_mosdep <- plot_moss_depth |> 
  mutate(s_year = scale(year))


#### Backwards selection----

# We start with the full model and remove terms gradually

model_moss_depth1 <- lmer(moss_depth_mean ~ s_year * site * warming * treatment + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth1) # We remove the four-way interaction

model_moss_depth2 <- lmer(moss_depth_mean ~ (s_year + site + warming + treatment)^3 + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth2) # We remove s_year:site:warming

model_moss_depth3 <- lmer(moss_depth_mean ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + s_year:warming:treatment + site:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth3) # We remove s_year:warming:treatment

model_moss_depth4 <- lmer(moss_depth_mean ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + site:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth4) # We remove s_year:warming

model_moss_depth5 <- lmer(moss_depth_mean ~ s_year + site + warming + treatment + s_year:site + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + site:warming:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth5) # We remove site:warming:treatment (Lavisdalen:W:N?)

model_moss_depth6 <- lmer(moss_depth_mean ~ s_year + site + warming + treatment + s_year:site + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth6) # We remove warming:treatment

model_moss_depth7 <- lmer(moss_depth_mean ~ s_year + site + warming + treatment + s_year:site + s_year:treatment + site:warming + site:treatment + s_year:site:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth7) # We remove site:warming

model_moss_depth8 <- lmer(moss_depth_mean ~ s_year + site + warming + treatment + s_year:site + s_year:treatment + site:treatment + s_year:site:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth8) # We remove warming

model_moss_depth9 <- lmer(moss_depth_mean ~ s_year + site + treatment + s_year:site + s_year:treatment + site:treatment + s_year:site:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth9) # We remove s_year:site:treatment (s_year:Lavisdalen:N)

model_moss_depth10 <- lmer(moss_depth_mean ~ s_year + site + treatment + s_year:site + s_year:treatment + site:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth10) # We remove s_year:treatment

model_moss_depth11 <- lmer(moss_depth_mean ~ s_year + site + treatment + s_year:site + site:treatment + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth11) # We remove site:treatment

model_moss_depth12 <- lmer(moss_depth_mean ~ s_year + site + treatment + s_year:site + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth12) # We remove treatment

model_moss_depth13 <- lmer(moss_depth_mean ~ s_year + site + s_year:site + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
backwards_selection(model_moss_depth13) # We stop
# We compare with the forwards selected model


#### Forwards selection----

# We start with the null model and add terms gradually

model_moss_depth_f1 <- lmer(moss_depth_mean ~ (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
forwards_selection(model_moss_depth_f1, model_factors) # We add site

model_moss_depth_f2 <- lmer(moss_depth_mean ~ site + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
forwards_selection(model_moss_depth_f2, model_factors) # We add s_year

model_moss_depth_f3 <- lmer(moss_depth_mean ~ s_year + site + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
forwards_selection(model_moss_depth_f3, model_factors) # We add s_year:site

model_moss_depth_f4 <- lmer(moss_depth_mean ~ s_year + site + s_year:site + (1 | site:block) + (1 | site:block:plot), data = plot_mosdep)
forwards_selection(model_moss_depth_f4, model_factors) # We stop
# It seems warming and treatment are not relevant. It seems the interaction s_year:site:treatment might be relevant, but treatment by itself not

plot_moss_depth |> gg_boxplot(year, moss_depth_mean, treatment, site) + 
  ylim(-2, 51) +
  labs(x = "Site", y = "Moss cover")


## Model Litter cover----

plot_litter_cover <- plotlv_wide |> select(1:7, 11) |> na.omit()

plot_litter_cover |> gg_boxplot(year, total_litter_cover, site, warming) + 
  ylim(-1, 86) +
  labs(x = "Year", y = "Litter cover")

# Year is in a very large scale, I use scale() to reduce it. And I use beta regression, so I adjust the percentages to the (0, 1) range. Since it's an open range, I cannot include 0, so I increase the 0 values by a small amount. I also give it a shorter name
plot_litcov <- plot_litter_cover |> 
  mutate(adjusted_litter_cover = pmin(pmax(plot_litter_cover$total_litter_cover/100, 0.001), 1 - 0.001)) |> 
  mutate(s_year = scale(year))


#### Backwards selection----

# We start with the full model and remove terms gradually

model_litter_cover1 <- glmmTMB(adjusted_litter_cover ~ s_year * site * warming * treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover1) # We remove the four-way interaction

model_litter_cover2 <- glmmTMB(adjusted_litter_cover ~ (s_year + site + warming + treatment)^3 + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover2) # We remove site:warming:treatment

model_litter_cover3 <- glmmTMB(adjusted_litter_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:warming + s_year:site:treatment + s_year:warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover3) # We remove s_year:site:warming (might be relevant. We remove it and see where we get(s_year:Gudmedalen:W??)

model_litter_cover4 <- glmmTMB(adjusted_litter_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:warming + site:treatment + warming:treatment + s_year:site:treatment + s_year:warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover4) # We remove site:warming

model_litter_cover5 <- glmmTMB(adjusted_litter_cover ~ s_year + site + warming + treatment + s_year:site + s_year:warming + s_year:treatment + site:treatment + warming:treatment + s_year:site:treatment + s_year:warming:treatment + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover5) # We stop
# The triple interactions seem relevant, at list for some levels


#### Forwards selection----

# We start with the null model and add terms gradually

model_litter_cover_f1 <- glmmTMB(adjusted_litter_cover ~ (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_litcov)
forwards_selection(model_litter_cover_f1, model_factors) # We add site

model_litter_cover_f2 <- glmmTMB(adjusted_litter_cover ~ site + (1 | site:block) + (1 | site:block:plot), family = "beta_family", data = plot_litcov)
forwards_selection(model_litter_cover_f2, model_factors) # We stop
# It seems only sites differ, with some high-order interactions with other factors

plot_litter_cover |> gg_boxplot(year, total_litter_cover, treatment, site) + 
  ylim(-2, 86) +
  labs(x = "Site", y = "Moss cover")


################################################# SITE AS RANDOM----

#### NMDS1 Backwards selection----

# We start with the full model and remove terms gradually

model_plot_ordination1_r1 <- lmer(NMDS1 ~ s_year * warming * treatment + (1 | site/block/plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination1_r1) # We remove the three-way interaction (s_year:Skjellingahaugen:W:N??, s_year:Skjellingahaugen:W:E?)

model_plot_ordination1_r2 <- lmer(NMDS1 ~ (s_year + warming + treatment)^2 + (1 | site/block/plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination1_r2) # We remove s_year:treatment

model_plot_ordination1_r3 <- lmer(NMDS1 ~ s_year + warming + treatment + s_year:warming + warming:treatment + (1 | site/block/plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination1_r3) # We stop


#### NMDS1 Forwards selection----

# We start with the null model and add terms gradually

model_plot_ordination1_rf1 <- lmer(NMDS1 ~ (1 | site/block/plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination1_rf1, model_factors_r) # We add s_year

model_plot_ordination1_rf2 <- lmer(NMDS1 ~ s_year + (1 | site/block/plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination1_rf2, model_factors_r) # We stop

nmds_plot_sites |> gg_boxplot(year, NMDS1, treatment, warming) + 
  ylim(-0.8, 0.6) +
  labs(x = "Year", y = "NMDS1")


#### NMDS2 Backwards selection----

# We start with the full model and remove terms gradually

model_plot_ordination2_r1 <- lmer(NMDS2 ~ s_year * warming * treatment + (1 | site/block/plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_r1) # We may remove the three-way interaction (s_year:W:N??)

model_plot_ordination2_r2 <- lmer(NMDS2 ~ (s_year + warming + treatment)^2 + (1 | site/block/plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_r2) # We remove s_year:treatment

model_plot_ordination2_r3 <- lmer(NMDS2 ~ s_year + warming + treatment + s_year:warming + warming:treatment + (1 | site/block/plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_r3) # We remove warming:treatment

model_plot_ordination2_r4 <- lmer(NMDS2 ~ s_year + warming + treatment + s_year:warming + (1 | site/block/plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_r4) # We remove s_year:warming

model_plot_ordination2_r5 <- lmer(NMDS2 ~ s_year + warming + treatment + (1 | site/block/plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_r5) # We remove warming

model_plot_ordination2_r6 <- lmer(NMDS2 ~ s_year + treatment + (1 | site/block/plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_r6) # We may remove treatment (E?)

model_plot_ordination2_r7 <- lmer(NMDS2 ~ s_year + (1 | site/block/plot), data = nmds_plot_sites)
backwards_selection(model_plot_ordination2_r7) # We stop


#### NMDS2 Forwards selection----

# We start with the null model and add terms gradually

model_plot_ordination2_rf1 <- lmer(NMDS2 ~ (1 | site/block/plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination2_rf1, model_factors_r) # We add s_year

model_plot_ordination2_rf2 <- lmer(NMDS2 ~ s_year + (1 | site/block/plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination2_rf2, model_factors_r) # We maybe add treatment

model_plot_ordination2_rf3 <- lmer(NMDS2 ~ s_year + treatment + (1 | site/block/plot), data = nmds_plot_sites)
forwards_selection(model_plot_ordination2_rf3, model_factors_r) # We stop

nmds_plot_sites |> gg_boxplot(year, NMDS2, warming, treatment) + 
  ylim(-0.5, 0.5) +
  labs(x = "Year", y = "NMDS2")


## Model Vegetation cover.  Backwards selection----

# We start with the full model and remove terms gradually

model_vegetation_cover_r1 <- glmmTMB(adjusted_vegetation_cover ~ s_year * warming * treatment + (1 | site/block/plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover_r1) # We remove the three-way interaction

model_vegetation_cover_r2 <- glmmTMB(adjusted_vegetation_cover ~ (s_year + warming + treatment)^2 + (1 | site/block/plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover_r2) # We remove s_year:warming

model_vegetation_cover_r3 <- glmmTMB(adjusted_vegetation_cover ~ s_year + warming + treatment + s_year:treatment + warming:treatment + (1 | site/block/plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover_r3) # We remove s_year:treatment

model_vegetation_cover_r4 <- glmmTMB(adjusted_vegetation_cover ~ s_year + warming + treatment + warming:treatment + (1 | site/block/plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover_r4) # We may remove warming:treatment (W:E?)

model_vegetation_cover_r5 <- glmmTMB(adjusted_vegetation_cover ~ s_year + warming + treatment + (1 | site/block/plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover_r5) # We remove warming

model_vegetation_cover_r6 <- glmmTMB(adjusted_vegetation_cover ~ s_year + treatment + (1 | site/block/plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover_r6) # We remove treatment

model_vegetation_cover_r7 <- glmmTMB(adjusted_vegetation_cover ~ s_year + (1 | site/block/plot), family = "beta_family", data = plot_vegcov)
backwards_selection(model_vegetation_cover_r7) # We stop
# We compare it with the forwards selected model


#### Forwards selection----

# We start with the null model and add terms gradually

model_vegetation_cover_rf1 <- glmmTMB(adjusted_vegetation_cover ~ (1 | site/block/plot), family = "beta_family", data = plot_vegcov)
forwards_selection(model_vegetation_cover_rf1, model_factors) # We add s_year

model_vegetation_cover_rf2 <- glmmTMB(adjusted_vegetation_cover ~ s_year + (1 | site/block/plot), family = "beta_family", data = plot_vegcov)
forwards_selection(model_vegetation_cover_rf2, model_factors) # We stop

# It seems warming and treatment are not relevant. It seems the interactions and warming:treatment might be relevant, but not the factors by themselves

plot_vegetation_cover |> gg_boxplot(year, vegetation_cover, treatment, warming) + 
  ylim(24, 101) +
  labs(x = "Year", y = "Vegetation cover")


## Model Vegetation height. Backwards selection----

# We start with the full model and remove terms gradually

model_vegetation_height_r1 <- lmer(vegetation_height_mean ~ s_year * warming * treatment + (1 | site/block/plot), data = plot_veghei)
backwards_selection(model_vegetation_height_r1) # We remove the three-way interaction

model_vegetation_height_r2 <- lmer(vegetation_height_mean ~ (s_year + warming + treatment)^2 + (1 | site/block/plot), data = plot_veghei)
backwards_selection(model_vegetation_height_r2) # We remove s_year:treatment

model_vegetation_height_r3 <- lmer(vegetation_height_mean ~ s_year + warming + treatment + s_year:warming + warming:treatment + (1 | site/block/plot), data = plot_veghei)
backwards_selection(model_vegetation_height_r3) # We stop
# We now see the forwards model


#### Forwards selection----

# We start with the null model and add terms gradually

model_vegetation_height_rf1 <- lmer(vegetation_height_mean ~ (1 | site/block/plot), data = plot_veghei)
forwards_selection(model_vegetation_height_rf1, model_factors_r) # We add s_year

model_vegetation_height_rf2 <- lmer(vegetation_height_mean ~ s_year +(1 | site/block/plot), data = plot_veghei)
forwards_selection(model_vegetation_height_rf2, model_factors_r) # We stop
# It seems warming and treatment are not relevant. It seems the interactions s_year:warming and warming:treatment might be relevant, but not the factors by themselves

plot_vegetation_height |> gg_boxplot(year, vegetation_height_mean, treatment, warming) + 
  ylim(9, 225) +
  labs(x = "Year", y = "Vegetation height")


## Model Moss cover. Backwards selection----

# We start with the full model and remove terms gradually

model_moss_cover_r1 <- glmmTMB(adjusted_moss_cover ~ s_year * warming * treatment + (1 | site/block/plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover_r1) # We remove the three-way interaction

model_moss_cover_r2 <- glmmTMB(adjusted_moss_cover ~ (s_year + warming + treatment)^2 + (1 | site/block/plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover_r2) # We remove the three-way interaction

model_moss_cover_r3 <- glmmTMB(adjusted_moss_cover ~ s_year + warming + treatment + s_year:warming + warming:treatment + (1 | site/block/plot), family = "beta_family", data = plot_moscov)
backwards_selection(model_moss_cover_r3) # We stop
# We compare with the forwards selected model


#### Forwards selection----

# We start with the null model and add terms gradually

model_moss_cover_rf1 <- glmmTMB(adjusted_moss_cover ~ (1 | site/block/plot), family = "beta_family", data = plot_moscov)
forwards_selection(model_moss_cover_rf1, model_factors_r) # We add s_year

model_moss_cover_rf2 <- glmmTMB(adjusted_moss_cover ~ s_year + (1 | site/block/plot), family = "beta_family", data = plot_moscov)
forwards_selection(model_moss_cover_rf2, model_factors_r) # We stop
# It seems warming and treatment are not relevant. It seems the interactions s_year:warming and warming:treatment might be relevant, but not the factors by themselves

plot_moss_cover |> gg_boxplot(year, total_moss_cover, warming, treatment) + 
  ylim(-2, 87) +
  labs(x = "Site", y = "Moss cover")


## Model Moss depth. Backwards selection----

# We start with the full model and remove terms gradually

model_moss_depth_r1 <- lmer(moss_depth_mean ~ s_year * warming * treatment + (1 | site/block/plot), data = plot_mosdep)
backwards_selection(model_moss_depth_r1) # We remove the three-way interaction

model_moss_depth_r2 <- lmer(moss_depth_mean ~ (s_year + warming + treatment)^2 + (1 | site/block/plot), data = plot_mosdep)
backwards_selection(model_moss_depth_r2) # We remove s_year:warming

model_moss_depth_r3 <- lmer(moss_depth_mean ~ s_year + warming + treatment + s_year:treatment + warming:treatment + (1 | site/block/plot), data = plot_mosdep)
backwards_selection(model_moss_depth_r3) # We remove warming:treatment

model_moss_depth_r4 <- lmer(moss_depth_mean ~ s_year + warming + treatment + s_year:treatment + (1 | site/block/plot), data = plot_mosdep)
backwards_selection(model_moss_depth_r4) # We remove warming

model_moss_depth_r5 <- lmer(moss_depth_mean ~ s_year + treatment + s_year:treatment + (1 | site/block/plot), data = plot_mosdep)
backwards_selection(model_moss_depth_r5) # We remove s_year:treatment

model_moss_depth_r6 <- lmer(moss_depth_mean ~ s_year + treatment + (1 | site/block/plot), data = plot_mosdep)
backwards_selection(model_moss_depth_r6) # We remove treatment

model_moss_depth_r7 <- lmer(moss_depth_mean ~ s_year + (1 | site/block/plot), data = plot_mosdep)
backwards_selection(model_moss_depth_r7) # We stop
# We compare with the forwards selected model


#### Forwards selection----

# We start with the null model and add terms gradually

model_moss_depth_rf1 <- lmer(moss_depth_mean ~ (1 | site/block/plot), data = plot_mosdep)
forwards_selection(model_moss_depth_rf1, model_factors_r) # We add s_year

model_moss_depth_rf2 <- lmer(moss_depth_mean ~ s_year + (1 | site/block/plot), data = plot_mosdep)
forwards_selection(model_moss_depth_rf2, model_factors_r) # We stop

plot_moss_depth |> gg_boxplot(year, moss_depth_mean, treatment, warming) + 
  ylim(-2, 51) +
  labs(x = "Site", y = "Moss cover")


## Model Litter cover. Backwards selection----

# We start with the full model and remove terms gradually

model_litter_cover_r1 <- glmmTMB(adjusted_litter_cover ~ s_year * warming * treatment + (1 | site/block/plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover_r1) # We maybe remove the three-way interaction (s_year:W:N??)

model_litter_cover_r2 <- glmmTMB(adjusted_litter_cover ~ (s_year + warming + treatment)^2 + (1 | site/block/plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover_r2) # We remove s_year:treatment

model_litter_cover_r3 <- glmmTMB(adjusted_litter_cover ~ s_year + warming + treatment + s_year:warming + warming:treatment + (1 | site/block/plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover_r3) # We remove warming:treatment

model_litter_cover_r4 <- glmmTMB(adjusted_litter_cover ~ s_year + warming + treatment + s_year:warming + (1 | site/block/plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover_r4) # We remove treatment

model_litter_cover_r5 <- glmmTMB(adjusted_litter_cover ~ s_year + warming + s_year:warming + (1 | site/block/plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover_r5) # We maybe remove s_year:Warming (s_year:W?)

model_litter_cover_r6 <- glmmTMB(adjusted_litter_cover ~ s_year + warming + (1 | site/block/plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover_r6) # We remove s_year

model_litter_cover_r7 <- glmmTMB(adjusted_litter_cover ~ warming + (1 | site/block/plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover_r7) # We remove warming

model_litter_cover_r8 <- glmmTMB(adjusted_litter_cover ~ (1 | site/block/plot), family = "beta_family", data = plot_litcov)
backwards_selection(model_litter_cover_r8) # We stop
# s_year may interact with warming. We look at the forwards model


#### Forwards selection----

# We start with the null model and add terms gradually

model_litter_cover_rf1 <- glmmTMB(adjusted_litter_cover ~ (1 | site/block/plot), family = "beta_family", data = plot_litcov)
forwards_selection(model_litter_cover_rf1, model_factors_r) # We stop
# Nothing

plot_litter_cover |> gg_boxplot(year, total_litter_cover, warming, treatment) + 
  ylim(-2, 86) +
  labs(x = "Site", y = "Moss cover")

