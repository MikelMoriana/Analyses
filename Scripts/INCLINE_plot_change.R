# Libraries and files----

library(tidyverse)
library(purrr)
library(broom)

# community_: subplot level. abundance_: plot level. cover_: plot level

community_clean <- read.csv("Data/INCLINE_community_subplot.csv") |> 
  mutate(block = as.factor(block)) |> 
  mutate(plot = as.factor(plot)) |> 
  mutate(subPlot = as.factor(subPlot))

cover_clean <- read.csv("Data/INCLINE_community_species_cover.csv") |> 
  select(-c(11:13)) |> 
  mutate(block = as.factor(block)) |> 
  mutate(plot = as.factor(plot))

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

cover_use <- cover_clean |> 
  filter(year != 2022) |> 
  filter(!(site == "Skjellingahaugen" & block == 5))  |> 
  filter(!(treatment == "E" & species %in% c("Car_pil", "Ver_off", "Vio_can"))) |>  
  filter(!(treatment == "N" & species %in% c("Car_pal", "Hyp_mac", "Suc_pra"))) |> 
  filter(!(site == "Gudmedalen" & species %in% c("Tri_pra", "Vio_tri"))) |> 
  filter(!(site == "Lavisdalen" & species %in% c("Ach_mil", "Lys_eur", "Pot_ere", "Pru_vul", "Tri_pra", "Tri_rep"))) |> 
  filter(!(site == "Skjellingahaugen" & species %in% c("Ach_mil", "Ane_nem", "Pru_vul", "Sil_vul", "Ste_gra"))) |> 
  filter(!(site == "Ulvehaugen" & species %in% c("Gal_bor", "Lot_cor", "Pot_ere", "Pru_vul", "Ran_aur", "Tri_pra", "Tri_rep")))


# Functions----

function_presence <- function(data, years) {
  data |> 
    filter(year %in% years) |>  
    select(plotID, species) |> 
    distinct() |> 
    group_by(species) |> 
    summarise(plots = n())
}

function_presence_all <- function(data, last_years) {
  year_pairs <- combn(unique(data$year), 2, simplify = FALSE)
  year_pairs <- year_pairs[c(1,3, 4, 6)]
  results_list <- map(year_pairs, ~ function_presence(data, .x))
  for(i in seq_along(results_list)) {
    results_list[[i]] <- results_list[[i]] |> 
      mutate(years = paste(year_pairs[[i]], collapse = "-"))
  }
  all_years_result <- function_presence(data, unique(data$year)) |> 
    mutate(years = "all_plots")
  results_list <- c(list(all_years_result), results_list)
  results <- bind_rows(results_list)
  results <- results |> 
    pivot_wider(names_from = years, values_from = plots, values_fill = 0) |> 
    relocate({{last_years}}, .after = last_col())
  return(results)
}

function_value <- function(data, col_range) {
  data |> 
    select(year, plotID, all_of(col_range)) |> 
    pivot_longer(cols = -c(year, plotID), names_to = "species") |> 
    pivot_wider(names_from = year)
}

function_change <- function(data, col_range) {
  all_values <- function_value(data, col_range)
  year_pairs <- combn(unique(data$year), 2, simplify = FALSE)
  year_pairs <- year_pairs[c(1,3, 4, 6)]
  results <- map(year_pairs, ~ {
    year1 = .[1]
    year2 = .[2]
    dat_year1 <- all_values |> 
      select(species, plotID, all_of(as.character(year1)))
    dat_year2 <- all_values |> 
      select(species, plotID, all_of(as.character(year2)))
    diff_data <- dat_year2
    diff_data[[as.character(year2)]] <- dat_year2[[as.character(year2)]] - dat_year1[[as.character(year1)]]
    diff_data <- diff_data |> rename(diff = all_of(as.character(year2)))
    counts <- diff_data |> 
      rowwise() |> 
      mutate(increased = sum(c_across(diff) > 0, na.rm = TRUE), 
             decreased = sum(c_across(diff) < 0, na.rm = TRUE)) |> 
      group_by(species) |> 
      summarise(increased = sum(increased), decreased = sum(decreased), .groups = "drop")
    col_names <- paste(c("increased", "decreased"), paste(year1, year2, sep = "_"), sep = "_")
    names(counts)[2:3] <- col_names
    return(counts)
  })
  results <- reduce(results, full_join, by = "species")
  return(results)
}

function_wilcoxon <- function(data, value) {
  year_pairs <- combn(unique(data$year), 2, simplify = FALSE)
  year_pairs <- year_pairs[c(1,3, 4, 6)]
  results <- map(year_pairs, ~ {
    years <- .x
    sub_data <- data |> 
      filter(year %in% years) |> 
      group_by(species) %>% 
      do(tidy(wilcox.test(pull(., value) ~ year, data = ., paired = TRUE))) |> 
      ungroup() |>  
      mutate(p.value = round(p.value, 3))
    col_names <- paste(c("statistic", "p.value"), paste(years, collapse = "_"), sep = "_")
    names(sub_data)[2:3] <- col_names
    sub_data <- sub_data |> select(species, all_of(col_names))
    return(sub_data)
  })
  results <- reduce(results, full_join, by = "species")
  return(results)
}

function_persistence <- function(dataframe) {
  dataframe |> 
    group_by(species) |> 
    nest() %>%
    mutate(data = map(data, ~ {
      dat <- .
      year_pairs = combn(unique(dat$year), 2, simplify = FALSE)
      year_pairs = year_pairs[c(1,3, 4, 6)]
      map_dfr(year_pairs, ~ {
        year1 = .[1]
        year2 = .[2]
        dat_year1 <- dat %>%
          filter(year == year1) %>%
          mutate(presence = if_else(abundance > 0, 1, 0))
        dat_year2 <- dat %>%
          filter(year == year2) %>%
          mutate(presence = if_else(abundance > 0, 1, 0))
        merged_dat <- full_join(dat_year1, dat_year2, by = "plotID", suffix = c("_year1", "_year2"))
        a = sum(merged_dat$presence_year1 == 1 & merged_dat$presence_year2 == 1, na.rm = TRUE)
        b = sum(merged_dat$presence_year1 == 1 & merged_dat$presence_year2 == 0, na.rm = TRUE)
        c = sum(merged_dat$presence_year1 == 0 & merged_dat$presence_year2 == 1, na.rm = TRUE)
        d = sum(merged_dat$presence_year1 == 0 & merged_dat$presence_year2 == 0, na.rm = TRUE)
        v = (a*d - b*c) / sqrt((a+b)*(a+c)*(b+d)*(c+d))
        tibble(years = paste("V", year1, year2, sep = "_"), V = v)
      })
    })) |> 
    unnest(data) |> 
    mutate(V = round(V, 2)) |> 
    pivot_wider(names_from = years, values_from = V)
}

# Changes in abundance through the years----

abundance <- community_use |> 
  group_by(year, site, plotID, block, plot, warming, treatment, species) |> 
  summarise(abundance = sum(presence)) |> 
  ungroup() |> 
  arrange(species)

# We calculate the number of different plots each species was present in for each time interval
subplot_presence <- community_use |> select(plotID, subPlot, species) |> distinct() |> group_by(species) |> summarise(all_subplots = n())
plot_presence <- abundance |> function_presence_all("2018-2023")

# We calculate the abundance of each species at each plot for each year
abundance_wide <- abundance |> 
  pivot_wider(names_from = species, values_from = abundance, values_fill = 0) |> 
  arrange(year, plotID)
abundance_change <- abundance_wide |> function_change(8:115)

# We calculate the p value for the change in abundance for each species at each time interval (we need all combinations of year, plotID and species)
abundance_full <- abundance_wide |> 
  pivot_longer(cols = 8:115, names_to = "species", values_to = "abundance")
abundance_wilcoxon <- abundance_full |> function_wilcoxon("abundance")

# We calculate persistence and create the final dataframe
persistence <- abundance_full |> function_persistence()

abundance_trend <- plot_presence |> 
  left_join(subplot_presence, by = "species") |> 
  relocate(all_subplots, .after = all_plots) |> 
  left_join(abundance_change, by = "species") |> 
  relocate(increased_2018_2019:decreased_2018_2019, .after = "2018-2019") |> 
  relocate(increased_2019_2021:decreased_2019_2021, .after = "2019-2021") |> 
  relocate(increased_2021_2023:decreased_2021_2023, .after = "2021-2023") |> 
  relocate(increased_2018_2023:decreased_2018_2023, .after = "2018-2023") |> 
  left_join(abundance_wilcoxon, by = "species") |> 
  relocate(statistic_2018_2019:p.value_2018_2019, .after = decreased_2018_2019) |> 
  relocate(statistic_2019_2021:p.value_2019_2021, .after = decreased_2019_2021) |> 
  relocate(statistic_2021_2023:p.value_2021_2023, .after = decreased_2021_2023) |> 
  relocate(statistic_2018_2023:p.value_2018_2023, .after = decreased_2018_2023) |> 
  left_join(persistence, by = "species") |> 
  relocate(V_2018_2019, .after = p.value_2018_2019) |> 
  relocate(V_2019_2021, .after = p.value_2019_2021) |> 
  relocate(V_2021_2023, .after = p.value_2021_2023) |> 
  relocate(V_2018_2023, .after = p.value_2018_2023)

write.csv(abundance_trend, "Data/Abundance_trend.csv", row.names = FALSE)


# Changes in cover through the years TO DOOOOOOOOOOOOO----

# We already have the number of different plots each species was present in for each time interval

# We calculate the cover of each species at each plot for each year

cover_wide <- cover_use |> 
  select(-functional_group) |> 
  arrange(species) |> 
  pivot_wider(names_from = species, values_from = cover, values_fill = 0) |> 
  arrange(year, plotID)

cover_18 <- cover_wide |> function_value_year(2018)
cover_19 <- cover_wide |> function_value_year(2019)
cover_21 <- cover_wide |> function_value_year(2021)
cover_23 <- cover_wide |> function_value_year(2023)


# We calculate the change of each species at each plot for each time interval

cover_change_18_19 <- function_value_change(cover_18, cover_19)
cover_change_19_21 <- function_value_change(cover_19, cover_21)
cover_change_21_23 <- function_value_change(cover_21, cover_23)
cover_change_18_23 <- function_value_change(cover_18, cover_23)


# We calculate the p value for the change in abundance for each species at each time interval

# We first need a dataframe with all combination of year, plotID and species (abundance = 0 when the species was not present)
cover_full <- cover_wide |> 
  pivot_longer(cols = 8:115, names_to = "species", values_to = "cover")

cover_wilcoxon_18_19 <- cover_full |> function_wilcoxon("cover", c(2018, 2019))
cover_wilcoxon_19_21 <- cover_full |> function_wilcoxon("cover", c(2019, 2021))
cover_wilcoxon_21_23 <- cover_full |> function_wilcoxon("cover", c(2021, 2023))
cover_wilcoxon_18_23 <- cover_full |> function_wilcoxon("cover", c(2018, 2023))


# We combine all values into one dataframe

cover_change <- plot_presence |> 
  rename(all_plots = plots) |> 
  left_join(subplot_presence, by = "species") |> 
  rename(all_subplots = subplots) |> 
  left_join(plot_presence_18_19, by = "species") |> 
  left_join(cover_change_18_19[, 1:3], by = "species") |> 
  rename(plots_18_19 = plots, increased_18_19 = increased, decreased_18_19 = decreased) |> 
  left_join(plot_presence_19_21, by = "species") |> 
  left_join(cover_change_19_21[, 1:3], by = "species") |> 
  rename(plots_19_21 = plots, increased_19_21 = increased, decreased_19_21 = decreased) |> 
  left_join(plot_presence_21_23, by = "species") |> 
  left_join(cover_change_21_23[, 1:3], by = "species") |> 
  rename(plots_21_23 = plots, increased_21_23 = increased, decreased_21_23 = decreased) |> 
  left_join(plot_presence_18_23, by = "species") |> 
  left_join(cover_change_18_23[, 1:3], by = "species") |> 
  rename(plots_18_23 = plots, increased_18_23 = increased, decreased_18_23 = decreased) |> 
  mutate(p_value_18_19 = cover_wilcoxon_18_19$p.value) |> 
  relocate(p_value_18_19, .after = decreased_18_19) |> 
  mutate(p_value_19_21 = cover_wilcoxon_19_21$p.value) |> 
  relocate(p_value_19_21, .after = decreased_19_21) |> 
  mutate(p_value_21_23 = cover_wilcoxon_21_23$p.value) |> 
  relocate(p_value_21_23, .after = decreased_21_23) |> 
  mutate(p_value_18_23 = cover_wilcoxon_18_23$p.value) |> 
  relocate(p_value_18_23, .after = decreased_18_23)

write.csv(cover_change, "Data/Cover_change.csv", row.names = FALSE)


# Changes in abundance through the years by treatment----

abundance_c_c <- abundance |> 
  filter(warming == "C" & treatment == "C")
abundance_c_e <- abundance |> 
  filter(warming == "C" & treatment == "E")
abundance_c_n <- abundance |> 
  filter(warming == "C" & treatment == "N")
abundance_w_c <- abundance |> 
  filter(warming == "W" & treatment == "C")
abundance_w_e <- abundance |> 
  filter(warming == "W" & treatment == "E")
abundance_w_n <- abundance |> 
  filter(warming == "W" & treatment == "N")


# c c

abundance_c_c |> group_by(species) |> filter(n(species)>5)

# We calculate the number of different plots each species was present in for each time interval
