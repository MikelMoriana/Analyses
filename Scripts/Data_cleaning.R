## Libraries and obtaining the files----

library(tidyverse)
library(lubridate)
library(turfmapper)
library(pipebind)

#Use your OSF token to get excess to osf. From here you can download neccesary files
#osf_auth(token = "")#Your personal OSF token

# Community data
dataDownloader::get_file(node = "zhk3m",
                         file = "INCLINE_community_2018_2019_2021_2022_2023.csv",
                         path = "raw_data",
                         remote_path = "RawData/Community")

# Meta data
dataDownloader::get_file(node = "zhk3m",
                         file = "INCLINE_metadata.csv",
                         path = "raw_data",
                         remote_path = "RawData")

# Name dictionary
dataDownloader::get_file(node = "zhk3m",
                         file = "INCLINE_community_name_dictionary_20231204.csv",
                         path = "raw_data",
                         remote_path = "RawData/Community")

# Species name dictionary
dataDownloader::get_file(node = "zhk3m",
                         file = "INCLINE_species_taxonomic_name.csv",
                         path = "raw_data",
                         remote_path = "RawData/Community")


## Reading the files----

# 1. We load the files into the environment

community_download <- read_delim("raw_data/INCLINE_community_2018_2019_2021_2022_2023.csv", col_types = cols(.default = col_character()))
# 2021 Lavisdalen, Block 3, plot 6: this plot is empty. We remove it and add it back from another csv file
# I could not upload the file to OSF without it getting messed up, so I keep it in my PC
lav_3_6_2021_community <- read_delim("raw_data/INCLINE_community_2021_Lavisdalen_3_6.csv", col_types = cols(.default = col_character()))
community_without_lav_3_6_2021 <- community_download |> 
  filter(!(Site == "Lavisdalen" & Block == 3 & plot == 6 & year == 2021))
community_with_lav_3_6_2021 <- bind_rows(community_without_lav_3_6_2021, lav_3_6_2021_community)

metadata_download <- read_delim("raw_data/INCLINE_metadata.csv") # Need the meta data to fill in the missing part of the treatment and OTC column for 2018
name_dictionary <- read_delim("raw_data/INCLINE_community_name_dictionary_20231204.csv")
species_dictionary <- read_delim("raw_data/INCLINE_species_taxonomic_name.csv")


# 2. We create functions that we will use repeatedly to check for duplicates

check_duplicates_wide <- function(x) {
  wide_unique <- x |> select(year, plotID, subPlot, Ach_mil:moss)
  wide_duplicated <- wide_unique[duplicated(wide_unique), ]
  view(wide_duplicated)
}

check_duplicates_long <- function(x) {
  long_unique <- x |> select(year, plotID, subPlot, species)
  long_duplicated <- long_unique[duplicated(long_unique), ]
  view(long_duplicated)
}


## Cleaning variables----

# 2. We check for missing information in any of the main columns (site, block, plot, subplot, year). We check the scans for corrections

community_with_lav_3_6_2021 |> filter(is.na(Site)) |> print(n = Inf) # Two sites, both are from Ulvehaugen
community_with_lav_3_6_2021 |> filter(is.na(Block)) # Nothing missing
community_with_lav_3_6_2021 |> filter(is.na(plot)) # Nothing missing
community_with_lav_3_6_2021 |> filter(is.na(subPlot)) # Nothing missing
community_with_lav_3_6_2021 |> filter(is.na(year)) # Nothing missing


# 3. We give the columns proper names (we combine soil and bare into bare_ground, since sometimes it was registered one way, sometimes the other), correct the missing site, create a plotID and establish the data type. We then check for duplicates

community_data <- community_with_lav_3_6_2021 |> 
  rename(site = Site, block = Block, measure = Measure, weather = Weather, Cer_Sag_cf = "Cer/sag_cf", Cer_sp = "Cer _sp", Nid_seedling = "Nid seedling", vegetation_cover = Veg_cover, vegetation_height_mm = Veg_height_mm, moss_depth_mm = Moss_depth_mm)|>
  mutate(site = replace_na(site, "Ulvehaugen")) |> 
  mutate(plotID = paste0(str_sub(site, 1,3), "_", block, "_", plot)) |> 
  select(-Treatment) |> 
  mutate(block = as.factor(block)) |> 
  mutate(plot = as.factor(plot)) |> 
  mutate(year = as.numeric(year, na.rm = TRUE)) |> 
  mutate(moss = as.numeric(moss, na.rm = TRUE)) |> 
  mutate(lichen = as.numeric(lichen, na.rm = TRUE)) |> 
  mutate(litter = as.numeric(litter, na.rm = TRUE)) |> 
  mutate(bare_ground = coalesce(soil, bare), 
         bare_ground = as.numeric(bare_ground, na.rm = TRUE)) |> 
  select(!c(soil, bare)) |> 
  mutate(rock = as.numeric(rock, na.rm = TRUE)) |> 
  mutate(poo = as.numeric(poo, na.rm = TRUE)) |> 
  mutate(fungus = ifelse(fungus == "O", NA, fungus), 
         fungus = as.numeric(fungus, na.rm = TRUE))|> 
  mutate(logger = as.numeric(logger, na.rm = TRUE)) |> 
  mutate(vegetation_cover = as.numeric(vegetation_cover, na.rm = TRUE)) |> 
  mutate(vegetation_height_mm = as.numeric(vegetation_height_mm, na.rm = TRUE)) |> 
  mutate(moss_depth_mm = as.numeric(moss_depth_mm, na.rm = TRUE)) |> 
  mutate(date_comment = ifelse(date == "14.08.2019/15.08.2019", "Vegetation analysis was conducted on the 14.08.2019 and the 15.08.2019", NA)) |> 
  mutate(date = ifelse(date == "14.08.2019/15.08.2019", "14.08.2019", date)) |>
  mutate(date_comment = ifelse(date == "01.08.2019/02.08.2019", "Vegetation analysis was conducted on the 01.08.2019 and the 02.08.2019", NA)) |> 
  mutate(date = ifelse(date == "01.08.2019/02.08.2019", "01.08.2019", date)) |>
  mutate(date_comment = ifelse(date == "30.07.2019/31.07.2019", "Vegetation analysis was conducted on the 30.07.2019 and the 31.07.2019", NA)) |> 
  mutate(date = ifelse(date == "30.07.2019/31.07.2019", "30.07.2019", date)) |>
  mutate(date_comment = ifelse(date == "31.07.2019/01.08.2019", "Vegetation analysis was conducted on the 31.07.2019 and the 01.08.2019", NA)) |> 
  mutate(date = ifelse(date == "31.07.2019/01.08.2019", "31.07.2019", date)) |>
  mutate(date_comment = ifelse(date == "30.07.2021/02.08.2021", "Vegetation analysis was conducted on the 30.07.2021 and the 02.08.2021", NA)) |> 
  mutate(date = ifelse(date == "30.07.2021/02.08.2021", "30.07.2021", date)) |>
  mutate(date = dmy(date)) |> 
  relocate(year, site, plotID, block, plot, subPlot, measure) |> 
  relocate(bare_ground, .after = litter) |> 
  arrange(year, plotID)

check_duplicates_wide(community_data) # One plot was typed in twice in 2018, but with some mistakes. We correct them
community_data_without_duplicates <- community_data |> 
  mutate(Bis_viv = ifelse(year == 2018 & plotID == "Lav_4_6" & subPlot == 21, 1, Bis_viv), 
         Sib_pro = ifelse(year == 2018 & plotID == "Lav_4_6" & subPlot %in% c(17, 18), "1S", Sib_pro), 
         Tar_sp = ifelse(year == 2018 & plotID == "Lav_4_6" & subPlot == 35, NA, Tar_sp), 
         Tar_sp = ifelse(year == 2018& plotID == "Lav_4_6" & subPlot == "cover" , 3, Tar_sp), 
         vegetation_height_mm = ifelse(year == 2018 & plotID == "Lav_4_6" & subPlot == 10, 47, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2018 & plotID == "Lav_4_6" & subPlot == 12, 53, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2018 & plotID == "Lav_4_6" & subPlot == 24, 44, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2018 & plotID == "Lav_4_6" & subPlot == 26, 38, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Lav_4_6" & subPlot == 10, 30, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Lav_4_6" & subPlot == 12, 63, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Lav_4_6" & subPlot == 24, 46, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Lav_4_6" & subPlot == 26, 37, moss_depth_mm)) |> 
  distinct()


# 4. We select relevant variables from the metadata and add it to the data. We are not interested in the removal plots, so we remove them

metadata <- metadata_download |> 
  select(plotID, OTC, treatment) # Selecting relevant variables from the meta data

community_metadata <- community_data_without_duplicates |> 
  left_join(metadata, by = "plotID") |> 
  rename(warming = OTC) |> 
  left_join(name_dictionary, by = c("recorder" = "initials")) |> 
  select(-recorder) |> 
  rename(recorder = name) |> 
  left_join(name_dictionary, by = c("writer" = "initials")) |> 
  select(-writer) |> 
  rename(writer = name) |> 
  filter(treatment != "R") |> 
  relocate(c(warming, treatment), .after = plot) |> 
  relocate(c(recorder, writer), .after = date)
# There was a plot Lav_7_6 in the data (which doesn't exist). It doesn't seem to belong to any other plots, the treatement != "R" removes it as well


## Preparing the data for the turfmapper----

# 5. We make the data long

community_long <- community_metadata |> 
  pivot_longer(Ach_mil:Nid_seedling, values_drop_na = TRUE)|> # Check which species is the first and the last and use these instead of Ach_mil and Nid_seedling
  rename(species = name) |> 
  relocate(c(species, value), .after = measure) |> 
  relocate(c(date, recorder, writer, weather), .after = last_col())


# 6. We take a general look at the recorded species names, and correct those we easily can. In some cases typing mistakes occurred, and in others old taxonomic names were used that not everyone has caught up on. We also standardize the names so its easier to work on when cleaning the data

community_long |> select(species) |> arrange(species) |> distinct() |> print(n = Inf) # Some typos, and some species called with different names
# Changed taxonomic name: Coel_vir, Emp_her, Hup_app, Lyc_lyc, Tri_eur
# Standardizing names: Antennaria_sp, Epilobium_sp, Juncus_sp, Ranunculus
# Typing mistakes: Gen_ana, Gen_riv, Hyp_sel, Leu_aut_cf, Val_atr
# Other: Arc_urv (I don't know where this came from, it is Bis_viv), Eup_sp and Eup_str (we will call them Eup_wet), Hyp_sp (this was missing, but it is Hyp_mac), and Pyr_min and Pyr_rot (since these are difficult to differentiate, we group them as Pyr_sp)

# We create a function to check whether there are any plots where we find the same species with two different names (according to the name changes we're going to apply)
find_same_name <- function(a, b, c) {
  result <-  inner_join(
    x = (a |> filter(species == b) |> select(year, plotID) |> distinct()), 
    y = (a |> filter(species == c) |> select(year, plotID) |> distinct())
  )
  if (nrow(result) == 0) {
    return("You're good")
  } else {
    return(result)
  }
}

find_same_name(community_long, "Antennaria_sp", "Ant_sp") # No duplicates
find_same_name(community_long, "Arc_urv", "Bis_viv") # No duplicates
find_same_name(community_long, "Coel_vir", "Dac_vir") # No duplicates
find_same_name(community_long, "Emp_her", "Emp_nig") # No duplicates
find_same_name(community_long, "Epilobium_sp", "Epi_sp") # No duplicates
find_same_name(community_long, "Eup_sp", "Eup_str") # 2019 Gud_7_1. Checking the scan, we found out this is Epi_ana, not Eup_str
find_same_name(community_long, "Eup_sp", "Eup_wet") # No duplicates
find_same_name(community_long, "Eup_str", "Eup_wet") # No duplicates
find_same_name(community_long, "Gen_ana", "Gen_ama") # No duplicates
find_same_name(community_long, "Gen_riv", "Geu_riv") # No duplicates
find_same_name(community_long, "Gen_riv", "Gen_niv") # No duplicates
find_same_name(community_long, "Hup_app", "Hup_sel") # No duplicates
find_same_name(community_long, "Hyp_sel", "Hup_sel") # 2018 Gud_5_1. Checking the scan, I found out that the same species has been typed in twice, once as Hup_sel and once as Hyp_sel. Using distinct() we can remove the duplicates
find_same_name(community_long, "Hyp_sp", "Hyp_mac") # No duplicates
find_same_name(community_long, "Juncus_sp", "Jun_sp") # No duplicates
find_same_name(community_long, "Leu_aut_cf", "Leo_aut_cf") # No duplicates
find_same_name(community_long, "Lyc_lyc", "Sel_sel") # No duplicates
find_same_name(community_long, "Pyr_sp", "Pyr_min") # No duplicates
find_same_name(community_long, "Pyr_sp", "Pyr_rot") # No duplicates
find_same_name(community_long, "Pyr_min", "Pyr_rot") # No duplicates
find_same_name(community_long, "Ranunculus", "Ran_sp") # No duplicates
find_same_name(community_long, "Tri_eur", "Lys_eur") # No duplicates
find_same_name(community_long, "Val_atr", "Vah_atr") # No duplicates
find_same_name(community_long, "Vio_riv", "Vio_can") # No duplicates


community_long_species <- community_long |> 
  mutate(species = ifelse(species == "Antennaria_sp", "Ant_sp", species)) |> 
  mutate(species = ifelse(species == "Arc_urv", "Bis_viv", species)) |> 
  mutate(species = ifelse(species == "Coel_vir", "Dac_vir", species)) |> 
  mutate(species = ifelse(species == "Emp_her", "Emp_nig", species)) |> 
  mutate(species = ifelse(species == "Epilobium_sp", "Epi_sp", species)) |> 
  mutate(species = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot %in% c(16, "cover") & species == "Eup_str", "Epi_ana", species), 
         species = ifelse(species %in% c("Eup_sp", "Eup_str"), "Eup_wet", species)) |> 
  mutate(species = ifelse(species == "Gen_ana", "Gen_ama", species)) |> 
  mutate(species = ifelse(species == "Gen_riv" & site == "Gudmedalen", "Geu_riv", species)) |> 
  mutate(species = ifelse(species == "Gen_riv" & site == "Skjellingahaugen", "Gen_niv", species)) |> 
  mutate(species = ifelse(species == "Hup_app", "Hup_sel", species)) |> 
  mutate(species = ifelse(species == "Hyp_sel", "Hup_sel", species)) |> 
  mutate(species = ifelse(species == "Hyp_sp", "Hyp_mac", species)) |> 
  mutate(species = ifelse(species == "Juncus_sp", "Jun_sp", species))|> 
  mutate(species = ifelse(species == "Leu_aut_cf", "Leo_aut_cf", species)) |> 
  mutate(species = ifelse(species == "Lyc_lyc", "Sel_sel", species)) |> 
  mutate(species = ifelse(grepl("Pyr_", species), "Pyr_sp", species)) |> 
  mutate(species = ifelse(species == "Ranunculus", "Ran_sp", species)) |> 
  mutate(species = ifelse(species == "Tri_eur", "Lys_eur", species)) |> 
  mutate(species = ifelse(species == "Val_atr", "Vah_atr", species)) |> 
  mutate(species = ifelse(species == "Vio_riv", "Vio_can", species)) |> 
  distinct()


# 7. We create the cover columns, one for the species and the other for the total vegetation cover of the plot. We combine them with the data

cover_column <- community_long_species |> 
  filter(subPlot == "cover") |> 
  select(year, plotID, species, value) |> 
  rename(cover = value)

vegetation_cover_column <- community_long_species |> 
  filter(!is.na(vegetation_cover)) |>
  select(year, plotID, vegetation_cover) |> 
  distinct()

community_long_cover_warning <- community_long_species |> 
  filter(subPlot != "cover") |> 
  left_join(cover_column, by = c("year", "plotID", "species")) |> 
  select(-vegetation_cover) |> 
  left_join(vegetation_cover_column, by = c("year", "plotID")) |> 
  relocate(c(cover, vegetation_cover), .before = "vegetation_height_mm") |> 
  mutate(measure = case_when(year == 2022 & subPlot == 9 ~ "plot", 
                             TRUE ~ measure)) |> 
  mutate(subPlot = as.numeric(subPlot))
# We get the warning that there are unexpected many-to-many relationships

vegetation_cover_column |> group_by(year, plotID) |> filter(n_distinct(vegetation_cover) > 1L) |> select(year, plotID) |> distinct() # Six plots have more than one value for vegetation cover. This is due to typing mistakes. We fix them here, and look deeper in the problem at each plot
vegetation_cover_column_fixed <- vegetation_cover_column |> 
  mutate(vegetation_cover = ifelse(year == 2019 & plotID == "Gud_1_4", 80, vegetation_cover), 
         vegetation_cover = ifelse(year == 2021 & plotID == "Gud_5_2", 95, vegetation_cover), 
         vegetation_cover = ifelse(year == 2021 & plotID == "Gud_6_1", 95, vegetation_cover), 
         vegetation_cover = ifelse(year == 2021 & plotID == "Lav_2_3", 80, vegetation_cover), 
         vegetation_cover = ifelse(year == 2021 & plotID == "Lav_7_2", 80, vegetation_cover), 
         vegetation_cover = ifelse(year == 2019 & plotID == "Skj_4_2", 55, vegetation_cover)) |> 
  distinct()

community_long_cover <- community_long_species |> 
  filter(subPlot != "cover") |> 
  left_join(cover_column, by = c("year", "plotID", "species")) |> 
  select(-vegetation_cover) |> 
  left_join(vegetation_cover_column_fixed, by = c("year", "plotID")) |> 
  relocate(c(cover, vegetation_cover), .before = "vegetation_height_mm") |> 
  mutate(measure = case_when(year == 2022 & subPlot == 9 ~ "plot", 
                             TRUE ~ measure)) |> 
  mutate(subPlot = as.numeric(subPlot))


## Explaining the different corrections that will be applied----

# A. Wrong values entered for the species (different from the standard 1, cf, d, f, j, o or s). In Skjelligahaugen, at each subplot cover of Sib_pro and Ver_alp was registered as 1 (0-25%), 2 (25%-50%), 3 (50%-75%) or 4 (75%-100%). We take that into account in the function

# B. Wrong values entered for species cover (containing characters different from 0-9, or only 0)

# C. Wrong values entered for vegetation cover, vegetation height or moss depth. They were next to each other, easy to write them in the wrong column. When moss depth was absent we write 0, so that we can compare averages between years and plots

# D. Species written in the wrong subplot. This is checked for subplots 9, 11, 13, 23, 25 and 27. In 2022 species present in the plot, but not in the recorded subplots, were written in subplot 9. We don't count them if they have the correct value (O instead of 1). The rest are typos, or other types of mistakes

# E. In some cases the register was unsure about the species. This was written either in the value (cf) or in the species name (_cf or _sp suffixes). We check the turfplot, and in some cases plots in the same block or site, to decide what species it is, if possible. Alc_sp, Pyr_sp and Tar_sp are considered species by themselves and will not be changed. In some cases we don't even know the genus, this will be explored individually:

# F. When species names are changed, we need to adjust the cover of the species in the subplot and in the plot. Sometimes the species, plot, year combination was missing, and this needed to be imputed by taking the average of the cover of this species in that plot the year before and/or the year after

# G. Other mistakes we have caught up on when looking at the turfplot/scans


# 8. We create functions that look for the possible errors and unknowns, and that create plots for a more visual interpretation

# A function that finds the subplots where the errors we are interested in happen:
find_errors_unknowns <- function(x, y){
  interesting_columns <- c("year", "subPlot", "species", "value")
  result1 <- x |> filter(plotID == y & if_else(site == "Skjellingahaugen" & species %in% c("Sib_pro", "Ver_alp"), (str_detect(value, "(?i)[1234cdfjos_ ]") == FALSE | grepl("?", value, fixed = TRUE) | grepl("*", value, fixed = TRUE) | grepl(".", value, fixed = TRUE)), (str_detect(value, "(?i)[1cdfjos_ ]") == FALSE | grepl("?", value, fixed = TRUE) | grepl("*", value, fixed = TRUE) | grepl(".", value, fixed = TRUE)))) |> select(all_of(interesting_columns))
  result2 <- x |> filter(plotID == y & (is.na(cover) | cover == 0 | !grepl("^[0-9]+$", cover))) |> select(year, species, cover) |> distinct()
  result3 <- x |> filter(plotID == y & (is.na(vegetation_cover) | vegetation_cover == 0 | !grepl("^[0-9]+$", vegetation_cover))) |> select(year, vegetation_cover) |> distinct()
  result4 <- x |> filter(plotID == y & subPlot %in% c(10, 12, 24, 26) & (is.na(vegetation_height_mm) | vegetation_height_mm == 0 | !grepl("^[0-9]+$", vegetation_height_mm))) |> select(year, subPlot, vegetation_height_mm) |> distinct()
  result5 <- x |> filter(plotID == y & subPlot %in% c(10, 12, 24, 26) & (is.na(moss_depth_mm) | !grepl("^[0-9]+$", moss_depth_mm))) |> select(year, subPlot, moss_depth_mm) |> distinct()
  result6 <- x |> filter(plotID == y & subPlot %in% c(9, 11, 13, 23, 25, 27)) |> filter(!(year == 2022 & subPlot == 9 & grepl("O", value, ignore.case = TRUE)))|> select(all_of(interesting_columns))
  result7 <- x |> filter(plotID == y & grepl("cf", value, ignore.case = TRUE)) |> arrange(species) |> select(all_of(interesting_columns))
  result8 <- x |> filter(plotID == y & grepl("_cf", species, ignore.case = TRUE)) |> arrange(species) |> select(all_of(interesting_columns))
  result9 <- x |> filter(plotID == y & (grepl("_sp$", species) | grepl("_sp_", species)) & !(species %in% c("Alc_sp", "Pyr_sp", "Tar_sp"))) |> arrange(species) |> select(all_of(interesting_columns))
  result10 <- x |> filter(plotID == y & !(grepl("[A-Za-z]{3}_[A-Za-z]{3}$", species)) & !(grepl("[A-Za-z]{3}_sp$", species)) & !(grepl("_sp_", species)) & !(grepl("_cf$", species, ignore.case = TRUE))) |> arrange(species) |> select(all_of(interesting_columns))
  result11 <- x |> filter(plotID == y) |> 
    group_by(year, site, plotID, block, plot, warming, treatment, subPlot, species) |> 
    filter(n_distinct(value) > 1L) |> 
    ungroup() |> 
    select(year, subPlot, species, value) |> 
    distinct()
  result12 <- x |> filter(plotID == y) |> 
    group_by(year, site, plotID, block, plot, warming, treatment, species) |> 
    filter(n_distinct(cover) > 1L) |> 
    ungroup() |> 
    select(year, species, cover) |> 
    distinct()
  long_unique <- x |> select(year, plotID, subPlot, species)
  result13 <- long_unique[duplicated(long_unique), ]
  non_empty_results <- list()
  if (nrow(result1) > 0) non_empty_results[["Species value"]] <- result1
  if (nrow(result2) > 0) non_empty_results[["Species cover"]] <- result2
  if (nrow(result3) > 0) non_empty_results[["Vegetation cover"]] <- result3
  if (nrow(result4) > 0) non_empty_results[["Vegetation height"]] <- result4
  if (nrow(result5) > 0) non_empty_results[["Moss depth"]] <- result5
  if (nrow(result6) > 0) non_empty_results[["Subplot number"]] <- result6
  if (nrow(result7) > 0) non_empty_results[["Value cf"]] <- result7
  if (nrow(result8) > 0) non_empty_results[["Species cf"]] <- result8
  if (nrow(result9) > 0) non_empty_results[["Species sp"]] <- result9
  if (nrow(result10) > 0) non_empty_results[["Species unknown"]] <- result10
  if (nrow(result11) > 0) non_empty_results[["Duplicated species - Wrong value"]] <- result11
  if (nrow(result12) > 0) non_empty_results[["Duplicated species - Wrong cover"]] <- result12
  if (nrow(result13) > 0) non_empty_results[["Duplicated species"]] <- result13
  return(non_empty_results)
}

check_species_cover <- function(x, y, z){
  x |> filter(plotID == y & species == z) |> select(year, species, cover) |> distinct()
}

check_species_values <- function(w, x, y, z){
  w |> filter(plotID == x & species %in% c(y, z)) |> select(year, subPlot, species, value) |> distinct()
}

check_vegetation_cover_height_moss_depth <- function(x, y, z){
  x |> filter(year == y & plotID == z) |> select(subPlot, vegetation_cover, vegetation_height_mm, moss_depth_mm) |> distinct() |> print(n = Inf)
}

# We create a function that takes the values and cover of a species from the previous year and uses them on the year of interest (in order to keep everything else correct). We give always the value 1
fix_species_before <- function(w, x, y, z) {
  subPlot_nr <- w |> 
    filter(year == x & plotID == y & species == z) |> 
    pull(subPlot)
  species_cover <- w |> 
    filter(year == x & plotID == y & species == z) |> 
    pull(cover) |> 
    unique()
  subset_w <- w |> 
    filter(year == x + 1 & plotID == y & subPlot %in% subPlot_nr) |> 
    mutate(species = z, value = "1", cover = species_cover) |> 
    distinct()
  return(subset_w)
}

# We adjust the function if we need data from the year after
fix_species_after <- function(w, x, y, z) {
  subPlot_nr <- w |> 
    filter(year == x & plotID == y & species == z) |> 
    pull(subPlot)
  species_cover <- w |> 
    filter(year == x & plotID == y & species == z) |> 
    pull(cover) |> 
    unique()
  subset_w <- w |> 
    filter(year == x - 1 & plotID == y & subPlot %in% subPlot_nr) |> 
    mutate(species = z, value = "1", cover = species_cover) |> 
    distinct()
  return(subset_w)
}

# A 5x7 grid representing the position of the subplots, for the turfmapper
grid <- make_grid(ncol = 7, nrow = 5) |> rename(subPlot = subturf)

# A function that adjust the make_turf_function from turfmapper to not include cover
make_turf_plot <- function(data, grid_long, year, subPlot, species, title) {
  data <- rename(data, 
                 subPlot = {{ subPlot }}, 
                 species = {{ species }}, 
                 year = {{ year }}
  )
  stopifnot(all(data$subPlot %in% grid_long$subPlot))
  data <- left_join(data, grid_long, by = "subPlot")
  ggplot(data, aes(x = .data$.x, y = .data$.y)) + 
    geom_tile(colour = "white", fill = "blue") + 
    facet_grid(species ~ year) + 
    ggtitle(title) + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    theme_bw() + 
    theme(
      axis.text = element_blank(), 
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      panel.spacing.y = unit(0.04, "lines"), 
      panel.grid.minor = element_blank(), 
      strip.text.y = element_text(angle = 0)
    )
}

# A function that makes the turfmapper plot given the dataset and plot ID
turfplot <- function(x, y) {
  x |> 
    filter(plotID == y) |> 
    pipebind::bind(
      x, 
      make_turf_plot(
        data = x, 
        grid_long = grid, 
        year = year, species = species, subPlot = subPlot, 
        title = glue::glue("Site {x$site}: plot {x$plotID}")
      )
    )
}


## Gudmedalen----

# 9. We work on each plot in turn

community_long_cover |> filter(site == "Gudmedalen") |> select(plotID) |> distinct() |> print(n = Inf)
# Gud_1_2, Gud_1_3, Gud_1_4, Gud_1_5, Gud_1_6, Gud_2_1, Gud_2_2, Gud_2_3, Gud_2_4, Gud_3_2, Gud_3_3, Gud_3_5, Gud_3_6, Gud_4_1, Gud_4_3, Gud_4_4, Gud_4_6, Gud_5_1, Gud_5_2, Gud_5_4, Gud_5_5, Gud_6_1, Gud_6_3, Gud_6_4, Gud_6_6, Gud_7_1, Gud_7_2, Gud_7_3, Gud_7_4, Gud_7_6


# Gud_1_2
find_errors_unknowns(community_long_cover, "Gud_1_2")
# Species value/cover. Car_vag 2019, 35 - 3. This is cover, we change it to 1f, and cover to 3
# Species value. 2021 Geu_riv 30 is 1D, not 1D1 (found afterwards, not with this function)
check_vegetation_cover_height_moss_depth(community_long_cover, 2021, "Gud_1_2")
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_1_2")
# Moss depth. No moss in 2021 or 2022, we change it to 0 (logger covered 100% of subplot 12 in 2021, so that subplot is not in the dataset)
# Subplot number. Des_ces 2019, 23. Error, we remove it
turfplot(community_long_cover, "Gud_1_2")
# Car_sp. Seems it is Car_vag in 2018. In the other cases either the other species were already present in the subplot, or none appear any year, and then better to drop them
# Ragnhild: Keeping this as Car_sp because both me and Siri describe it differently than Car_big, and similarly (light green, long, thin, with long tip) (for 2019)

community_gud_1_2 <- community_long_cover |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_1_2" & subPlot == 35 & species == "Car_vag", "1f", value)) |> 
  mutate(value = ifelse(year == 2021 & plotID == "Gud_1_2" & subPlot == 30 & species == "Geu_riv", "1D", value)) |> 
  mutate(moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_1_2" & subPlot %in% c(10, 24, 26), 0, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_1_2" & subPlot %in% c(10, 12, 24, 26), 0, moss_depth_mm)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_1_2" & species == "Car_vag", "3", cover)) |> 
  filter(!(year == 2019 & plotID == "Gud_1_2" & subPlot == 23)) |> 
  mutate(species = ifelse(year == 2018 & plotID == "Gud_1_2" & species == "Car_sp", "Car_vag", species), 
         cover = ifelse(year == 2018 & plotID == "Gud_1_2" & species == "Car_vag", 3, cover)) |> 
  filter(!(plotID == "Gud_1_2" & species == "Car_sp"))

find_errors_unknowns(community_gud_1_2, "Gud_1_2") # We check again, in case we have introduced errors


# Gud_1_3
find_errors_unknowns(community_long_cover, "Gud_1_3")
# Species cover. Tha_alp 2019 - NA. Last columns missing in the scan.  Looking at the same species in other plots/years, it was probably 1
# Species cover. Des_ces 2023 - NA. Not written in the scan. Looking at the same species in other plots/years, it was probably around 5
check_vegetation_cover_height_moss_depth(community_long_cover, 2021, "Gud_1_3")
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_1_3")
# Vegetation height. Not recorded in 2021
# Moss depth. No moss in those subplots and years, we change it to 0
# Subplot number. Pot_ere 2019, 27. Error, it is subplot 26
# Subplot number. Species in 2022: 9. In the plot, but not subplot. We keep them
turfplot(community_long_cover, "Gud_1_3")
# Value cf, Vio_pal. It is Vio_pal
# Car_sp. Seems it is Car_vag in all cases

community_gud_1_3 <- community_gud_1_2 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_1_3" & species == "Tha_alp", 1, cover)) |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Gud_1_3" & species == "Des_ces", 5, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_1_3" & subPlot %in% c(10, 12), 0, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_1_3" & subPlot %in% c(10, 12, 24, 26), 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2019 & plotID == "Gud_1_3" & subPlot == 27, 26, subPlot), 
         moss = ifelse(year == 2019 & plotID == "Gud_1_3" & subPlot == 26, 1, moss), 
         litter = ifelse(year == 2019 & plotID == "Gud_1_3" & subPlot == 26, 40, litter), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Gud_1_3" & subPlot == 26, 120, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Gud_1_3" & subPlot == 26, 0, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_1_3" & subPlot == 2 & species == "Vio_pal", 1, value)) |>  
  mutate(species = ifelse(plotID == "Gud_1_3" & species == "Car_sp", "Car_vag", species), 
         cover = ifelse(year == 2018 & plotID == "Gud_1_3" & species == "Car_vag", 3, cover), 
         cover = ifelse(year == 2019 & plotID == "Gud_1_3" & species == "Car_vag", 5, cover), 
         cover = ifelse(year == 2021 & plotID == "Gud_1_3" & species == "Car_vag", 3, cover))

find_errors_unknowns(community_gud_1_3, "Gud_1_3")


# Gud_1_4
find_errors_unknowns(community_long_cover, "Gud_1_4")
# Species cover. Ver_off 2019 - NA. It is 1, it was written in subplot 35
check_vegetation_cover_height_moss_depth(community_long_cover, 2019, "Gud_1_4")
# Moss depth. It's value is 1. But we see that vegetation cover, vegetation height and moss depth were written in the wrong columns. We fix it
# Subplot number. Ast_alp was in subplot 8, not 9
turfplot(community_long_cover, "Gud_1_4")
# Agr_cap_cf. It is Agr_cap
# Car_sp. Seems it is Car_vag in all cases
# Bet_sp. It is Bet_nan, checked with turfmapper in the field 2023
# Unknown. The recorder says it could be Oxy_dig, but they doubt it. And Oxy_dig has never been found in that block or in the one next to it
# After turfmapper check in the field in 2023, we decided Ave_fle in 2019 and 2022 is actually Fes_rub

community_gud_1_4 <- community_gud_1_3 |> 
  filter(!(year == 2019 & plotID == "Gud_1_4" & subPlot == 35 & species == "Ver_off")) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_1_4" & species == "Ver_off", 1, cover)) |> 
  mutate(vegetation_height_mm = ifelse(year == 2019 & plotID == "Gud_1_4" & subPlot == 10, 55, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Gud_1_4" & subPlot == 12, 93, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Gud_1_4" & subPlot == 24, 94, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Gud_1_4" & subPlot == 26, 89, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Gud_1_4" & subPlot %in% c(10, 12, 24, 26), 1, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2019 & plotID == "Gud_1_4" & subPlot == 9, 8, subPlot), 
         moss = ifelse(year == 2019 & plotID == "Gud_1_4" & subPlot == 8, 1, moss), 
         litter = ifelse(plotID == "Gud_1_4" & year == 2019 & subPlot == 8, 20, litter)) |> 
  mutate(species = ifelse(plotID == "Gud_1_4" & species == "Agr_cap_cf" , "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Gud_1_4" & species == "Bet_sp", "Bet_nan", species)) |> 
  mutate(species = ifelse(plotID == "Gud_1_4" & species == "Car_sp", "Car_vag", species), 
         cover = ifelse(year == 2018 & plotID == "Gud_1_4" & species == "Car_vag", 8, cover), 
         cover = ifelse(year == 2019 & plotID == "Gud_1_4" & species == "Car_vag", 18, cover),
         cover = ifelse(year == 2021 & plotID == "Gud_1_4" & species == "Car_vag", 15, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_1_4" & species == "Ave_fle", "Fes_rub", species)) |> 
  filter(!(plotID == "Gud_1_4" & species == "Unknown")) |> 
  distinct()

find_errors_unknowns(community_gud_1_4, "Gud_1_4")


# Gud_1_5
find_errors_unknowns(community_long_cover, "Gud_1_5")
# Species cover. Nid_seedling. This will be removed, so we don't spend time on it
check_vegetation_cover_height_moss_depth(community_long_cover, 2021, "Gud_1_5")
# Vegetation height and moss depth measured in subplot 17, since logger in 10
# Subplot number. Ast_alp was in subplot 8, not 9
turfplot(community_long_cover, "Gud_1_5")
# Car_fla_CF. It is Car_fla
# Agr_cap_cf. It is Agr_cap
# Car_sp. Seems it is Car_vag in all cases. We adjust the cover to 4
# Epi_sp. Difficult to tell, and not Epilobium ever found in the plot. We remove it

community_gud_1_5 <- community_gud_1_4 |> 
  mutate(species = ifelse(plotID == "Gud_1_5" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Gud_1_5" & species == "Car_fla_CF", "Car_fla", species)) |> 
  mutate(species = ifelse(plotID == "Gud_1_5" & species == "Car_sp", "Car_vag", species), 
         cover = ifelse(year == 2021 & plotID == "Gud_1_5" & species == "Car_vag", 4, cover)) |> 
  filter(!(plotID == "Gud_1_5" & species == "Epi_sp")) |> 
  filter(!(plotID == "Gud_1_5" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_gud_1_5, "Gud_1_5")


# Gud_1_6
find_errors_unknowns(community_long_cover, "Gud_1_6")
# Species cover. Ast_alp. This is 3 (according to Ragnhild)
# Species cover. Car_cap. This is 1. Looking at this we found out that values for Nar_str in 2023 are mixed with those of Pot_ere, which wasn't typed in
# Subplot number. For Phl_alp in 2022 it says only F. We change it to OF, since it was not in the recorded subplots
turfplot(community_long_cover, "Gud_1_6")
# Agr_cap_cf. It is Agr_cap. Missing subplot 24 - We create it
# Leo_aut_cf. It is Leo_aut
# Car_fla in 2019 is probably Car_big. We change it

gud_1_6_2023_nar_str <- community_long_cover |> 
  filter(year == 2023 & plotID == "Gud_1_6" & subPlot %in% c(1, 2, 3, 4, 8, 15, 22, 24, 28, 29, 30, 31)) |> 
  mutate(species = "Nar_str", 
         value = ifelse(subPlot %in% c(1, 2, 3, 4, 8, 15, 22, 28, 29), 1, 
                        ifelse(subPlot %in% c(24, 31), "f", 
                               ifelse(subPlot == 30, "D", FALSE))), 
         cover = "8") |> 
  distinct()

gud_1_6_2023_pot_ere <- community_long_cover |> 
  filter(year == 2023 & plotID == "Gud_1_6" & subPlot %in% c(2, 4, 5, 12, 15, 16, 17, 18, 22, 24, 26, 30, 31, 32, 33, 34)) |> 
  mutate(species = "Pot_ere", 
         value = ifelse(subPlot %in% c(2, 4, 15, 16, 22, 30), 1, 
                        ifelse(subPlot %in% c(5, 12, 17, 24, 26, 33, 34), "f", 
                               ifelse(subPlot %in% c(18, 31, 32), "Df", FALSE))), 
         cover = "11") |> 
  distinct()

gud_1_6_24_2022_agr_cap <- community_long_cover |> 
  filter(year == 2022 & plotID == "Gud_1_6" & subPlot == 24) |> 
  mutate(species = "Agr_cap", 
         value = "1", 
         cover = "2") |> 
  distinct()


community_gud_1_6 <- community_gud_1_5 |> 
  mutate(cover = ifelse(year == 2022 & plotID == "Gud_1_6" & species == "Ast_alp", 1, cover)) |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Gud_1_6" & species == "Car_cap", 1, cover)) |> 
  filter(!(year == 2023 & plotID == "Gud_1_6" & species == "Nar_str")) |> 
  bind_rows(gud_1_6_2023_nar_str) |> 
  bind_rows(gud_1_6_2023_pot_ere) |> 
  mutate(value = ifelse(year == 2022 & plotID == "Gud_1_6" & species == "Phl_alp", "OF", value)) |> 
  mutate(species = ifelse(plotID == "Gud_1_6" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Gud_1_6" & species == "Leo_aut_cf", "Leo_aut", species)) |> 
  bind_rows(gud_1_6_24_2022_agr_cap) |> 
  mutate(species = ifelse(year == 2019 & plotID == "Gud_1_6" & species == "Car_fla", "Car_big", species))

find_errors_unknowns(community_gud_1_6, "Gud_1_6")


# Gud_2_1
find_errors_unknowns(community_long_cover, "Gud_2_1")
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_2_1")
# Vegetation cover not typed in. Moss is present only in subplot 24. For absent, we change it to 0
turfplot(community_long_cover, "Gud_2_1")
# Car_sp. Seems it is Car_big in all cases. Subplot1 must be removed (Car_big fertile in that subplot). No need to adjust cover

community_gud_2_1 <- community_gud_1_6 |> 
  mutate(vegetation_cover = ifelse(year == 2022 & plotID == "Gud_2_1", 100, vegetation_cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_2_1" & subPlot %in% c(10, 12, 26), 0, moss_depth_mm)) |> 
  filter(!(plotID == "Gud_2_1" & subPlot == 1 & species == "Car_sp")) |> 
  mutate(species = ifelse(plotID == "Gud_2_1"& species == "Car_sp", "Car_big", species), 
         cover = ifelse(year == 2019 & plotID == "Gud_2_1"& species == "Car_big", 10, cover)) |> 
  filter(!(plotID == "Gud_2_1" & species == "Nid_seedling"))

find_errors_unknowns(community_gud_2_1, "Gud_2_1")


# Gud_2_2
find_errors_unknowns(community_long_cover, "Gud_2_2")
# Species value. Logger cover is written in Nid_seedling
# Species cover. Luz_mul, cover is 1
# Species cover. Ran_acr (and later Ran_acr_cf). The scans says Rum_ace. But neither of the species grow in this block. I remove it
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_2_2")
# Vegetation height and moss depth. Logger in subplot 10, no moss in the missing subplots, we change it to 0
# Subplot number. Alc_alp: 20 for 23. 
# Subplot number. Ave_fle: 24 for 23, 19 for 22
# Subplot number. Nar_str: 10 is 12
turfplot(community_long_cover, "Gud_2_2")
# Value cf, Car_fla. It is Car_vag, already present in the subplot. We remove it
# Car_sp. Seems it is Car_big in all cases. Car_sp in subplot17 2022 is cf, we remove it. Car_big in subplot34 2023 is D, we remove the Car_sp. No need to change the covers
# Car_sax in 2021 is Car_pal
# Car_pil in 2019 is Car_pal

community_gud_2_2 <- community_gud_2_1 |> 
  mutate(logger = ifelse(year == 2019 & plotID == "Gud_2_2" & subPlot == 3, 15, logger), 
         logger = ifelse(year == 2019 & plotID == "Gud_2_2" & subPlot == 17, 40, logger)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_2_2" & species == "Luz_mul", 1, cover)) |> 
  filter(!(plotID == "Gud_2_2" & species == "Ran_acr")) |> 
  filter(!(plotID == "Gud_2_2" & species == "Ran_acr_cf")) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_2_2" & subPlot %in% c(12, 26), 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Gud_2_2" & subPlot == 23 & species == "Alc_alp", 20, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Gud_2_2" & subPlot == 20, NA, moss), 
         litter = ifelse(year == 2021 & plotID == "Gud_2_2" & subPlot == 20, 70, litter)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Gud_2_2" & subPlot == 19 & species == "Ave_fle", 22, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Gud_2_2" & subPlot == 22, NA, moss), 
         litter = ifelse(year == 2021 & plotID == "Gud_2_2" & subPlot == 22, 70, litter)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Gud_2_2" & subPlot == 23 & species == "Ave_fle", 24, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Gud_2_2" & subPlot == 24, 10, moss), 
         litter = ifelse(year == 2021 & plotID == "Gud_2_2" & subPlot == 24, 60, litter), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Gud_2_2" & subPlot == 24, 75, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_2_2" & subPlot == 24, 13, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2022 & plotID == "Gud_2_2" & subPlot == 10 & species == "Nar_str", 12, subPlot), 
         litter = ifelse(year == 2022 & plotID == "Gud_2_2" & subPlot == 12, 20, litter), 
         vegetation_height_mm = ifelse(year == 2022 & plotID == "Gud_2_2" & subPlot == 12, 110, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_2_2" & subPlot == 12, 0, moss_depth_mm)) |> 
  filter(!(year == 2022 & plotID == "Gud_2_2" & subPlot == 26 & species == "Car_fla")) |> 
  filter(!(year == 2022 & plotID == "Gud_2_2" & subPlot == 17 & species == "Car_sp")) |> 
  filter(!(year == 2023 & plotID == "Gud_2_2" & subPlot == 34 & species == "Car_sp")) |> 
  mutate(species = ifelse(plotID == "Gud_2_2" & species == "Car_sp", "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Gud_2_2" & species == "Car_big", 6, cover), 
         cover = ifelse(year == 2023 & plotID == "Gud_2_2" & species == "Car_big", 20, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_2_2" & species == "Car_sax", "Car_pal", species), 
         cover = ifelse(year == 2021 & plotID == "Gud_2_2" & species == "Car_pal", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_2_2" & species == "Car_pil", "Car_pal", species)) |> 
  filter(!(plotID == "Gud_2_2" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_gud_2_2, "Gud_2_2")


# Gud_2_3
find_errors_unknowns(community_long_cover, "Gud_2_3")
# Species value. 2023 Bis_viv says 11 (found out later, not with this function), when it should be 1
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_2_3")
# No moss in the missing subplots, we change it to 0
turfplot(community_long_cover, "Gud_2_3")
# Vio_tri_cf is Vio_tri
# Car_sp. Seems it is Car_big in all cases, we change cover to 4

community_gud_2_3 <- community_gud_2_2 |> 
  mutate(value = ifelse(year == 2023 & plotID == "Gud_2_3" & subPlot == 7 & species == "Bis_viv", 1, value)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_2_3" & subPlot %in% c(10, 12, 24), 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Gud_2_3" & species == "Vio_tri_cf", "Vio_tri", species)) |> 
  mutate(species = ifelse(plotID == "Gud_2_3" & species == "Car_sp", "Car_big", species), 
         cover = ifelse(year == 2021 & plotID == "Gud_2_3" & species == "Car_big", 4, cover)) |> 
  filter(!(plotID == "Gud_2_3" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_gud_2_3, "Gud_2_3")


# Gud_2_4
find_errors_unknowns(community_long_cover, "Gud_2_4")
check_vegetation_cover_height_moss_depth(community_long_cover, 2021, "Gud_2_4")
# Vegetation height and moss depth measured in subplot 31, since logger was in 24
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_2_4")
# Moss depth. Not typed in. One subplot without, we change it to 0
turfplot(community_long_cover, "Gud_2_4")
# Ver_ser_cf is Ver_alp, we change cover to 5
# Car_sp. Seems it is Car_big in all cases

community_gud_2_4 <- community_gud_2_3 |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_2_4" & subPlot == 10, 0, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_2_4" & subPlot == 12, 26, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_2_4" & subPlot == 24, 3, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_2_4" & subPlot == 26, 3, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Gud_2_4" & species == "Ver_ser_cf", "Ver_alp", species), 
         cover = ifelse(year == 2019 & plotID == "Gud_2_4" & species == "Ver_alp", 5, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_2_4" & species == "Car_sp", "Car_big", species), 
         cover = ifelse(year == 2019 & plotID == "Gud_2_4" & species == "Car_big", 6, cover)) |> 
  filter(!(plotID == "Gud_2_4" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_gud_2_4, "Gud_2_4")


# Gud_3_2
find_errors_unknowns(community_long_cover, "Gud_3_2")
# Species cover. Cam_rot cover written in subplot 35
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_3_2")
# Moss depth. No moss in the missing subplots, we change it to 0
turfplot(community_long_cover, "Gud_3_2")
# Value cf. Both are Ver_alp
# Car_big_cf is Car_big
# Car_sp. Seems it is Car_big in 2018 and Car_vag in 2019, no need to change cover

community_gud_3_2 <- community_gud_2_4 |> 
  filter(!(year == 2021 & plotID == "Gud_3_2" & subPlot == 35 & species == "Cam_rot")) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_3_2" & species == "Cam_rot", 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_3_2" & subPlot %in% c(12, 24, 26), 0, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_3_2" & subPlot %in% c(4, 15) & species == "Ver_alp", "j", value)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_3_2" & species == "Ver_alp", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_3_2" & species == "Car_big_cf", "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Gud_3_2" & species == "Car_big", 5, cover)) |> 
  mutate(species = ifelse(year == 2018 & plotID == "Gud_3_2" & species == "Car_sp", "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Gud_3_2" & species == "Car_big", 5, cover)) |> 
  mutate(species = ifelse(year == 2019 & plotID == "Gud_3_2" & species == "Car_sp", "Car_vag", species), 
         cover = ifelse(year == 2019 & plotID == "Gud_3_2" & species == "Car_vag", 5, cover)) |> 
  filter(!(plotID == "Gud_3_2" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_gud_3_2, "Gud_3_2")


# Gud_3_3
find_errors_unknowns(community_long_cover, "Gud_3_3")
# Species cover. Ver_alp was crossed out. But it said 3, which fits with values from other years
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_3_3")
# Moss depth. No moss in the missing subplots, we change it to 0
turfplot(community_long_cover, "Gud_3_3")
# Value cf. Car_vag is correct
# Value cf. Ran_acr is correct
# Car_sp. Seems it is Car_big in all cases, change cover to 3
# In 2019 Fes_rub was registered, when this was actually Ave_fle
# Vio_pal in 2019 was Vio_bif. We correct it and adjust the cover to 11

community_gud_3_3 <- community_gud_3_2 |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_3_3" & species == "Ver_alp", 3, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_3_3" & subPlot %in% c(10, 24), 0, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_3_3" & subPlot == 8 & species == "Car_vag", 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_3_3" & subPlot == 26 & species == "Ran_acr", "s", value)) |> 
  mutate(species = ifelse(plotID == "Gud_3_3" & species == "Car_sp", "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Gud_3_3" & species == "Car_big", 3, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_3_3" & species == "Fes_rub", "Ave_fle", species)) |> 
  mutate(species = ifelse(plotID == "Gud_3_3" & species == "Vio_pal", "Vio_bif", species)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_3_3" & species == "Vio_bif", 11, cover)) |> 
  filter(!(plotID == "Gud_3_3" & species == "Nid_seedling"))

find_errors_unknowns(community_gud_3_3, "Gud_3_3")


# Gud_3_5
find_errors_unknowns(community_long_cover, "Gud_3_5")
# Species value. Ver_alp is correct
# Species cover. Ave_fle 2019 - Not recorded, we estimate it to be 1
# Species cover. Car_big 2019 - Not recorded, we estimate it to be 1
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_3_5")
# Moss depth. No moss in the missing subplots, we change it to 0
# Subplot number. Vio_bif:  we remove it
turfplot(community_long_cover, "Gud_3_5")
# Hie_sp is Hie_alp
# Car_sp. Seems it is Car_vag in all cases. Change cover to 2
# Fes_sp. Seems it is Fes_rub

community_gud_3_5 <- community_gud_3_3 |> 
  mutate(value = ifelse(year == 2021 & plotID == "Gud_3_5" & subPlot == 4 & species == "Ver_alp", 1, value)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_3_5" & species == "Ave_fle", 1, cover)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_3_5" & species == "Car_big", 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_3_5" & subPlot %in% c(12, 24, 26), 0, moss_depth_mm)) |> 
  filter(!(year == 2021 & plotID == "Gud_3_5" & subPlot == 9)) |> 
  mutate(species = ifelse(plotID == "Gud_3_5" & species == "Hie_sp", "Hie_alp", species)) |> 
  mutate(species = ifelse(plotID == "Gud_3_5" & species == "Car_sp", "Car_vag", species)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_3_5" & species == "Car_vag", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_3_5" & species == "Fes_sp", "Fes_rub", species))

find_errors_unknowns(community_gud_3_5, "Gud_3_5")


# Gud_3_6
find_errors_unknowns(community_long_cover, "Gud_3_6")
# Vegetation cover not recorded
turfplot(community_long_cover, "Gud_3_6")
# Car_sp is Car_cap in 2019 and Car_nor in 2021, based on comments from field sheet
# Gal_sp. Not enough information, we keep it as Gal_sp
# Hie_sp and Hie_alp are Hie_pil

community_gud_3_6 <- community_gud_3_5 |> 
  mutate(species = ifelse(year == 2019 & plotID == "Gud_3_6" & species == "Car_sp", "Car_cap", species)) |> 
  mutate(species = ifelse(year == 2021 & plotID == "Gud_3_6" & species == "Car_sp", "Car_nor", species)) |> 
  mutate(species = ifelse(plotID == "Gud_3_6" & species %in% c("Hie_sp", "Hie_alp"), "Hie_pil", species))

find_errors_unknowns(community_gud_3_6, "Gud_3_6")


# Gud_4_1
find_errors_unknowns(community_long_cover, "Gud_4_1")
# Species cover. Ast_alp 2019 - Not recorded, we estimate it to be 1
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_4_1")
# Moss depth. No moss in the missing subplots, we change it to 0 (I do it after adding the Car_vag from 2021)
turfplot(community_long_cover, "Gud_4_1")
# Agr_cap_cf. Written in the wrong column. This is Ach_mil
# Car_nor_cf. It is Car_big, adjust the cover to 2
# Car_sp. I remove it
# Hie_sp. Only found once in the plot. In that block and the one next to it has Hie_pil been found several times, we change it
# Agr_cap was registered as Agr_mer in 2021, we fix it
# Seems Car_fla was not recorded in 2018, we add information from 2019
# Seems Car_vag was not recorded in 2022, we add information from 2021

gud_4_1_2018_car_fla <- fix_species_after(community_long_cover, 2019, "Gud_4_1", "Car_fla")
gud_4_1_2022_car_vag <- fix_species_before(community_long_cover, 2021, "Gud_4_1", "Car_vag")

community_gud_4_1 <- community_gud_3_6 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_4_1" & species == "Ast_alp", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_4_1" & species == "Agr_cap_cf", "Ach_mil", species)) |> 
  mutate(species = ifelse(year == 2019 & plotID == "Gud_4_1" & species == "Car_nor_cf", "Car_big", species), 
         cover = ifelse(year == 2019 & plotID == "Gud_4_1" & species == "Car_big", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_4_1" & species == "Hie_sp", "Hie_pil", species)) |> 
  mutate(species = ifelse(plotID == "Gud_4_1" & species == "Agr_mer", "Agr_cap", species)) |> 
  filter(!(plotID == "Gud_4_1" & species == "Car_sp")) |> 
  bind_rows(gud_4_1_2018_car_fla) |> 
  bind_rows(gud_4_1_2022_car_vag) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_4_1" & subPlot %in% c(10, 12, 24, 26), 0, moss_depth_mm)) |> 
  distinct()

find_errors_unknowns(community_gud_4_1, "Gud_4_1")


# Gud_4_3
find_errors_unknowns(community_long_cover, "Gud_4_3")
# Species cover. Ast_alp 2021 - Not recorded, we used that from 2019, 3
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_4_3")
# Moss depth. No moss in the missing subplots, we change it to 0
turfplot(community_long_cover, "Gud_4_3")
# Car_big_cf is Car_big
# Car_cap_cf seems to be Car_fla
# Car_nor_cf is Car_nor
# Car_sp. Cannot really tell, and since other Carex are already present in those subplots, I drop these
# Hie_sp. Hie_pil is the only Hie found in that block

community_gud_4_3 <- community_gud_4_1 |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_4_3" & species == "Ast_alp", 3, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_4_3" & subPlot %in% c(12, 24, 26), 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Gud_4_3" & species == "Car_big_cf", "Car_big", species)) |> 
  mutate(species = ifelse(plotID == "Gud_4_3" & species == "Car_cap_cf", "Car_fla", species)) |> 
  mutate(species = ifelse(plotID == "Gud_4_3" & species == "Car_nor_cf","Car_nor", species )) |> 
  filter(!(plotID == "Gud_4_3" & species == "Car_sp")) |> 
  mutate(species = ifelse(plotID == "Gud_4_3" & species == "Hie_sp", "Hie_pil", species))

find_errors_unknowns(community_gud_4_3, "Gud_4_3")


# Gud_4_4
find_errors_unknowns(community_long_cover, "Gud_4_4")
# Species cover. Agr_cap 2019 - Not recorded, we estimate it to be 1
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_4_4")
# Moss depth. No moss in the missing subplots, we change it to 0
# Subplot number. Nar_str: 13 for 12, 17 for 14. Missing 18 - We create it
turfplot(community_long_cover, "Gud_4_4")
# Value cf. It is Alc_alp
# Car_cap_cf is indeed Car_cap
# Car_sp. All other Carex already present in those subplots. I drop them
# Cer_sp is Cer_fon. We change cover to 2

gud_4_4_18_2021_nar_str <- community_long_cover |>
  filter(year == 2021 & plotID == "Gud_4_4" & subPlot == 18 & species == "Car_big") |> 
  mutate(species = "Nar_str", 
         cover = "25")

community_gud_4_4 <- community_gud_4_3 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_4_4" & species == "Agr_cap", 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_4_4" & subPlot %in% c(10, 26), 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Gud_4_4" & subPlot == 13, 12, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Gud_4_4" & subPlot == 12, 5, moss), 
         litter = ifelse(year == 2021 & plotID == "Gud_4_4" & subPlot == 12, 70, litter), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Gud_4_4" & year == 2021 & subPlot == 12, 70, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_4_4" & subPlot == 12, 5, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Gud_4_4" & subPlot == 17 & species == "Nar_str", 14, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Gud_4_4" & subPlot == 14, 1, moss), 
         litter = ifelse(year == 2021 & plotID == "Gud_4_4" & subPlot == 14, 85, litter)) |> 
  bind_rows(gud_4_4_18_2021_nar_str) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_4_4" & subPlot %in% c(3, 30) & species == "Alc_alp", "J", value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_4_4" & subPlot == 5 & species == "Alc_alp", "S", value)) |> 
  mutate(species = ifelse(plotID == "Gud_4_4" & species == "Car_cap_cf", "Car_cap", species)) |> 
  filter(!(species == "Car_sp" & plotID == "Gud_4_4")) |> 
  mutate(species = ifelse(plotID == "Gud_4_4" & species == "Cer_sp", "Cer_fon", species), 
         cover = ifelse(year == 2019 & plotID == "Gud_4_4" & species == "Cer_fon", 2, cover))

find_errors_unknowns(community_gud_4_4, "Gud_4_4")


# Gud_4_6
find_errors_unknowns(community_long_cover, "Gud_4_6")
# Species cover. Par_pal - Not typed in, it is 1
check_vegetation_cover_height_moss_depth(community_long_cover, 2021, "Gud_4_6")
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_4_6")
# Moss depth. No moss in the missing subplots, we change it to 0
# Subplot number. Tha_alp: 9 is 10 and 11 is 12
turfplot(community_long_cover, "Gud_4_6")
# Value cf. We keep both Cer_fon and Par_pal
# Cer_sp is Cer_fon

community_gud_4_6 <- community_gud_4_4 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_4_6" & species == "Par_pal", 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_4_6" & subPlot %in% c(10, 24), 0, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_4_6" & subPlot %in% c(10, 12, 24), 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2023 & plotID == "Gud_4_6" & subPlot == 9 , 10, subPlot), 
         litter = ifelse(year == 2023 & plotID == "Gud_4_6" & subPlot == 10, 80, litter), 
         vegetation_height_mm = ifelse(year == 2023 & plotID == "Gud_4_6" & subPlot == 10, 100, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2023 & plotID == "Gud_4_6" & subPlot == 10, 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2023 & plotID == "Gud_4_6" & subPlot == 11 , 12, subPlot), 
         moss = ifelse(year == 2023 & plotID == "Gud_4_6" & subPlot == 12, 5, moss), 
         litter = ifelse(year == 2023 & plotID == "Gud_4_6" & subPlot == 12, 70, litter), 
         vegetation_height_mm = ifelse(year == 2023 & plotID == "Gud_4_6" & subPlot == 12, 60, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2023 & plotID == "Gud_4_6" & subPlot == 12, 20, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_4_6" & subPlot == 18 & species == "Cer_fon", "J", value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_4_6" & species == "Par_pal", "J", value)) |> 
  mutate(species = ifelse(plotID == "Gud_4_6" & species == "Cer_sp", "Cer_fon", species))

find_errors_unknowns(community_gud_4_6, "Gud_4_6")


# Gud_5_1
find_errors_unknowns(community_long_cover, "Gud_5_1")
check_vegetation_cover_height_moss_depth(community_long_cover, 2021, "Gud_5_1")
# Vegetation height and moss depth measured in subplot 17, since logger in 10
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_5_1")
# Moss depth. No moss in the missing subplots, we change it to 0
turfplot(community_long_cover, "Gud_5_1")
# Car_sp. Seems it is Car_cap
# Sal_sp. Seems it is Sal_lan
# Unknown. We cannot tell
# Seems Fes_rub has been mistaken for Ave_fle and Tri_ces
# Seems Car_sax has been mistaken for Car_atr

community_gud_5_1 <- community_gud_4_6 |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_5_1" & subPlot %in% c(24, 26), 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Gud_5_1" & species == "Car_sp", "Car_cap", species), 
         cover = ifelse(year == 2019 & plotID == "Gud_5_1" & species == "Car_cap", 3, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_5_1" & species == "Sal_sp", "Sal_lan", species)) |> 
  mutate(species = ifelse(plotID == "Gud_5_1" & species %in% c("Ave_fle", "Tri_ces"), "Fes_rub", species)) |> 
  mutate(species = ifelse(plotID == "Gud_5_1" & species == "Car_atr", "Car_sax", species)) |> 
  filter(!(plotID == "Gud_5_1" & species == "Unknown"))


find_errors_unknowns(community_gud_5_1, "Gud_5_1")


# Gud_5_2
find_errors_unknowns(community_long_cover, "Gud_5_2")
# Species value. Ver_alp: 1*. Ver_alp was not found in this plot, we remove it
check_vegetation_cover_height_moss_depth(community_long_cover, 2021, "Gud_5_2")
# Vegetation height written on vegetation cover, and moss depth on vegetation height
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_5_2")
# Moss depth. No moss in the missing subplotsm, we change it to 0
# Subplot number. Species were recorded in all subplots. We remove from these subplots
turfplot(community_long_cover, "Gud_5_2")
# Car_sp. Car_sax in 2019, not sure in 2018 (we remove it)
# Epi_sp is Epi_ana
# Unknown. The orchid is Dac_vir
# Cover of Oxy_dig in 2021 is 6, not 65

community_gud_5_2 <- community_gud_5_1 |> 
  filter(!(year == 2022 & plotID == "Gud_5_2" & subPlot == 18 & species == "Ver_alp")) |> 
  mutate(vegetation_height_mm = ifelse(year == 2021 & plotID == "Gud_5_2" & subPlot == 12, 150, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_5_2" & subPlot == 12, 0, moss_depth_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Gud_5_2" & subPlot == 24, 150, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_5_2" & subPlot == 24, 0, moss_depth_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Gud_5_2" & subPlot == 26, 160, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_5_2" & subPlot == 26, 0, moss_depth_mm)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_5_2" & subPlot %in% c(12, 24, 26), 0, moss_depth_mm)) |> 
  filter(!(year == 2021 & plotID == "Gud_5_2" & subPlot %in% c(9, 11, 13, 23, 25, 27))) |> 
  mutate(species = ifelse(year == 2019 & plotID == "Gud_5_2" & species == "Car_sp", "Car_sax", species)) |> 
  filter(!(year == 2018 & plotID == "Gud_5_2" & species == "Car_sp")) |> 
  mutate(species = ifelse(plotID == "Gud_5_2" & species == "Epi_sp", "Epi_ana", species)) |> 
  mutate(species = ifelse(plotID == "Gud_5_2" & species %in% c("Nid_orchid", "Orchid"), "Dac_vir", species)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_5_2" & species == "Oxy_dig", 6, cover)) |> 
  filter(!(plotID == "Gud_5_2" & species %in% c("Nid_juvenile", "Nid_seedling")))

find_errors_unknowns(community_gud_5_2, "Gud_5_2")


# Gud_5_4
find_errors_unknowns(community_long_cover, "Gud_5_4")
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_5_4")
# Moss depth. No moss in the missing subplots, we change it to 0
# Subplot number. Species were recorded in all subplots. We remove from these subplots
turfplot(community_long_cover, "Gud_5_4")
# Car_sp. Seems it is Car_big
# After checking the turfmapper in the field in 2023,  we change Fes_rub for Ave_fle, Leo_aut for Tar_sp and Lyc_alp for Hup_sel

community_gud_5_4 <- community_gud_5_2 |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_5_4" & subPlot %in% c(12, 24, 26), 0, moss_depth_mm)) |> 
  filter(!(year == 2021 & plotID == "Gud_5_4" & subPlot %in% c(9, 11, 13, 23, 25, 27))) |> 
  mutate(species = ifelse(plotID == "Gud_5_4" & species == "Car_sp", "Car_big", species)) |> 
  mutate(species = ifelse(year == 2018 & plotID == "Gud_5_4" & species == "Fes_rub", "Ave_fle", species))|> 
  mutate(species = ifelse(plotID == "Gud_5_4" & species == "Leo_aut", "Tar_sp", species), 
         cover = ifelse(year == 2018 & plotID == "Gud_5_4" & species == "Tar_sp", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_5_4" & species == "Lyc_alp", "Hup_sel", species)) |> 
  filter(!(plotID == "Gud_5_4" & species == "Nid_seedling"))

find_errors_unknowns(community_gud_5_4, "Gud_5_4")


# Gud_5_5
find_errors_unknowns(community_long_cover, "Gud_5_5")
# Species value. Car_big: 1*. Uncertainty about the species, we look at the turfplot
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_5_5")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_long_cover, "Gud_5_5")
# Species value. Car_big: 1*. These are Car_fla. No need to adjust Car_big cover
# Value cf. Par_pal 2019, not really able to tell, but probably something different. Remove it. In 2022 it's wrong, this is Hie_pil
# Value cf. Pot_ere is correct, Pyr_sp is left like that (value = 1)
# Car_sp. Seems it is Car_vag in all cases
# Hie_sp. It is Hie_pil
# Unknown. This is Hup_sel. And it seems to have been confused with Lyc_alp and Sel_sel in other years. We correct them
# Pot_cra in 2019 is Pot_ere. We change cover to 9

community_gud_5_5 <- community_gud_5_4 |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_5_5" & subPlot == 10, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(year == 2021 & plotID == "Gud_5_5" & subPlot %in% c(6, 14) & species == "Car_big", "Car_fla", species), 
         value = ifelse(year == 2021 & plotID == "Gud_5_5" & subPlot %in% c(6, 14) & species == "Car_fla", 1, value)) |> 
  filter(!(year == 2019 & plotID == "Gud_5_5" & species == "Par_pal")) |> 
  mutate(species = ifelse(year == 2022 & plotID == "Gud_5_5" & species == "Par_pal", "Hie_pil", species)) |> 
  mutate(value = ifelse(year == 2022 & plotID == "Gud_5_5" & species == "Hie_pil", 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_5_5" & subPlot == 12 & species == "Pot_ere", "j", value)) |> 
  mutate(value = ifelse(year == 2022 & plotID == "Gud_5_5" & subPlot %in% c(16, 24) & species == "Pyr_sp", 1, value)) |> 
  mutate(species = ifelse(plotID == "Gud_5_5" & species == "Car_sp", "Car_vag", species), 
         value = ifelse(year == 2022 & plotID == "Gud_5_5" & subPlot == 20 & species == "Car_vag", 1, value), 
         cover = ifelse(year == 2018 & plotID == "Gud_5_5" & species == "Car_vag", 2, cover), 
         cover = ifelse(year == 2022 & plotID == "Gud_5_5" & species == "Car_vag", 6, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_5_5" & species == "Hie_sp", "Hie_pil", species)) |> 
  mutate(species = ifelse(year == 2021 & plotID == "Gud_5_5" & species == "Unknown", "Hup_sel", species)) |> 
  mutate(species = ifelse(plotID == "Gud_5_5" & species %in% c("Lyc_alp","Sel_sel"), "Hup_sel", species)) |> 
  mutate(species = ifelse(plotID == "Gud_5_5" & species == "Pot_cra", "Pot_ere", species), 
         cover = ifelse(year == 2019 & plotID == "Gud_5_5" & species == "Pot_ere", 9, cover))

find_errors_unknowns(community_gud_5_5, "Gud_5_5")


# Gud_6_1
find_errors_unknowns(community_long_cover, "Gud_6_1")
check_vegetation_cover_height_moss_depth(community_long_cover, 2021, "Gud_6_1")
# Vegetation height written on vegetation cover, and moss depth on vegetation height. Subplot 17 measured instead of 10
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_6_1")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_long_cover, "Gud_6_1")
# Unknown is Sol_vir. Hil_pil is also Sol_vir
# In 2018 and 2022 some Vio_pal was confounded with Vio_bif and Vio_can. We fix it. We change cover in 2018 to 4 and in 2022 to 3

community_gud_6_1 <- community_gud_5_5 |> 
  mutate(vegetation_cover = ifelse(year == 2021 & plotID == "Gud_6_1", 95, vegetation_cover), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Gud_6_1" & subPlot == 12, 95, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Gud_6_1" & subPlot == 17, 80, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Gud_6_1" & subPlot == 24, 110, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Gud_6_1" & subPlot == 26, 105, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_6_1" & subPlot == 12, 0, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_6_1" & subPlot == 17, 0, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_6_1" & subPlot == 24, 0, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_6_1" & subPlot == 26, 0, moss_depth_mm)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_6_1" & subPlot == 24, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Gud_6_1" & species == "Unknown", "Sol_vir", species)) |> 
  mutate(species = ifelse(plotID == "Gud_6_1" & species == "Hie_pil", "Sol_vir", species)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_6_1" & species == "Sol_vir", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_6_1" & species %in% c("Vio_bif", "Vio_can"), "Vio_pal", species)) |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Gud_6_1" & species =="Vio_pal", 4, cover)) |> 
  mutate(cover = ifelse(year == 2022 & plotID == "Gud_6_1" & species =="Vio_pal", 6, cover)) |> 
  filter(!(plotID == "Gud_6_1" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_gud_6_1, "Gud_6_1")


# Gud_6_3
find_errors_unknowns(community_long_cover, "Gud_6_3")
# Species cover. Sil_aca 2019 - Not recorded, we estimate it to be 1
check_vegetation_cover_height_moss_depth(community_long_cover, 2021, "Gud_6_3")
# Vegetation height and moss depth were measured in subplot 31, since logger was in 24
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_6_3")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_long_cover, "Gud_6_3")
# Car_cap_cf is Car_cap
# Car_fla_cf, Car_sp, Car_fla. After turfmapper check in 2023, and looking over comments in the field sheets I am calling it all Car_vag.  in 2018, in 2019 and 2021 we change it to 7
# Vio_can_cf is Vio_can

community_gud_6_3 <- community_gud_6_1 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_6_3" & species =="Sil_aca", 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_6_3" & subPlot == 12, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Gud_6_3" & species == "Car_cap_cf", "Car_cap", species)) |> 
  mutate(value = ifelse(plotID == "Gud_6_3" & species == "Car_fla_CF", 1, value)) |> 
  mutate(species = ifelse(plotID == "Gud_6_3" & species %in% c("Car_fla", "Car_fla_CF", "Car_sp"), "Car_vag", species)) |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Gud_6_3" & species =="Car_vag", 5, cover)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_6_3" & species =="Car_vag", 7, cover)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_6_3" & species =="Car_vag", 7, cover)) |> 
  mutate(species = ifelse(plotID == "Gud_6_3" & species == "Vio_can_cf", "Vio_can", species)) |> 
  distinct()

find_errors_unknowns(community_gud_6_3, "Gud_6_3")


# Gud_6_4
find_errors_unknowns(community_long_cover, "Gud_6_4")
# Species cover. Ant_odo 2021 written in subplot 35
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_6_4")
# Moss depth. No moss in the missing subplots, we change it to 0
turfplot(community_long_cover, "Gud_6_4")
# Ant_sp is Ant_dio
# Gal_sp. We cannot really tell, we keep it as Gal_sp

community_gud_6_4 <- community_gud_6_3 |> 
  filter(!(year == 2021 & plotID == "Gud_6_4" & subPlot == 35 & species == "Ant_odo")) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_6_4" & species == "Ant_odo", 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_6_4" & subPlot %in% c(12, 24), 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Gud_6_4" & species == "Ant_sp", "Ant_dio", species))

find_errors_unknowns(community_gud_6_4, "Gud_6_4")


# Gud_6_6
find_errors_unknowns(community_long_cover, "Gud_6_6")
check_vegetation_cover_height_moss_depth(community_long_cover, 2021, "Gud_6_6")
# Vegetation cover not typed in, it is 90
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_6_6")
# Moss depth. No moss in the missing subplots, we change it to 0
turfplot(community_long_cover, "Gud_6_6")
# Value cf. Hil_pil is correct
# Ant_sp is Ant_dio
# Ave_fle in 2023 - Called Fes_rub every other year, we change it
# 2018 Litter written in lichen row

community_gud_6_6 <- community_gud_6_4 |> 
  mutate(vegetation_cover = ifelse(year == 2021 & plotID == "Gud_6_6", 90, vegetation_cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_6_6" & subPlot %in% c(10, 12, 24, 26), 0, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2021 & plotID == "Gud_6_6" & subPlot == 31 & species == "Hie_pil", 1, value))|>
  mutate(species = ifelse(plotID == "Gud_6_6" & species == "Ant_sp", "Ant_dio", species)) |> 
  mutate(species = ifelse(plotID == "Gud_6_6" & species == "Ave_fle", "Fes_rub", species)) |> 
  mutate(litter = ifelse(year == 2018 & plotID == "Gud_6_6", lichen, litter), 
         lichen = ifelse(year == 2018 & plotID == "Gud_6_6", NA, lichen))

find_errors_unknowns(community_gud_6_6, "Gud_6_6")


# Gud_7_1
find_errors_unknowns(community_long_cover, "Gud_7_1")
# Species cover. We equate 0.1 to 1
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_7_1")
# Moss depth. No moss in the missing subplots, we change it to 0
# Subplot number. They're shifted to the right. And we change 7 for 14
turfplot(community_long_cover, "Gud_7_1")
# Value cf. Car_pal is correct
# Car_big_cf and Car_sp are Car_big. We change cover to 3
# We adjust the cover of Eup_wet to 4 (see point 6.)

community_gud_7_1 <- community_gud_6_6 |> 
  mutate(cover = ifelse(plotID == "Gud_7_1" & cover == 0.1, 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_7_1" & subPlot %in% c(10, 12, 26), 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 9, 8, subPlot), 
         moss = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 8, 15, moss), 
         litter = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 8, 50, litter)) |> 
  mutate(subPlot = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 11, 10, subPlot), 
         moss = ifelse(plotID == "Gud_7_1" & year == 2019 & subPlot == 10, 5, moss), 
         litter = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 10, 30, litter), 
         bare_ground = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 10, 15, bare_ground), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Gud_7_1" & year == 2019 & subPlot == 10, 90, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 10, 20, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 13, 12, subPlot), 
         litter = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 12, 25, litter), 
         bare_ground = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 12, 10, bare_ground), 
         logger = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 12, 10, logger), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 12, 100, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 12, 25, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 7 & species == "Tha_alp", 14, subPlot), 
         litter = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 14, 25, litter), 
         bare_ground = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 14, 15, bare_ground)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_7_1" & subPlot == 2 & species == "Car_pal", 1, value)) |> 
  mutate(species = ifelse(plotID == "Gud_7_1" & species %in% c("Car_big_cf", "Car_sp"), "Car_big", species)) |>
  mutate(cover = ifelse(year == 2018 & plotID == "Gud_7_1" & species == "Car_big", 3, cover)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_7_1" & species == "Eup_wet", 4, cover))

find_errors_unknowns(community_gud_7_1, "Gud_7_1")


# Gud_7_2
find_errors_unknowns(community_long_cover, "Gud_7_2")
# Species value. Car_sp 35, 0.1. This was cover, and there is no Car_sp in this subplot
# Species cover. Car_vag is 12 (wasn't typed in). For Jun_tri and Eup_wet we equate 0.1 to 1
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_7_2")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_long_cover, "Gud_7_2")
# Value cf. Ver_cha. The datasheet says Ver_alp J cf. So changed to that
# Car_big_cf is Car_big
# Car_sp. Car_big in 2018, not enough information in 2021
# 2018 Litter written in lichen row

community_gud_7_2 <- community_gud_7_1 |> 
  filter(!(year == 2021 & plotID == "Gud_7_2" & subPlot == 35 & species == "Car_sp")) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_7_2" & species == "Car_vag", 12, cover)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_7_2" & species == "Eup_wet", 1, cover)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_7_2" & species == "Jun_tri", 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_7_2" & subPlot == 10, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(year == 2019 & plotID == "Gud_7_2" & species == "Ver_cha", "Ver_alp", species), 
         value = ifelse(year == 2019 & plotID == "Gud_7_2" & subPlot == 7 & species == "Ver_alp", "J", value)) |> 
  mutate(species = ifelse(plotID == "Gud_7_2" & species == "Car_big_cf", "Car_big", species)) |> 
  mutate(species = ifelse(year == 2018 & plotID == "Gud_7_2" & species == "Car_sp", "Car_big", species)) |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Gud_7_2" & species == "Car_big", 2, cover)) |> 
  filter(!(year == 2021 & plotID == "Gud_7_2" & species == "Car_sp")) |> 
  mutate(litter = ifelse(year == 2018 & plotID == "Gud_7_2", lichen, litter), 
         lichen = ifelse(year == 2018 & plotID == "Gud_7_2", NA, lichen))

find_errors_unknowns(community_gud_7_2, "Gud_7_2")


# Gud_7_3
find_errors_unknowns(community_long_cover, "Gud_7_3")
# Species value. 2019 Cer_fon says 11 (found out later, not with this function), when it should be 1
# Species cover. We equate 0.1 to 1
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_7_3")
# Moss depth. No moss in the missing subplots, we change it to 0
turfplot(community_long_cover, "Gud_7_3")
# Value cf. Ast_alp is correct
# Agr_cap_cf is Agr_cap
# Car_sp. Car_big in 2018, we change cover to 2. Cannot really tell for 2021, we remove it
# Gen_sp is Gen_niv. We correct the value
# Hie_sp. Cannot really tell, we keep it

community_gud_7_3 <- community_gud_7_2 |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_7_3" & subPlot == 26 & species == "Cer_fon", 1, value)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Gud_7_3" & species %in% c("Ave_fle", "Eup_wet"), 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_7_3" & subPlot %in% c(10, 12, 24, 26), 0, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_7_3" & subPlot == 16 & species == "Ast_alp", "J", value)) |> 
  mutate(species = ifelse(plotID == "Gud_7_3" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(year == 2018 & plotID == "Gud_7_3" & species == "Car_sp", "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Gud_7_3" & species == "Car_big", 2, cover)) |> 
  filter(!(year == 2021 & plotID == "Gud_7_3" & species == "Car_sp")) |> 
  mutate(species = ifelse(plotID == "Gud_7_3" & species == "Gen_sp", "Gen_niv", species), 
         value = ifelse(year == 2019 & plotID == "Gud_7_3" & subPlot == 28 & species == "Gen_niv", 1, value)) |> 
  filter(!(plotID == "Gud_7_3" & species %in% c("Nid_juvenile", "Nid_seedling")))

find_errors_unknowns(community_gud_7_3, "Gud_7_3")


# Gud_7_4
find_errors_unknowns(community_long_cover, "Gud_7_4")
# Species cover. Ran_acr is not present in the plot, I remove it
# Species cover. Car_vag 2019. Not recorded, we estimate it to be 1
check_vegetation_cover_height_moss_depth(community_long_cover, 2021, "Gud_7_4")
# Moss depth. No moss in the missing subplots, we change it to 0
check_vegetation_cover_height_moss_depth(community_long_cover, 2022, "Gud_7_4")
# Moss depth. No moss in the missing subplots, no moss in the missing subplot
turfplot(community_long_cover, "Gud_7_4")
# Car_fla_CF. It is not Car_fla, but we cannot really decide what it is. I remove it
# Car_sp is Car_big
# Unknown. No idea what this may be, I remove it

community_gud_7_4 <- community_gud_7_3 |> 
  filter(!(year == 2018 & plotID == "Gud_7_4" & species == "Ran_acr")) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_7_4" & species == "Car_vag", 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2021 & plotID == "Gud_7_4" & subPlot %in% c(10, 26), 0, moss_depth_mm)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Gud_7_4" & subPlot == 24, 0, moss_depth_mm)) |> 
  filter(!(plotID == "Gud_7_4" & species == "Car_fla_CF")) |> 
  mutate(species = ifelse(year == 2018 & plotID == "Gud_7_4" & species == "Car_sp", "Car_big", species)) |> 
  filter(!(plotID == "Gud_7_4" & species == "Unknown"))

find_errors_unknowns(community_gud_7_4, "Gud_7_4")


# Gud_7_6
find_errors_unknowns(community_long_cover, "Gud_7_6")
# Species value. Car_cap ? - We change it to 1, present other years in that subplot
# Species cover. Agr_cap and Ant_odo not registered. We look at cover from 2018 and 2021 and estimate
turfplot(community_long_cover, "Gud_7_6")
# value cf. Alc_sp is correct
# Fes_rub_cf_kanskje_Ave_fle. Based on checking the turfmapper in the field, we change this and Fes_rub to Ave_fle in 2018, 20109 and 2023
# Car_sp. We cannot really tell what they are. We remove them
# Epi_sp is Epi_ana
# Hie_sp. Cannot really tell, I keep it
# Leo_sp is Leo_aut

community_gud_7_6 <- community_gud_7_4 |> 
  mutate(value = ifelse(year == 2021 & plotID == "Gud_7_6" & subPlot == 19 & species == "Car_cap", 1, value)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_7_6" & species == "Agr_cap", 7, cover)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Gud_7_6" & species == "Ant_odo", 6, cover)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Gud_7_6" & subPlot == 28 & species == "Alc_sp", "j", value)) |> 
  mutate(species = ifelse(year %in% c(2018, 2019, 2023) & plotID == "Gud_7_6" & species %in% c("Fes_rub","Fes_rub_cf_kanskje_Ave_fle"), "Ave_fle", species)) |> 
  filter(!(plotID == "Gud_7_6" & species == "Car_sp")) |> 
  mutate(species = ifelse(plotID == "Gud_7_6" & species == "Epi_sp", "Epi_ana", species)) |> 
  mutate(species = ifelse(plotID == "Gud_7_6" & species == "Leo_sp", "Leo_aut", species)) |> 
  filter(!(plotID == "Gud_7_6" & species == "Nid_seedling"))

find_errors_unknowns(community_gud_7_6, "Gud_7_6")


# 10. We create an object and file only for gudmedalen

community_gudmedalen <- community_gud_7_6 |> arrange(year, plotID, subPlot)
community_gudmedalen |> filter(site == "Gudmedalen") |> write.csv("data_cleaned/Gudmedalen.csv")


## Lavisdalen----

# 11. We work on each plot in turn

community_gudmedalen |> filter(site == "Lavisdalen") |> select(plotID) |> distinct() |> print(n = Inf)
# Lav_1_1, Lav_1_2, Lav_1_3, Lav_1_4, Lav_1_6, Lav_2_2, Lav_2_3, Lav_2_4, Lav_2_5, Lav_2_6, Lav_3_1, Lav_3_3, Lav_3_4, Lav_3_5, Lav_3_6, Lav_4_1, Lav_4_3, Lav_4_4, Lav_4_5, Lav_5_2, Lav_5_3, Lav_5_5, Lav_5_6, Lav_6_2, Lav_6_3, Lav_6_5, Lav_6_6, Lav_7_1, Lav_7_2, Lav_7_3


# Lav_1_1
find_errors_unknowns(community_gudmedalen, "Lav_1_1")
# Species cover. Suc_pra 2021. It's cover has been written on Car_pal
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2022, "Lav_1_1")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_gudmedalen, "Lav_1_1")
# Value cf. Car_pil 2019 and Car_sp 2018 are in the same subplot as an unidentified Carex in 2023, which was guessed to be Car_nor. We use the same here
# Value cf. Cer_cer 2019 is Cer_cer
# Value cf. Fes_ovi 2019 is Fes_ovi
# Value cf. Poa_pra 2019 is Poa_pra
# Agr_cap_cf is Agr_cap
# Car_nor_cf is Car_big, after checking turfmapper in the field
# Unknown. I don't what it is, remove

community_lav_1_1 <- community_gudmedalen |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Lav_1_1" & species == "Car_pal", 1, cover)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Lav_1_1" & species == "Suc_pra", 7, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Lav_1_1" & subPlot == 24, 0, moss_depth_mm)) |> 
  mutate(value = ifelse(plotID == "Lav_1_1" & species == "Car_pil", 1, value)) |> 
  mutate(species = ifelse(plotID == "Lav_1_1" & species %in% c("Car_pil", "Car_sp"), "Car_nor", species)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_1_1" & subPlot == 22 & species == "Cer_cer", "f", value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_1_1" & subPlot == 32 & species == "Fes_ovi", 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_1_1" & subPlot == 21 & species == "Poa_pra", 1, value)) |> 
  mutate(species = ifelse(plotID == "Lav_1_1" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_1_1" & species == "Car_nor_cf", "Car_big", species), 
         cover = ifelse(year == 2019 & plotID == "Lav_1_1" & species == "Car_big", 4, cover)) |> 
  filter(!(plotID == "Lav_1_1" & species %in% c("Nid_seedling", "Unknown"))) |> 
  distinct()

find_errors_unknowns(community_lav_1_1, "Lav_1_1")


# Lav_1_2
find_errors_unknowns(community_gudmedalen, "Lav_1_2")
# Species cover. Cer_fon 2018. Not recorded, we estimate it to be 1
turfplot(community_gudmedalen, "Lav_1_2")
# Value cf. Epi_ana is correct
# Value cf. Fes_ovi is correct
# Value cf. Luz_spi is Luz_mul
# Value cf. Poa_pra is correct
# Agr_cap_cf is Agr_cap. No need to change cover
# Cer_cer_cf is Cer_cer

community_lav_1_2 <- community_lav_1_1 |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Lav_1_2" & species == "Cer_fon", 1, cover)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_1_2" & subPlot %in% c(18, 19) & species == "Epi_ana", "J", value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_1_2" & subPlot %in% c(15, 26) & species == "Fes_ovi", 1, value)) |> 
  mutate(species = ifelse(plotID == "Lav_1_2" & species == "Luz_spi", "Luz_mul", species), 
         value = ifelse(year == 2019 & plotID == "Lav_1_2" & subPlot == 1 & species == "Luz_mul", 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_1_2" & subPlot %in% c(1, 31) & species == "Poa_pra", 1, value)) |> 
  mutate(species = ifelse(plotID == "Lav_1_2" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_1_2" & species == "Cer_cer_cf", "Cer_cer", species), 
         value = ifelse(year == 2019 & plotID == "Lav_1_2" & subPlot %in% c(10, 18) & species == "Cer_cer", "F", value), 
         cover = ifelse(year == 2019 & plotID == "Lav_1_2" & species == "Cer_cer", 3, cover)) |> 
  distinct()

find_errors_unknowns(community_lav_1_2, "Lav_1_2")


# Lav_1_3
find_errors_unknowns(community_gudmedalen, "Lav_1_3")
# Species cover. Bis_viv 2022. Not recorded, we use 2021's cover
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_1_3")
# Vegetation height and moss depth were not typed in
turfplot(community_gudmedalen, "Lav_1_3")
# Value cf. Agr_cap 2019 is correct
# Value cf. Car_big is correct
# Agr_cap_cf is Agr_cap
# Car_no_cf is Car_nor
# Unknown is Vah_atr. This species was mistaken other years for other species, we correct it. We change cover in 2019 to 2 (Agr_mer to 1). No need to change in 2021 or 2022

community_lav_1_3 <- community_lav_1_2 |> 
  mutate(cover = ifelse(year == 2022 & plotID =="Lav_1_3" & species == "Bis_viv", 4, cover)) |> 
  mutate(vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_1_3" & subPlot == 10, 40, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_1_3" & subPlot == 12, 95, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_1_3" & subPlot == 24, 80, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_1_3" & subPlot == 26, 40, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_1_3" & subPlot == 10, 10, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_1_3" & subPlot == 12, 10, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_1_3" & subPlot == 24, 30, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_1_3" & subPlot == 26, 20, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_1_3" & subPlot == 10 & species == "Agr_cap", "f", value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_1_3" & subPlot %in% c(34, 35) & species == "Car_big", 1, value)) |> 
  mutate(species = ifelse(plotID == "Lav_1_3" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_1_3" & species == "Car_nor_cf", "Car_nor", species)) |> 
  mutate(species = ifelse(year == 2021 & plotID == "Lav_1_3" & species == "Unknown", "Vah_atr", species)) |> 
  mutate(species = ifelse(year == 2019 & plotID == "Lav_1_3" & subPlot %in% c(10, 16, 17, 18) & species == "Agr_mer", "Vah_atr", species), 
         cover = ifelse(year == 2019 & plotID =="Lav_1_3" & species == "Agr_mer", 1, cover), 
         cover = ifelse(year == 2019 & plotID =="Lav_1_3" & species == "Vah_atr", 2, cover)) |> 
  mutate(species = ifelse(year == 2022 & plotID == "Lav_1_3" & species == "Ran_arc", "Vah_atr", species)) |> 
  filter(!(plotID == "Lav_1_3" & species == "Nid_seedling"))

find_errors_unknowns(community_lav_1_3, "Lav_1_3")


# Lav_1_4
find_errors_unknowns(community_gudmedalen, "Lav_1_4")
# Species cover. Rum_ace 2022. Not typed in, it was 1
# Species cover. Fes_rub 2023. Not recorded, we estimate it to be 1
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_1_4")
# Vegetation cover not typed in. Vegetation height and moss depth were not recorded
turfplot(community_gudmedalen, "Lav_1_4")
# Value cf. 2019 Ant_odo is correct
# Value cf. 2019 Cer_alp. After looking at the turfmapper, we decided that all Cer_alp is in fact Cer_fon
# Value cf. Poa_pra is correct
# Agr_cap_cf is Agr_cap
# Cer_cer_cf is Cer_cer
# Eri_uni_cf is Eri_uni

community_lav_1_4 <- community_lav_1_3 |> 
  mutate(cover = ifelse(year == 2022 & plotID =="Lav_1_4" & species == "Rum_ace", 1, cover)) |> 
  mutate(cover = ifelse(year == 2023 & plotID =="Lav_1_4" & species == "Fes_rub", 1, cover)) |> 
  mutate(vegetation_cover = ifelse(year == 2021 & plotID == "Lav_1_4", 50, vegetation_cover)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_1_4" & subPlot %in% c(14, 26) & species == "Ant_odo", "j", value)) |> 
  mutate(species = ifelse(plotID == "Lav_1_4" & species == "Cer_alp", "Cer_fon", species), 
         value = ifelse(year == 2019 & plotID == "Lav_1_4" & subPlot == 21 &  species == "Cer_fon", 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_1_4" & subPlot == 21 &  species == "Poa_pra", 1, value)) |> 
  mutate(species = ifelse(plotID == "Lav_1_4" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_1_4" & species == "Cer_cer_cf", "Cer_cer", species)) |> 
  mutate(species = ifelse(plotID == "Lav_1_4" & species == "Eri_uni_cf", "Eri_uni", species)) |> 
  filter(!(plotID == "Lav_1_4" & species == "Nid_seedling"))


find_errors_unknowns(community_lav_1_4, "Lav_1_4")


# Lav_1_6
find_errors_unknowns(community_gudmedalen, "Lav_1_6")
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2022, "Lav_1_6")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_gudmedalen, "Lav_1_6")
# Value cf. Fes_ovi is correct
# Agr_cap_cf is Agr_cap
# Car_nor_cf is Car_nor
# Cer_cer_cf is Cer_cer
# Cer_alp is in fact Cer_fon

community_lav_1_6 <- community_lav_1_4 |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Lav_1_6" & subPlot == 10, 0, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_1_6" & subPlot == 2 & species == "Fes_ovi", 1, value)) |> 
  mutate(species = ifelse(plotID == "Lav_1_6" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_1_6" & species == "Car_nor_cf", "Car_nor", species)) |> 
  mutate(species = ifelse(plotID == "Lav_1_6" & species == "Cer_cer_cf", "Cer_cer", species)) |> 
  mutate(species = ifelse(plotID == "Lav_1_6" & species == "Cer_alp", "Cer_fon", species))

find_errors_unknowns(community_lav_1_6, "Lav_1_6")


# Lav_2_2
find_errors_unknowns(community_gudmedalen, "Lav_2_2")
# Subplot number. 2021 Vio_bif. They're shifted to the left. And 14 is actually 16
turfplot(community_gudmedalen, "Lav_2_2")
# Agr_cap_cf is Agr_cap
# Car_vag_CF is Car_vag
# Car_sp is Car_big in 2019
# Car_sp_den_lyse is Car_vag
# Car_sp in 2022 is Car_pil
# Sal_sp does not exist in the scan. The values are not from another species. I remove it
# We found some other mistakes in 2021: Ant_alp is actually Alc_alp, the values of Tar_sp belong to Bis_viv, Ver_alp is missing in subplot 1. Since all values for Tar_sp are 1, we can create a tibble from the original one, choosing the correct subplots and changing the species

lav_2_2_2021_tar_sp <- community_gudmedalen |> 
  filter(year == 2021 & plotID == "Lav_2_2" & subPlot %in% c(8, 10, 19, 20, 26, 32, 33)) |> 
  mutate(species = "Tar_sp", 
         value = "1", 
         cover = "5") |> 
  distinct()

lav_2_2_2021_ver_alp <- community_gudmedalen |> 
  filter(year == 2021 & plotID == "Lav_2_2" & subPlot == 1) |> 
  mutate(species = "Ver_alp", 
         value = "1", 
         cover = "1") |> 
  distinct()

community_lav_2_2 <- community_lav_1_6 |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 9, 10, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 10, 40, moss), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 10, 20, litter), 
         logger = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 10, 40, logger), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 10, 60, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 10, 20, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 11, 12, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 12, 85, moss), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 12, 20, litter), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 12, 80, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 12, 30, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 14 & species == "Vio_bif", 16, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 16, 25, moss), 
         lichen = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 16, NA, lichen), 
         logger = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot == 16, 40, logger)) |> 
  mutate(species = ifelse(plotID == "Lav_2_2" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_2_2" & species == "Car_vag_CF", "Car_vag", species), 
         value = ifelse(year == 2021 & plotID == "Lav_2_2" & subPlot %in% c(8, 10) & species == "Car_vag", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Lav_2_2" & species == "Car_vag", 4, cover)) |> 
  mutate(species = ifelse(year == 2019 & plotID == "Lav_2_2" & species == "Car_sp_den_lyse", "Car_vag", species)) |> 
  mutate(species = ifelse(year == 2019 & plotID == "Lav_2_2" & species == "Car_sp", "Car_big", species), 
         cover = ifelse(year == 2019 & plotID == "Lav_2_2" & species == "Car_big", 6, cover)) |> 
  mutate(species = ifelse(year == 2022 & plotID == "Lav_2_2" & species == "Car_sp", "Car_pil", species)) |> 
  filter(!(plotID == "Lav_2_2" & species == "Sal_sp")) |> 
  mutate(species = ifelse(plotID == "Lav_2_2" & species == "Ant_alp", "Alc_alp", species)) |>
  filter(!(year == 2021 & plotID == "Lav_2_2" & species == "Tar_sp")) |>
  bind_rows(lav_2_2_2021_tar_sp) |> 
  bind_rows(lav_2_2_2021_ver_alp) |> 
  filter(!(plotID == "Lav_2_2" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_lav_2_2, "Lav_2_2")


# Lav_2_3
find_errors_unknowns(community_gudmedalen, "Lav_2_3")
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_2_3")
# Vegetation height and moss depth written in the wrong columns
turfplot(community_gudmedalen, "Lav_2_3")
# Value cf. 2019 Alc_alp is correct
# Value cf. 2019 Ave_fle is correct
# Value cf. 2019 Poa_pra is correct
# Value cf. 2019 Ver_alp is correct
# Agr_cap_cf is Agr_cap. And Agr_mer as well
# Car_nor_cf is Car_pal
# Ran_sp is Ran_pyg. Ran_acr as well
# Unknown. Not enough information to decide

community_lav_2_3 <- community_lav_2_2 |> 
  mutate(vegetation_cover = ifelse(year == 2021 & plotID == "Lav_2_3", 80, vegetation_cover), 
         vegetation_height_mm = ifelse(year == 2021 & subPlot == 10 & plotID == "Lav_2_3", 80, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & subPlot == 12 & plotID == "Lav_2_3", 75, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & subPlot == 24 & plotID == "Lav_2_3", 80, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & subPlot == 26 & plotID == "Lav_2_3", 70, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & subPlot == 10 & plotID == "Lav_2_3", 20, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & subPlot == 12 & plotID == "Lav_2_3", 20, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & subPlot == 24 & plotID == "Lav_2_3", 45, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & subPlot == 26 & plotID == "Lav_2_3", 25, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_2_3" & subPlot == 17 & species == "Alc_alp", "j", value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_2_3" & subPlot == 30 & species == "Ave_fle", "j", value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_2_3" & subPlot %in% c(10, 31) & species == "Poa_pra", 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_2_3" & subPlot == 1 & species == "Ver_alp", "s", value)) |> 
  mutate(species = ifelse(plotID == "Lav_2_3" & species %in% c("Agr_cap_cf", "Agr_mer"), "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_2_3" & species == "Car_nor_cf", "Car_pal", species)) |> 
  mutate(species = ifelse(plotID == "Lav_2_3" & species %in% c("Ran_sp", "Ran_acr"), "Ran_pyg", species)) |> 
  filter(!(plotID == "Lav_2_3" & species %in% c("Nid_seedling", "Unknown")))

find_errors_unknowns(community_lav_2_3, "Lav_2_3")


# Lav_2_4
find_errors_unknowns(community_gudmedalen, "Lav_2_4")
# Species cover. 2023 Phl_alp was not recorded, we estimate it to be 1
turfplot(community_gudmedalen, "Lav_2_4")
# Value cf, 2019 Poa_pra is correct
# Value cf, 2019 Ver_alp is correct. We correct cover to 3
# Ran_sp is Ran_acr

community_lav_2_4 <- community_lav_2_3 |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Lav_2_4" & species == "Phl_alp", 1, cover)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_2_4" & species == "Poa_pra", 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_2_4" & subPlot %in% c(18, 22) & species == "Ver_alp", 1, value), 
         cover = ifelse(year == 2019 & plotID == "Lav_2_4" & species == "Ver_alp", 3, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_2_4" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_2_4" & species == "Car_nor_cf", "Car_nor", species)) |> 
  mutate(species = ifelse(plotID == "Lav_2_4" & species == "Epi_ana_cf", "Epi_ana", species)) |> 
  mutate(species = ifelse(plotID == "Lav_2_4" & species == "Ran_sp", "Ran_acr", species))

find_errors_unknowns(community_lav_2_4, "Lav_2_4")


# Lav_2_5
find_errors_unknowns(community_gudmedalen, "Lav_2_5")
# Species value. Moss, lichen and litter were typed in one column to the left
check_species_cover(community_gudmedalen, "Lav_2_5", "Vio_bif")
# Species cover. 2021 Vio_bif not recorded, we make the average of 2019 and 2023
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_2_5")
# Vegetation height and moss depth measured in subplot 17, since logger in 10
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2023, "Lav_2_5")
# Vegetation height and moss depth not recorded in 2023
# Subplot number. They're shifted to the left, and I delete 13
turfplot(community_gudmedalen, "Lav_2_5")
# Value cf. Car_nor is correct
# Value cf. Epi_ana is correct
# Value cf. Poa_alp is correct
# Value cf. Ver_alp is correct
# Agr_cap_cf is Agr_cap
# By looking at other plots in other years in Lavisdalen, we think Tri_fol is Tri_pra
# From turfmapper check in the field in 2023, we think the Vac_myr in 2021 could have been small Pyrola

community_lav_2_5 <- community_lav_2_4 |> 
  mutate(moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 1, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 2, 5, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 3, 5, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 4, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 5, 20, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 6, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 7, 15, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 8, 5, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 10, 2, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 12, 15, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 14, 15, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 15, 5, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 16, 2, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 17, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 18, 5, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 19, 15, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 20, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 21, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 22, 5, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 24, 1, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 26, 15, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 28, 5, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 29, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 30, 5, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 31, 5, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 32, 2, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 33, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 34, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 35, 20, moss), 
         lichen = ifelse(year == 2021 & plotID == "Lav_2_5", NA, lichen), 
         lichen = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 1, 10, lichen), 
         lichen = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 21, 15, lichen), 
         lichen = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 24, 10, lichen), 
         lichen = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 28, 20, lichen), 
         lichen = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 31, 20, lichen), 
         lichen = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 32, 2, lichen), 
         lichen = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 34, 2, lichen), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 1, 5, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 2, 10, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 3, 10, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 4, 50, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 5, 30, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 6, 15, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 7, 10, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 8, 15, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 10, 0, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 12, 15, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 14, 35, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 15, 5, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 16, 5, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 17, 5, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 18, 10, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 19, 15, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 20, 50, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 21, 50, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 22, 2, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 24, 40, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 26, 30, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 28, 40, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 29, 50, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 30, 10, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 31, 30, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 32, 15, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 33, 15, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 34, 25, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 35, 15, litter)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Lav_2_5" & species == "Vio_bif", 5, cover)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 9, 10, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 10, 2, moss), 
         lichen = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 10, NA, lichen), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 10, 0, litter), 
         logger = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 10, 70, logger), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 10, 160, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 10, 20, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 11, 12, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 12, 15, moss), 
         lichen = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 12, NA, lichen), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 12, 15, litter), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 12, 70, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_2_5" & subPlot == 12, 30, moss_depth_mm)) |> 
  filter(!(year == 2021 & plotID == "Lav_2_5" & subPlot == 13)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_2_5" & subPlot == 16 & species == "Car_nor", 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_2_5" & subPlot == 19 & species == "Epi_ana", 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_2_5" & subPlot == 10 & species == "Poa_alp", 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_2_5" & subPlot %in% c(24, 31) & species == "Ver_alp", "j", value)) |> 
  mutate(species = ifelse(year == 2022 & plotID == "Lav_2_5" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_2_5" & species == "Tri_fol", "Tri_pra", species)) |> 
  mutate(species = ifelse(plotID == "Lav_2_5" & species == "Vac_myr", "Pyr_sp", species), 
         cover = ifelse(year == 2021 & plotID == "Lav_2_5" & species == "Pyr_sp", 21, cover)) |> 
  filter(!(plotID == "Lav_2_5" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_lav_2_5, "Lav_2_5")


# Lav_2_6
find_errors_unknowns(community_gudmedalen, "Lav_2_6")
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2022, "Lav_2_6")
# No moss in the missing subplots, we change it to 0
# Subplot number. 2021 Cer_cer 9 is actually 10. And 3 and 4 are also shifted (they're 4 and 5)
turfplot(community_gudmedalen, "Lav_2_6")
# Value cf. Ran_acr is correct
# Oma_sp is Oma_sup
# Vac_myr is wrong, it was probably small shoots of Sal_her. We correct the cover (it was wrong) and adjust it to 32

community_lav_2_6 <- community_lav_2_5 |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Lav_2_6" & subPlot %in% c(10, 12), 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Lav_2_6" & subPlot == 3 & species == "Cer_cer", 5, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Lav_2_6" & subPlot == 5, 5, moss), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_6" & subPlot == 5, 15, litter)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Lav_2_6" & subPlot == 9, 10, subPlot), 
         litter = ifelse(year == 2021 & plotID == "Lav_2_6" & subPlot == 10, 5, litter), 
         logger = ifelse(year == 2021 & plotID == "Lav_2_6" & subPlot == 10, 50, logger), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_2_6" & subPlot == 10, 160, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_2_6" & subPlot == 10, 25, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_2_6" & subPlot == 1 & species == "Ran_acr", "j", value)) |> 
  mutate(species = ifelse(species == "Oma_sp" & plotID == "Lav_2_6", "Oma_sup", species)) |> 
  mutate(species = ifelse(plotID == "Lav_2_6" & species == "Vac_myr", "Sal_her", species)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Lav_2_6" & species == "Sal_her", 32, cover )) |> 
  filter(!(plotID == "Lav_2_6" & year == 2021 & subPlot == 29 & species == "Sal_her" & value == 1)) |> 
  filter(!(plotID == "Lav_2_6" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_lav_2_6, "Lav_2_6")


# Lav_3_1
find_errors_unknowns(community_gudmedalen, "Lav_3_1")
turfplot(community_gudmedalen, "Lav_3_1")
# Value cf. Agr_mer is correct
# Value cf. Cer_cer is correct
# Value cf. Epi_ana is correct
# Epi_ana_cf is Epi_ana
# Fes_rub_cf_kanskje_Ave_fle is Ave_fle
# Ran_acr_cf is Epi_ana
# Agr_mer in subplots 3 and 4 is actually Agr_cap. And Agr_cap is missing in 2019, we add it from 2018

lav_3_1_2019_agr_cap <- fix_species_before(community_gudmedalen, 2018, "Lav_3_1", "Agr_cap")

community_lav_3_1 <- community_lav_2_6 |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_3_1" & subPlot %in% c(5, 12, 31) & species == "Agr_mer", "j", value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_3_1" & subPlot ==35 & species == "Cer_cer", "j", value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_3_1" & subPlot ==35 & species == "Epi_ana", "j", value)) |> 
  mutate(species = ifelse(year == 2021 & plotID == "Lav_3_1" & species == "Epi_ana_cf", "Epi_ana", species)) |> 
  mutate(species = ifelse(year == 2021 & plotID == "Lav_3_1" & species == "Fes_rub_cf_kanskje_Ave_fle", "Ave_fle", species)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Lav_3_1" & species == "Ave_fle", 2, cover)) |> 
  mutate(species = ifelse(year == 2021 & plotID == "Lav_3_1" & species == "Ran_acr_cf", "Ran_acr", species)) |> 
  mutate(species = ifelse(plotID == "Lav_3_1" & species == "Car_sp", "Car_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_3_1" & species == "Agr_mer" & subPlot %in% c(3, 4), "Agr_cap", species)) |> 
  bind_rows(lav_3_1_2019_agr_cap) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Lav_3_1" & species == "Agr_cap", 2, cover)) |> 
  filter(!(plotID == "Lav_3_1" & species == "Nid_seedling"))

find_errors_unknowns(community_lav_3_1, "Lav_3_1")


# Lav_3_3
find_errors_unknowns(community_gudmedalen, "Lav_3_3")
# Species cover. Oxy_dig not recorded, we set it at 2
# Species cover. Sal_sp is not in the scan, and its values do not belong to another species, we remove it
turfplot(community_gudmedalen, "Lav_3_3")
# Value cf. Epi_ana is correct
# Value cf. Phl_alp is correct
# Value cf. Tar_sp is correct
# Value cf. Ver_alp is correct
# Car_nor_cf, Car_nor and Car_sp are Car_big
# Poaceae_sp. By comparing to previous years, and from the comments in the scan, it seems it is Poa_alp
# Unknown in 2021 was Suc_pra (the scan said Suc_vul). No idea what it was in 2019
# Agr_cap in 2021 was also present in subplot 10, we add it
# Luz_spi is Luz_mul

lav_3_3_2021_agr_cap <- community_gudmedalen |>
  filter(year == 2021 & plotID == "Lav_3_3" & subPlot == 10) |> 
  mutate(species = "Agr_cap", value = "1", cover = "2") |> 
  distinct()

community_lav_3_3 <- community_lav_3_1 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Lav_3_3" & species == "Oxy_dig", 2, cover)) |>
  filter(!(plotID == "Lav_3_3" & species == "Sal_sp")) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_3_3" & subPlot == 7 & species == "Epi_ana", "j", value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_3_3" & subPlot == 14 & species == "Phl_alp", 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_3_3" & subPlot == 4 & species == "Tar_sp", "j", value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_3_3" & subPlot == 21 & species == "Ver_alp", "j", value)) |> 
  mutate(species = ifelse(plotID == "Lav_3_3" & species == "Car_nor_cf", "Car_big", species), 
         species = ifelse(plotID == "Lav_3_3" & species == "Car_nor", "Car_big", species), 
         species = ifelse(plotID == "Lav_3_3" & species == "Car_sp", "Car_big", species), 
         cover = ifelse(year == 2019 & plotID == "Lav_3_3" & species == "Car_big", 3, cover), 
         cover = ifelse(year == 2021 & plotID == "Lav_3_3" & species == "Car_big", 3, cover), 
         cover = ifelse(year == 2023 & plotID == "Lav_3_3" & species == "Car_big", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_3_3" & species == "Poaceae_sp", "Poa_alp", species), 
         value = ifelse(year == 2023 & plotID == "Lav_3_3" & subPlot %in% c(22, 29) & species == "Poa_alp", 1, value), 
         cover = ifelse(year == 2019 & plotID == "Lav_3_3" & species == "Poa_alp", 1, cover)) |> 
  filter(!(year == 2019 & plotID == "Lav_3_3" & species == "Unknown")) |> 
  mutate(species = ifelse(year == 2021 & plotID == "Lav_3_3" & species == "Unknown", "Suc_pra", species)) |> 
  bind_rows(lav_3_3_2021_agr_cap) |> 
  mutate(species = ifelse(plotID == "Lav_3_3" & species == "Luz_spi", "Luz_mul", species)) |> 
  filter(!(plotID == "Lav_3_3" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_lav_3_3, "Lav_3_3")


# Lav_3_4
find_errors_unknowns(community_gudmedalen, "Lav_3_4")
turfplot(community_gudmedalen, "Lav_3_4")
# Value cf. Car_vag, as well as Car_big, are Car_big is correct
# Agr_mer was actually Agr_cap (confirmed with turfmapper in the field in 2023)

community_lav_3_4 <- community_lav_3_3 |> 
  mutate(species = ifelse(plotID == "Lav_3_4" & species %in% c("Car_sp", "Car_vag"), "Car_big", species ), 
         value = ifelse(year == 2019 & plotID == "Lav_3_4" & subPlot == 26 & species == "Car_big", "1j", value), 
         value = ifelse(year == 2019 & plotID == "Lav_3_4" & subPlot == 29 & species == "Car_big", 1, value), 
         cover = ifelse(year == 2019 & plotID == "Lav_3_4" & species == "Car_big", 6, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_3_4" & species == "Agr_mer", "Agr_cap", species), 
         cover = ifelse(year == 2018 & plotID == "Lav_3_4" & species == "Agr_cap", 2, cover)) |> 
  filter(!(plotID == "Lav_3_4" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_lav_3_4, "Lav_3_4")


# Lav_3_5
find_errors_unknowns(community_gudmedalen, "Lav_3_5")
# Species cover. 2019 Rum_ace not registered, but only in one subplot. We estimate it to be 1
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_3_5")
# Vegetation height and moss depth recorded in cm, not mm
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2023, "Lav_3_5")
# Vegetation height and moss depth. Logger in the missing subplots
turfplot(community_gudmedalen, "Lav_3_5")
# Value cf. Car_vag, as well as Car_big, are Car_big is correct
# Fes_rub_cf_kanskje_Ave_fle was Ave_fle

community_lav_3_5 <- community_lav_3_4 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Lav_3_5" & species == "Rum_ace", 1, cover)) |> 
  mutate(vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_3_5", vegetation_height_mm * 10, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_3_5", moss_depth_mm * 10, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Lav_3_5" & species == "Fes_rub_cf_kanskje_Ave_fle", "Ave_fle", species), 
         cover = ifelse(year == 2019 & plotID == "Lav_3_5" & species == "Ave_fle", 7, cover))

find_errors_unknowns(community_lav_3_5, "Lav_3_5")


# Lav_3_6
find_errors_unknowns(community_gudmedalen, "Lav_3_6")
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_3_6")
# Moss depth. Moss absent in the missing subplot, we change it to 0
turfplot(community_gudmedalen, "Lav_3_6")
# Value cf. Car_vag, as well as Car_big, are Car_big is correct
# Fes_rub_cf_kanskje_Ave_fle was Ave_fle

community_lav_3_6 <- community_lav_3_5 |> 
  mutate(moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_3_6" & subPlot == 10, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Lav_3_6" & species == "Car_nor_cf", "Car_nor", species)) |> 
  mutate(species = ifelse(plotID == "Lav_3_6" & species == "Car_sp", "Car_big", species), 
         cover = ifelse(year == 2019 & plotID == "Lav_3_6" & species == "Car_big", 5, cover)) |> 
  distinct()

find_errors_unknowns(community_lav_3_6, "Lav_3_6")


# Lav_4_1
find_errors_unknowns(community_gudmedalen, "Lav_4_1")
# Species value. 2022 Sal_sp is cover. And looking at turfmapper we see it is Sal_lan
# Species value. 2021 Sib_pro 21 is J, not JJ (found afterwards, not with this function)
turfplot(community_gudmedalen, "Lav_4_1")
# Value cf. Car_nor_cf is Car_nor
# Agr_cap_cf is Agr_cap
# Car_vag_CF is Car_vag
# Gen_cam_cf is Gen_cam
# Leo_aut_cf is Leo_aut
# Des_alp and DeS_ces are both present in the plot. We need to make sure they are correct

community_lav_4_1 <- community_lav_3_6 |> 
  filter(!(year == 2022 & plotID == "Lav_4_1" & subPlot == 26 & species == "Sal_sp")) |> 
  mutate(species = ifelse(plotID == "Lav_4_1" & species == "Sal_sp", "Sal_lan", species), 
         cover = ifelse(year == 2022 & plotID == "Lav_4_1" & species == "Sal_lan", 5, cover)) |> 
  mutate(value = ifelse(year == 2021 & plotID == "Lav_4_1" & subPlot == 21 & species == "Lav_4_1", "J", value)) |> 
  mutate(species = ifelse(plotID == "Lav_4_1" & species == "Car_nor_cf", "Car_nor", species), 
         value = ifelse(year == 2021 & plotID == "Lav_4_1" & subPlot %in% c(4, 5, 29, 32) & species == "Car_nor", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Lav_4_1" & species == "Car_nor", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_4_1" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_4_1" & species == "Car_vag_CF", "Car_vag", species)) |> 
  mutate(species = ifelse(plotID == "Lav_4_1" & species == "Gen_cam_cf", "Gen_cam", species)) |> 
  mutate(species = ifelse(plotID == "Lav_4_1" & species == "Leo_aut_cf", "Leo_aut", species)) |> 
  mutate(species = ifelse(plotID == "Lav_4_1" & subPlot %in% c(8, 10, 12, 14, 15, 16, 17, 18, 19, 20, 21) & species == "Des_ces", "Des_alp", species), 
         cover = ifelse(year == 2018 & plotID == "Lav_4_1" & species == "Des_alp", 1, cover), 
         cover = ifelse(year == 2019 & plotID == "Lav_4_1" & species == "Des_alp", 2, cover), 
         cover = ifelse(year == 2021 & plotID == "Lav_4_1" & species == "Des_alp", 1, cover), 
         cover = ifelse(year == 2022 & plotID == "Lav_4_1" & species == "Des_alp", 1, cover), 
         cover = ifelse(year == 2023 & plotID == "Lav_4_1" & species == "Des_alp", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_4_1" & subPlot %in% c(22, 24, 26, 28, 29, 30, 31, 32, 33, 34, 35) & species == "Des_alp", "Des_ces", species), 
         cover = ifelse(year == 2018 & plotID == "Lav_4_1" & species == "Des_ces", 1, cover), 
         cover = ifelse(year == 2019 & plotID == "Lav_4_1" & species == "Des_ces", 4, cover), 
         cover = ifelse(year == 2021 & plotID == "Lav_4_1" & species == "Des_ces", 2, cover), 
         cover = ifelse(year == 2022 & plotID == "Lav_4_1" & species == "Des_ces", 1, cover), 
         cover = ifelse(year == 2023 & plotID == "Lav_4_1" & species == "Des_ces", 1, cover)) |> 
  filter(!(plotID == "Lav_4_1" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_lav_4_1, "Lav_4_1")


# Lav_4_3
find_errors_unknowns(community_gudmedalen, "Lav_4_3")
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_4_3")
# Vegetation height and moss depth recorded in cm, not mm
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2022, "Lav_4_3")
# Moss depth. Moss absent in the missing subplot, we change it to 0
turfplot(community_gudmedalen, "Lav_4_3")
# Car_nor_cf is Car_nor
# Car_sp. Not enough info to decide, we remove it
# Cer_alp is actually Cer_cer

community_lav_4_3 <- community_lav_4_1 |> 
  mutate(vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_4_3", vegetation_height_mm * 10, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_4_3", moss_depth_mm * 10, moss_depth_mm)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Lav_4_3" & subPlot == 26, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Lav_4_3" & species == "Car_nor_cf", "Car_nor", species)) |> 
  filter(!(plotID == "Lav_4_3" & species == "Car_sp")) |> 
  mutate(species = ifelse(plotID == "Lav_4_3" & species == "Cer_alp", "Cer_cer", species)) |> 
  filter(!(plotID == "Lav_4_3" & species == "Nid_seedling"))

find_errors_unknowns(community_lav_4_3, "Lav_4_3")


# Lav_4_4
find_errors_unknowns(community_gudmedalen, "Lav_4_4")
# Species cover. Sag_sag not typed in
# Species cover. Ver_pal not typed in
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_4_4")
# Moss depth. Moss absent in the missing subplot, we change it to 0
turfplot(community_gudmedalen, "Lav_4_4")
# Car_cap_cf is Car_cap
# Car_nor_cf is Car_nor
# Car_vag is Car_nor in 2021 and Car_big in 2022

community_lav_4_4 <- community_lav_4_3 |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Lav_4_4" & species == "Sag_sag", 1, cover)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Lav_4_4" & species == "Vio_pal", 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year %in% c(2021, 2022) & plotID == "Lav_4_4" & subPlot == 26, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Lav_4_4" &species == "Car_cap_cf", "Car_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_4_4" &species == "Car_nor_cf", "Car_nor", species)) |> 
  mutate(species = ifelse(year == 2022 & plotID == "Lav_4_4" & species == "Car_vag", "Car_nor", species)) |> 
  mutate(species = ifelse(year == 2022 & plotID == "Lav_4_4" & species == "Car_vag", "Car_big", species)) |> 
  filter(!(plotID == "Lav_4_4" & species == "Nid_seedling"))

find_errors_unknowns(community_lav_4_4, "Lav_4_4")


# Lav_4_5
find_errors_unknowns(community_gudmedalen, "Lav_4_5")
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_4_5")
# Vegetation height and moss depth. Logger in subplot 26, so measurements taken in 33
turfplot(community_gudmedalen, "Lav_4_5")
# In this plot there is a bit of chaos with the Carexes. Since we can't distinguish them consistently throughout the years we call all Car_big
# Fes_ovi is Fes_rub

community_lav_4_5 <- community_lav_4_4 |> 
  mutate(species = ifelse(plotID == "Lav_4_5" & species == "Car_big_cf", "Car_big", species), 
         species = ifelse(plotID == "Lav_4_5" & species == "Car_cap_cf", "Car_big", species), 
         species = ifelse(plotID == "Lav_4_5" & species == "Car_cap", "Car_big", species), 
         species = ifelse(plotID == "Lav_4_5" & species == "Car_nor", "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Lav_4_5" & species == "Car_big", 3, cover), 
         cover = ifelse(year == 2019 & plotID == "Lav_4_5" & species == "Car_big", 3, cover), 
         cover = ifelse(year == 2021 & plotID == "Lav_4_5" & species == "Car_big", 3, cover), 
         cover = ifelse(year == 2023 & plotID == "Lav_4_5" & species == "Car_big", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_4_5" & species == "Fes_ovi", "Fes_rub", species)) |> 
  filter(!(plotID == "Lav_4_5" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_lav_4_5, "Lav_4_5")


# Lav_5_2
find_errors_unknowns(community_gudmedalen, "Lav_5_2")
# Species cover. 2023 Alc_alp was not recorded
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_5_2")
# Vegetation height and moss depth recorded in cm, not mm
turfplot(community_gudmedalen, "Lav_5_2")
# Value cf. Car_big_cf was typed in also as Car_big, so we remove the former
# Agr_cap_cf is Agr_cap
# Car_nor_cf is Car_nor
# Car_pil_cf is Car_big

community_lav_5_2 <- community_lav_4_5 |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Lav_5_2" & species == "Alc_alp", 1, cover)) |> 
  mutate(vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_5_2", vegetation_height_mm * 10, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_5_2", moss_depth_mm * 10, moss_depth_mm)) |> 
  filter(!(plotID == "Lav_5_2" & species == "Car_big_cf")) |> 
  mutate(species = ifelse(plotID == "Lav_5_2" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Lav_5_2" & species == "Car_nor_cf", "Car_nor", species)) |> 
  mutate(species = ifelse(plotID == "Lav_5_2" & species == "Car_pil_cf", "Car_big", species)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Lav_5_2" & species == "Car_big", 10, cover))

find_errors_unknowns(community_lav_5_2, "Lav_5_2")


# Lav_5_3
find_errors_unknowns(community_gudmedalen, "Lav_5_3")
# Species value. 2019 Rum_ace cover typed in subplot 35
# Species value. 2022 Car_nor was unsure, but it is correct. We find out that has been typed in subplot 10, but it wasn't recorded there
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_5_3")
# Vegetation cover, vegetation height and moss depth not typed in in 2021. We find out that cover of non-vascular material wasn't typed in either
# Subplot number. 2021 Agr_cap 13 is wrong, as well as 12. And we are missing 14, 16 and 17
turfplot(community_gudmedalen, "Lav_5_3")
# Value cf. Car_nig and Car_nor_cf are Car_nor
# Car_pil_cf is Car_pil
# Epi_sp is a typo, it was Eup_sp, which is Eup_wet
# Vio_pal is Vio_can

lav_5_3_2021_agr_cap <- community_gudmedalen |>
  filter(year == 2021 & plotID == "Lav_5_3" & subPlot %in% c(14, 16, 17)) |> 
  mutate(species = "Agr_cap", value = "1", cover = "1") |> 
  distinct()

community_lav_5_3 <- community_lav_5_2 |> 
  mutate(value = ifelse(year == 2019 & plotID == "Lav_5_3" & subPlot == 35 & species == "Rum_ace", 1, value), 
         cover = ifelse(year == 2019 & plotID == "Lav_5_3" & species == "Rum_ace", 3, cover)) |> 
  mutate(value = ifelse(year == 2022 & plotID == "Lav_5_3" & subPlot == 26 & species == "Car_nor", 1, value)) |> 
  filter(!(year == 2022 & plotID == "Lav_5_3" & subPlot == 10 & species == "Car_nor")) |> 
  filter(!(year == 2021 & plotID == "Lav_5_3" & subPlot %in% c(12, 13) & species == "Agr_cap")) |> 
  bind_rows(lav_5_3_2021_agr_cap) |> 
  mutate(vegetation_cover = ifelse(year == 2021 & plotID == "Lav_5_3", 85, vegetation_cover), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 10, 50, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 12, 100, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 24, 70, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 26, 70, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 10, 20, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 12, 25, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 24, 30, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 26, 30, moss_depth_mm), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 1, 30, moss), moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 2, 25, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 3, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 4, 5, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 5, 30, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 6, 30, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 7, 20, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 8, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 10, 20, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 12, 15, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 14, 30, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 15, 15, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 16, 25, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 17, 25, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 18, 25, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 19, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 20, 35, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 21, 40, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 22, 20, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 24, 20, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 26, 15, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 28, 30, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 29, 30, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 30, 15, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 31, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 32, 10, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 33, 25, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 34, 15, moss), 
         moss = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 35, 5, moss), 
         lichen = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 2, 10, lichen), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 1, 15, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 2, 25, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 3, 30, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 4, 25, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 5, 30, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 6, 15, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 7, 20, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 8, 25, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 10, 25, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 12, 30, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 14, 20, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 15, 30, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 16, 30, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 17, 15, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 18, 20, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 19, 20, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 20, 15, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 21, 25, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 22, 25, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 24, 25, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 26, 10, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 28, 15, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 29, 25, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 30, 40, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 31, 30, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 32, 80, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 33, 30, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 34, 50, litter), 
         litter = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 35, 50, litter), 
         logger = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 19, 55, logger), 
         logger = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 20, 30, logger), 
         logger = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot == 26, 70, logger)) |> 
  mutate(species = ifelse(plotID == "Lav_5_3" & species %in% c("Car_nig", "Car_nor_cf"), "Car_nor", species), 
         value = ifelse(year == 2021 & plotID == "Lav_5_3" & subPlot %in% c(3, 4, 15, 22, 26, 29, 30) & species == "Car_nor", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Lav_5_3" & species == "Car_nor", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_5_3" & species == "Car_pil_cf", "Car_pil", species)) |> 
  mutate(species = ifelse(plotID == "Lav_5_3" & species == "Epi_sp", "Eup_wet", species)) |> 
  mutate(species = ifelse(plotID == "Lav_5_3" & species == "Vio_pal", "Vio_can", species)) |> 
  filter(!(plotID == "Lav_5_3" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_lav_5_3, "Lav_5_3")


# Lav_5_5
find_errors_unknowns(community_gudmedalen, "Lav_5_5")
# Species cover. 2023 Rum_ace not recorded, looking at other years we estimate it to be 1
turfplot(community_gudmedalen, "Lav_5_5")
# Car_big_cf is Car_big
# Car_nor_cf and Car_nor are Car_vag
# Hie_sp and Hie_alp are Hie_pil
# Cer_fon is Cer_alp
# Poa_alp is Poa_pra
# Unknown. Not enough information

community_lav_5_5 <- community_lav_5_3 |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Lav_5_5" & species == "Rum_ace", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_5_5" & species == "Car_big_cf", "Car_big", species), 
         value = ifelse(year == 2021 & plotID == "Lav_5_5" & subPlot == 26 & species == "Car_big", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Lav_5_5" & species == "Car_big", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_5_5" & species %in% c("Car_nor", "Car_nor_cf"), "Car_vag", species), 
         cover = ifelse(year == 2019 & plotID == "Lav_5_5"& species == "Car_vag", 4, cover), 
         value = ifelse(year == 2021 & plotID == "Lav_5_5"& subPlot %in% c(34, 35) & species == "Car_vag", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Lav_5_5"& species == "Car_vag", 4, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_5_5" & species %in% c("Hie_alp", "Hie_sp"), "Hie_pil", species)) |> 
  mutate(species = ifelse(plotID == "Lav_5_5" & species == "Cer_fon", "Cer_alp", species)) |> 
  mutate(species = ifelse(plotID == "Lav_5_5" & species == "Poa_alp", "Poa_pra", species)) |> 
  filter(!(plotID == "Lav_5_5" & species == "Unknown")) |> 
  distinct()

find_errors_unknowns(community_lav_5_5, "Lav_5_5")


# Lav_5_6
find_errors_unknowns(community_gudmedalen, "Lav_5_6")
# Species cover. 2023 Sil_aca not recorded, looking at other years we estimate it to be 1
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2022, "Lav_5_6")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_gudmedalen, "Lav_5_6")
# Agr_cap_cf and Agr_mer are Agr_cap
# Car_nor_cf, Car_nor and Car_big are Car_vag
# Sal_sp is Sal_lan
# Fern. We keep it
# Cer_alp is Cer_fon

community_lav_5_6 <- community_lav_5_5 |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Lav_5_6" & species == "Sil_aca", 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Lav_5_6" & subPlot == 26, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Lav_5_6" & species %in% c("Agr_mer", "Agr_cap_cf"), "Agr_cap", species), 
         cover = ifelse(year == 2021 & plotID == "Lav_5_6" & species == "Agr_cap", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_5_6" & species %in% c("Car_nor", "Car_nor_cf", "Car_big"), "Car_vag", species), 
         cover = ifelse(year == 2018 & plotID == "Lav_5_6" & species == "Car_vag", 3, cover), 
         value = ifelse(year == 2019 & plotID == "Lav_5_6" & subPlot == 2 & species == "Car_vag", "f", value), 
         cover = ifelse(year == 2019 & plotID == "Lav_5_6" & species == "Car_vag", 5, cover), 
         cover = ifelse(year == 2021 & plotID == "Lav_5_6" & species == "Car_vag", 5, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_5_6" & species == "Sal_sp", "Sal_lan", species)) |> 
  mutate(species = ifelse(plotID == "Lav_5_6" & species == "Cer_alp", "Cer_fon", species)) |> 
  distinct()

find_errors_unknowns(community_lav_5_6, "Lav_5_6")


# Lav_6_2
find_errors_unknowns(community_gudmedalen, "Lav_6_2")
turfplot(community_gudmedalen, "Lav_6_2")
# Alc_sp_cf is Alc_sp
# Car_sp and Car_cap are Car_nor
# Agr_cap is Agr_mer

community_lav_6_2 <- community_lav_5_6 |> 
  mutate(species = ifelse(plotID == "Lav_6_2" & species == "Alc_sp_cf", "Alc_sp", species)) |> 
  mutate(species = ifelse(plotID == "Lav_6_2" & species %in% c("Car_cap", "Car_sp"), "Car_nor", species)) |> 
  mutate(species = ifelse(plotID == "Lav_6_2" & species == "Agr_cap", "Agr_mer", species)) |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Lav_6_2" & species == "Agr_mer", 2, cover)) |> 
  distinct()

find_errors_unknowns(community_lav_6_2, "Lav_6_2")


# Lav_6_3
find_errors_unknowns(community_gudmedalen, "Lav_6_3")
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2023, "Lav_6_3")
# Vegetation height and moss depth not typed in
turfplot(community_gudmedalen, "Lav_6_3")
# Car_big_cf is Car_big
# Car_sp_den_lyse and Car_nor_cf are Car_nor

community_lav_6_3 <- community_lav_6_2 |> 
  mutate(vegetation_height_mm = ifelse(year == 2023 & plotID == "Lav_6_3" & subPlot == 10, 100, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2023 & plotID == "Lav_6_3" & subPlot == 12, 95, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2023 & plotID == "Lav_6_3" & subPlot == 24, 100, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2023 & plotID == "Lav_6_3" & subPlot == 26, 85, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2023 & plotID == "Lav_6_3" & subPlot == 10, 15, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2023 & plotID == "Lav_6_3" & subPlot == 12, 35, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2023 & plotID == "Lav_6_3" & subPlot == 24, 15, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2023 & plotID == "Lav_6_3" & subPlot == 26, 30, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Lav_6_3" & species == "Car_big_cf", "Car_big", species)) |> 
  mutate(species = ifelse(plotID == "Lav_6_3" & species %in% c("Car_sp_den_lyse", "Car_nor_cf"), "Car_nor", species), 
         value = ifelse(year == 2022 & plotID == "Lav_6_3" & subPlot == 16 & species == "Car_nor", 1, value))

find_errors_unknowns(community_lav_6_3, "Lav_6_3")


# Lav_6_5
find_errors_unknowns(community_gudmedalen, "Lav_6_5")
turfplot(community_gudmedalen, "Lav_6_5")
# Jun_tri_CF is Ave_fle
# Car_sp_den_lyse is Car_big

community_lav_6_5 <- community_lav_6_3 |> 
  mutate(species = ifelse(plotID == "Lav_6_5" & species == "Jun_tri_CF", "Ave_fle", species)) |> 
  mutate(species = ifelse(plotID == "Lav_6_5" & species == "Car_sp_den_lyse", "Car_big", species), 
         cover = ifelse(year == 2022 & plotID == "Lav_6_5" & species == "Car_big", 7, cover)) |> 
  filter(!(plotID == "Lav_6_5" & species == "Nid_seedling")) |> 
  distinct()


find_errors_unknowns(community_lav_6_5, "Lav_6_5")


# Lav_6_6
find_errors_unknowns(community_gudmedalen, "Lav_6_6")
# Species value. 2018 Vio_bif is 1
turfplot(community_gudmedalen, "Lav_6_6")
# Car_nor_cf is Car_nor
# Car_pil_cf is Car_pil
# Unknown. The scan says Hyp_mac, but this has never been found in this plot, I remove it

community_lav_6_6 <- community_lav_6_5 |> 
  mutate(value = ifelse(year == 2018 & plotID == "Lav_6_6" & subPlot == 17 & species == "Vio_bif", 1, value)) |> 
  mutate(species = ifelse(plotID == "Lav_6_6" & species == "Car_nor_cf", "Car_nor", species)) |> 
  mutate(species = ifelse(plotID == "Lav_6_6" & species == "Car_pil_cf", "Car_pil", species)) |> 
  filter(!(plotID == "Lav_6_6" & species %in% c("Nid_seedling", "Unknown")))


find_errors_unknowns(community_lav_6_6, "Lav_6_6")


# Lav_7_1
find_errors_unknowns(community_gudmedalen, "Lav_7_1")
# Species value. 2022 Fes_viv. We group Fes_viv and Fes_ovi together
# Species cover. 2022 Ver_alp cover typed in subplot 26
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_7_1")
# Vegetation height and moss depth measured in subplot 31, since logger was in 24
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2022, "Lav_7_1")
# Moss depth. No moss in the missing subplot, we change it to 0
# Subplot number. 2021 Sib_pro 9 is actually 8
turfplot(community_gudmedalen, "Lav_7_1")
# Ran_acr_cf is Ran_acr
# Car_sp is Car_nor
# Oma_sp is Oma_sup

community_lav_7_1 <- community_lav_6_6 |> 
  mutate(species = ifelse(plotID == "Lav_7_1" & species == "Fes_ovi", "Fes_viv", species), 
         cover = ifelse(year == 2019 & plotID == "Lav_7_1" & species == "Fes_viv", 4, cover), 
         value = ifelse(year == 2022 & plotID == "Lav_7_1" & subPlot== 26 & species == "Fes_viv", "F", value), 
         cover = ifelse(year == 2022 & plotID == "Lav_7_1" & species == "Fes_viv", 2, cover)) |> 
  filter(!(year == 2022 & plotID == "Lav_7_1" & subPlot == 26 & species == "Ver_alp")) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Lav_7_1" & subPlot == 24, 0, moss_depth_mm)) |> 
  mutate(cover = ifelse(year == 2022 & plotID == "Lav_7_1" & species == "Ver_alp", 1, cover)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Lav_7_1" & subPlot == 9 , 8, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Lav_7_1" & subPlot == 8, 35, moss), 
         litter = ifelse(year == 2021 & plotID == "Lav_7_1" & subPlot == 8, 5, litter)) |> 
  mutate(species = ifelse(plotID == "Lav_7_1" & species == "Ran_acr_cf", "Ran_acr", species)) |> 
  mutate(species = ifelse(plotID == "Lav_7_1" & species == "Car_sp", "Car_nor", species)) |> 
  mutate(species = ifelse(plotID == "Lav_7_1" & species == "Oma_sp", "Oma_sup", species)) |> 
  filter(!(plotID == "Lav_7_1" & species == "Nid_seedling"))

find_errors_unknowns(community_lav_7_1, "Lav_7_1")


# Lav_7_2
find_errors_unknowns(community_gudmedalen, "Lav_7_2")
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2019, "Lav_7_2")
# Vegetation cover, vegetation height and moss depth not typed in 2019
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_7_2")
# Moss depth not typed in 2021
turfplot(community_gudmedalen, "Lav_7_2")
# Car_nor_cf is Car_big
# Ant_sp is Ant_dio
# Fes_ovi is Fes_viv
# Luz_spi is Luz_mul
# Poa_alp is Poa_pra

community_lav_7_2 <- community_lav_7_1 |> 
  mutate(vegetation_cover = ifelse(year == 2019 & plotID == "Lav_7_2", 85, vegetation_cover), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 10, 5, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 12, 28, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 24, 25, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 26, 35, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 10, 15, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 12, 24, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 24, 14, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 26, 17, moss_depth_mm), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 1, 10, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 2, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 3, 40, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 4, 20, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 5, 70, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 6, 60, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 7, 70, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 8, 20, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 10, 10, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 12, 40, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 14, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 15, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 16, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 17, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 18, 25, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 19, 60, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 20, 60, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 21, 60, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 22, 80, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 24, 30, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 26, 20, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 28, 10, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 29, 30, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 30, 20, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 31, 10, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 32, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 33, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 34, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 35, 10, moss), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 1, 30, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 2, 20, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 3, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 4, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 5, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 6, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 7, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 8, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 10, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 12, 20, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 14, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 15, 20, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 16, 10, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 17, 1, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 18, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 19, 10, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 20, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 21, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 22, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 24, 1, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 26, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 28, NA, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 29, 2, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 30, 15, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 31, 25, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 32, 20, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 33, 25, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 34, 25, lichen), 
         lichen = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 35, 10, lichen), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 1, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 2, 50, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 3, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 4, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 5, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 6, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 7, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 8, 70, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 10, 70, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 12, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 14, 70, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 15, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 16, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 17, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 18, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 19, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 20, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 21, 60, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 22, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 24, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 26, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 28, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 29, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 30, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 31, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 32, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 33, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 34, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 35, 20, litter), 
         bare_ground = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 10, 10, bare_ground), 
         rock = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 18, 5, rock), 
         rock = ifelse(year == 2019 & plotID == "Lav_7_2" & subPlot == 31, 5, rock)) |> 
  mutate(moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_7_2" & subPlot == 10, 20, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_7_2" & subPlot == 12, 35, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_7_2" & subPlot == 24, 15, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Lav_7_2" & subPlot == 26, 20, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Lav_7_2" & species == "Car_nor_cf", "Car_big", species), 
         cover = ifelse(plotID == "Lav_7_2" & species == "Car_big" & plotID == "Lav_7_2", 3, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_7_2" & species == "Ant_sp", "Ant_dio", species)) |> 
  mutate(species = ifelse(plotID == "Lav_7_2" & species == "Fes_ovi", "Fes_viv", species), 
         cover = ifelse(year == 2019 & plotID == "Lav_7_2" & species == "Fes_viv", 3, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_7_2" & species == "Luz_spi", "Luz_mul", species)) |> 
  mutate(species = ifelse(plotID == "Lav_7_2" & species == "Poa_alp", "Poa_pra", species), 
         cover = ifelse(year == 2019 & plotID == "Lav_7_2" & species == "Poa_pra", 2, cover)) |> 
  distinct()

find_errors_unknowns(community_lav_7_2, "Lav_7_2")


# Lav_7_3
find_errors_unknowns(community_gudmedalen, "Lav_7_3")
# Species value. 2021 Vio_bif 21 is S1, not S1S (found afterwards, not with this function)
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2019, "Lav_7_3")
# Vegetation cover, vegetation height and moss depth not typed in 2019
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2021, "Lav_7_3")
# Vegetation height and moss depth. Logger in subplot 10
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2022, "Lav_7_3")
# Vegetation height and moss depth not typed in for subplot 26
check_vegetation_cover_height_moss_depth(community_gudmedalen, 2023, "Lav_7_3")
# Moss depth. Moss absent in the missing subplots, we change it to 0
# Subplot number. Some columns have shifted left: I change 13 to 15 and 16 to 18
turfplot(community_gudmedalen, "Lav_7_3")
# Ave_fle and Fes_viv are Fes_ovi
# Des_alp is Des_ces
# Poa_alp is Poa_pra

community_lav_7_3 <- community_lav_7_2 |> 
  mutate(value = ifelse(year == 2021 & plotID == "Lav_7_3" & subPlot == 14 & species == "Vio_bif", "S1", value)) |> 
  mutate(vegetation_cover = ifelse(year == 2019 & plotID == "Lav_7_3", 75, vegetation_cover), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 10, 60, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 12, 40, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 24, 70, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 26, 30, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 10, 25, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 12, 30, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 24, 20, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 26, 25, moss_depth_mm), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 1, 25, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 2, 20, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 3, 30, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 4, 75, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 5, 75, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 6, 50, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 7, 20, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 8, 50, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 10, 1, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 12, 50, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 14, 25, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 15, 50, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 16, 50, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 17, 10, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 18, 40, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 19, 40, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 20, 25, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 21, 60, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 22, 10, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 24, 25, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 26, 50, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 28, 30, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 29, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 30, 70, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 31, 30, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 32, 50, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 33, 50, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 34, 25, moss), 
         moss = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 35, 40, moss), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 1, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 2, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 3, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 4, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 5, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 6, 5, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 7, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 8, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 10, 5, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 12, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 14, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 15, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 16, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 17, 75, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 18, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 19, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 20, 5, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 21, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 22, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 24, 60, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 26, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 28, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 29, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 30, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 31, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 32, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 33, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 34, 50, litter), 
         litter = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 35, 15, litter), 
         bare_ground = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 2, 5, bare_ground), 
         bare_ground = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 12, 5, bare_ground), 
         bare_ground = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 34, 5, bare_ground), 
         rock = ifelse(year == 2019 & plotID == "Lav_7_3" & subPlot == 12, 10, rock)) |> 
  mutate(vegetation_height_mm = ifelse(year == 2022 & plotID == "Lav_7_3" & subPlot == 26, 60, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2022 & plotID == "Lav_7_3" & subPlot == 26, 30, moss_depth_mm)) |> 
  mutate(moss_depth_mm = ifelse(year == 2023 & plotID == "Lav_7_3" & subPlot %in% c(10, 24), 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Lav_7_3" & subPlot == 13 , 15, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Lav_7_3" & subPlot == 15, 35, moss), 
         litter = ifelse(year == 2021 & plotID == "Lav_7_3" & subPlot == 15, 15, litter)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Lav_7_3" & subPlot == 16 & species == "Ant_odo", 18, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Lav_7_3" & subPlot == 18, 25, moss), 
         litter = ifelse(year == 2021 & plotID == "Lav_7_3" & subPlot == 18, 25, litter)) |> 
  mutate(species = ifelse(plotID == "Lav_7_3" & species %in% c("Ave_fle", "Fes_viv"), "Fes_ovi", species)) |> 
  mutate(species = ifelse(plotID == "Lav_7_3" & species == "Des_alp", "Des_ces", species), 
         cover = ifelse(year == 2021 & plotID == "Lav_7_3" & species =="Des_ces", 6, cover)) |> 
  mutate(species = ifelse(plotID == "Lav_7_3" & species == "Poa_alp", "Poa_pra", species)) |> 
  filter(!(plotID == "Lav_7_3" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_lav_7_3, "Lav_7_3")


# 12. We create an object and file only for lavisdalen

community_lavisdalen <- community_lav_7_3 |> arrange(year, plotID, subPlot)
community_lavisdalen |> filter(site == "Lavisdalen") |> write.csv("data_cleaned/Lavisdalen.csv")


## Skjellingahaugen----

# 13. We work on each plot in turn

community_lavisdalen |> filter(site == "Skjellingahaugen") |> select(plotID) |> distinct() |> print(n = Inf)
# Skj_1_1, Skj_1_3, Skj_1_4, Skj_1_5, Skj_2_1, Skj_2_2, Skj_2_5, Skj_2_6, Skj_3_1, Skj_3_3, Skj_3_4, Skj_3_6, Skj_4_1, Skj_4_2, Skj_4_3, Skj_4_4, Skj_4_5, Skj_5_1, Skj_5_2, Skj_5_3, Skj_5_4, Skj_5_5, Skj_5_6, Skj_6_2 Skj_6_3 Skj_6_4 Skj_6_6 Skj_7_1 Skj_7_2 Skj_7_5

# For a few of the plots we need to add Agr_mer for 2019 and 2022

# Skj_1_1
find_errors_unknowns(community_lavisdalen, "Skj_1_1")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2022, "Skj_1_1")
turfplot(community_lavisdalen, "Skj_1_1")
# Value cf. Car_big_cf and Car_sp are Car_big
# Value cf. Fes_rub is correct
# Car_nor_cf is Car_nor
# Ran_acr_cf is Ran_acr
# Sib_rpo_cf is Sib_pro
# Pyr_sp is wrong. Looking at turfmapper it seems it might be Pin_vul
# Unknown. Not enough info

skj_1_1_2019_agr_mer <- fix_species_before(community_lavisdalen, 2018, "Skj_1_1", "Agr_mer")
skj_1_1_2022_agr_mer <- fix_species_before(community_lavisdalen, 2021, "Skj_1_1", "Agr_mer")

community_skj_1_1 <- community_lavisdalen |> 
  mutate(species = ifelse(plotID == "Skj_1_1" & species %in% c("Car_big_cf", "Car_sp"), "Car_big", species), 
         value = ifelse(year == 2021 & plotID == "Skj_1_1" & subPlot == 4 & species == "Car_big", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Skj_1_1" & species == "Car_big", 2, cover), 
         value = ifelse(year == 2022 & plotID == "Skj_1_1" & subPlot == 16 & species == "Car_big", 1, value), 
         cover = ifelse(year == 2022 & plotID == "Skj_1_1" & species == "Car_big", 6, cover )) |> 
  mutate(value = ifelse(year == 2022 & plotID == "Skj_1_1" & subPlot == 20 & species == "Fes_rub", 1, value)) |> 
  mutate(species = ifelse(plotID == "Skj_1_1" & species == "Car_nor_cf", "Car_nor", species)) |> 
  mutate(species = ifelse(plotID == "Skj_1_1" & species == "Ran_acr_cf", "Ran_acr", species)) |> 
  mutate(species = ifelse(plotID == "Skj_1_1" & species == "Sib_pro_cf", "Sib_pro", species)) |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Skj_1_1" & species == "Sib_pro", 4, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_1_1" & species == "Pyr_sp", "Pin_vul", species)) |> 
  bind_rows(skj_1_1_2019_agr_mer) |> 
  bind_rows(skj_1_1_2022_agr_mer) |> 
  filter(!(plotID == "Skj_1_1" & species %in% c("Nid_seedling", "Unknown"))) |> 
  distinct()

find_errors_unknowns(community_skj_1_1, "Skj_1_1")


# Skj_1_3
find_errors_unknowns(community_lavisdalen, "Skj_1_3")
# Species value. 2019 Leo_aut says H, it is J
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2021, "Skj_1_3")
# Vegetation height and moss depth measured in 31, since logger in 24
# Subplot number. 2021 Agr_cap. 11 is 12, and 14 is 15. 17 should be empty (only logger)
turfplot(community_lavisdalen, "Skj_1_3")
# Car_big_cf and Car_mor are Car_big, after checking turfmapper in the field in 2023

skj_1_3_2019_agr_mer <- fix_species_before(community_lavisdalen, 2018, "Skj_1_3", "Agr_mer")
skj_1_3_2022_agr_mer <- fix_species_before(community_lavisdalen, 2021, "Skj_1_3", "Agr_mer")

community_skj_1_3 <- community_skj_1_1 |> 
  mutate(value = ifelse(year == 2019 & plotID == "Skj_1_3" & subPlot == 16 & species == "Leo_aut", "J", value)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Skj_1_3" & subPlot == 11 , 12, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Skj_1_3" & subPlot == 12, 25, moss), 
         litter = ifelse(year == 2021 & plotID == "Skj_1_3" & subPlot == 12, 15, litter), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Skj_1_3" & subPlot == 12, 32, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Skj_1_3" & subPlot == 12, 12, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Skj_1_3" & subPlot == 14 & species == "Agr_cap", 15, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Skj_1_3" & subPlot == 15, 35, moss), 
         litter = ifelse(year == 2021 & plotID == "Skj_1_3" & subPlot == 15, 10, litter)) |> 
  filter(!(year == 2021 & plotID == "Skj_1_3" & subPlot == 17)) |> 
  mutate(species = ifelse(year == 2018 & plotID == "Skj_1_3" & species %in% c("Car_big_cf", "Car_nor"), "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_1_3" & species == "Car_big", 3, cover)) |> 
  bind_rows(skj_1_3_2019_agr_mer) |> 
  bind_rows(skj_1_3_2022_agr_mer) |> 
  filter(!(plotID == "Skj_1_3" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_1_3, "Skj_1_3")


# Skj_1_4
find_errors_unknowns(community_lavisdalen, "Skj_1_4")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2021, "Skj_1_4")
# Vegetation height and moss depth measured on subplot 31, since logger in 24
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2022, "Skj_1_4")
# Moss depth. No moss in the missing subplots, we change it to 0
turfplot(community_lavisdalen, "Skj_1_4")
# Value cf. Fes_rub is correct
# Agr_cap_cf and Agr_mer are Agr_cap
# Leo_aut_cf is Leo_aut
# Eri_sp. Eri_bor found in the same block
# Rhi_sp is probably Rhi_min (also found in Skj_1_5)
# Unknown. Not enough info
# Car_fla is Car_vag. Most years it has been called Car_vag, and when it was called Car_fla it says specifically that it could be Car_vag.

community_skj_1_4 <- community_skj_1_3 |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Skj_1_4" & subPlot == 12, 0, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2022 & plotID == "Skj_1_4" & species == "Fes_rub", 1, value)) |> 
  mutate(species = ifelse(plotID == "Skj_1_4" & species %in% c("Agr_cap_cf","Agr_mer") , "Agr_cap", species)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Skj_1_4" & species == "Agr_cap", 25, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_1_4" & species == "Leo_aut_cf", "Leo_aut", species)) |> 
  mutate(species = ifelse(plotID == "Skj_1_4" & species == "Rhi_sp", "Rhi_min", species)) |> 
  mutate(species = ifelse(plotID == "Skj_1_4" & species == "Eri_sp", "Eri_bor", species)) |> 
  mutate(species = ifelse(plotID == "Skj_1_4" & species == "Car_fla", "Car_vag", species)) |>  
  filter(!(plotID == "Skj_1_4" & species == "Unknown"))

find_errors_unknowns(community_skj_1_4, "Skj_1_4")


# Skj_1_5
find_errors_unknowns(community_lavisdalen, "Skj_1_5")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2022, "Skj_1_5")
# Moss depth. No moss in the missing subplot, we change it to 0
# Subplot number. 2021 Tha_alp has been shifted to the right. 23 is 22, 25 is 24 and 27 is 26
turfplot(community_lavisdalen, "Skj_1_5")
# Pot_cra is Pot ere

skj_1_5_2019_agr_mer <- fix_species_before(community_lavisdalen, 2018, "Skj_1_5", "Agr_mer")
skj_1_5_2022_agr_mer <- fix_species_before(community_lavisdalen, 2021, "Skj_1_5", "Agr_mer")

community_skj_1_5 <- community_skj_1_4 |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Skj_1_5" & subPlot == 24, 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 23, 22, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 22, 10, moss), 
         litter = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 22, 20, litter)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 25, 24, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 24, 5, moss), 
         litter = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 24, 15, litter), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 24, 75, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 24, 17, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 27, 26, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 26, 60, moss), 
         litter = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 26, 10, litter), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 26, 85, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Skj_1_5" & subPlot == 26, 27, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2022 & plotID == "Skj_1_5" & subPlot == 9 & species == "Car_cap", "OF", value)) |> 
  mutate(species = ifelse(year == 2022 & plotID == "Skj_1_5" & species == "Pot_cra", "Pot_ere", species)) |> 
  bind_rows(skj_1_5_2019_agr_mer) |> 
  bind_rows(skj_1_5_2022_agr_mer) |> 
  filter(!(plotID == "Skj_1_5" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_1_5, "Skj_1_5")


# Skj_2_1
find_errors_unknowns(community_lavisdalen, "Skj_2_1")
# Species cover. Ave_fle not recorded, it must have been 1
# Species cover. Cal_vul not recorded, it must have been 1
# Species cover. Eup_wet not typed in, it was 2
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2023, "Skj_2_1")
# Vegetation height and moss depth. Logger in that subplot, recorded in subplot 3 instead
turfplot(community_lavisdalen, "Skj_2_1")
# Epi_ana_cf is Epi_ana
# Car_atr is Car_big
# Ver_cha_eller_Hyp_mac- After some discussion we have agreed this is Hyp_mac (it was seen in the field close to the plot)

community_skj_2_1 <- community_skj_1_5 |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Skj_2_1" & species == "Ave_fle", 1, cover)) |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Skj_2_1" & species == "Cal_vul", 1, cover)) |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Skj_2_1" & species == "Eup_wet", 2, cover)) |> 
  mutate(species = ifelse(year == 2018 & plotID == "Skj_2_1" & species == "Epi_ana_cf", "Epi_ana", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_2_1" & species == "Epi_ana", 1, cover)) |> 
  mutate(species = ifelse(year == 2018 & plotID == "Skj_2_1" & species == "Car_atr", "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_2_1" & species == "Car_big", 7, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_2_1" & species == "Ver_cha_eller_Hyp_mac", "Hyp_mac", species)) |> 
  filter(!(plotID == "Skj_2_1" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_skj_2_1, "Skj_2_1")


# Skj_2_2
find_errors_unknowns(community_lavisdalen, "Skj_2_2")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2023, "Skj_2_2")
# Vegetation height and moss depth. Logger in that subplot, recorded in subplot 3 instead
turfplot(community_lavisdalen, "Skj_2_2")
# Car_nor_cf is Car_big
# Ran_acr_cf is Ran_acr
# Gen_sp is Gen_niv
# Agr_mer is Agr_cap. Agr_mer was only found one year, not in 2023, and there was no comment on turfmapper

community_skj_2_2 <- community_skj_2_1 |> 
  mutate(species = ifelse(plotID == "Skj_2_2" & species == "Car_nor_cf", "Car_big", species), 
         value = ifelse (year == 2018 & plotID == "Skj_2_2" & subPlot == 16 & species == "Car_big", "F", value), 
         cover = ifelse(year == 2018 & plotID == "Skj_2_2" & species == "Car_big", 6, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_2_2" & species == "Ran_acr_cf", "Ran_acr", species)) |> 
  mutate(species = ifelse(plotID == "Skj_2_2" & species == "Gen_sp", "Gen_niv", species)) |> 
  mutate(species = ifelse(plotID == "Skj_2_2" & species == "Agr_mer", "Agr_cap", species)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Skj_2_2" & species == "Agr_cap", 5, cover)) |> 
  filter(!(plotID == "Skj_2_2" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_skj_2_2, "Skj_2_2")


# Skj_2_5
find_errors_unknowns(community_lavisdalen, "Skj_2_5")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2021, "Skj_2_5")
# Vegetation height and moss depth. Logger in subplot 10, registered in 16 instead
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2023, "Skj_2_5")
# Moss depth. No moss in the missing subplot
turfplot(community_lavisdalen, "Skj_2_5")
# Alc_sp_cf is Alc_sp
# Cer_cer_cf is Cer_cer
# Epi_ana_cf and Epi_sp are Epi_ana
# Equ_sp in 2019 is Equ_arv. In 2021 it was actually Eup_sp, which is Eup_wet
# Hie_sp. There is Hie_pil in the block next to it

community_skj_2_5 <- community_skj_2_2 |> 
  mutate(moss_depth_mm = ifelse(year == 2023 & plotID == "Skj_2_5" & subPlot == 12, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Skj_2_5" & species == "Alc_sp_cf", "Alc_sp", species)) |> 
  mutate(species = ifelse(plotID == "Skj_2_5" & species == "Cer_cer_cf", "Cer_cer", species)) |> 
  mutate(species = ifelse(plotID == "Skj_2_5" & species %in% c("Epi_ana_cf", "Epi_sp"), "Epi_ana", species)) |> 
  mutate(species = ifelse(year == 2019 & plotID == "Skj_2_5" & species == "Equ_sp", "Equ_arv", species)) |> 
  mutate(species = ifelse(year == 2021 & plotID == "Skj_2_5" & species == "Equ_sp", "Eup_wet", species)) |> 
  mutate(species = ifelse(plotID == "Skj_2_5" & species == "Hie_sp", "Hie_pil", species))

find_errors_unknowns(community_skj_2_5, "Skj_2_5")


# Skj_2_6
find_errors_unknowns(community_lavisdalen, "Skj_2_6")
turfplot(community_lavisdalen, "Skj_2_6")
# Value cf. Ver_off is correct
# Agr_cap_cf is Agr_cap
# Alc_sp_cf is Alc_sp
# Car_vag_CF is Car_vag
# Ran_acr_cf is Ran_acr
# Fes_sp is Fes_rub
# Poa_pra is Poa_alp

community_skj_2_6 <- community_skj_2_5 |> 
  mutate(value = ifelse(year == 2021 & plotID == "Skj_2_6" & subPlot %in% c(1, 3) & species == "Ver_off", "s", value)) |> 
  mutate(species = ifelse(plotID == "Skj_2_6" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Skj_2_6" & species == "Alc_sp_cf", "Alc_sp", species)) |> 
  mutate(species = ifelse(plotID == "Skj_2_6" & species == "Car_vag_CF", "Car_vag", species)) |> 
  mutate(species = ifelse(plotID == "Skj_2_6" & species == "Ran_acr_cf", "Ran_acr", species), 
         value = ifelse(year == 2021 & plotID == "Skj_2_6" & subPlot == 15 & species == "Ran_acr", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Skj_2_6" & species == "Ran_acr", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_2_6" & species == "Fes_sp", "Fes_rub", species)) |> 
  mutate(species = ifelse(year == 2021 & plotID == "Skj_2_6" & species == "Poa_pra", "Poa_alp", species)) |> 
  distinct()

find_errors_unknowns(community_skj_2_6, "Skj_2_6")


# Skj_3_1
find_errors_unknowns(community_lavisdalen, "Skj_3_1")
# Species cover. 2023 Cam_rot not recorded, we use the value from the previous year
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2018, "Skj_3_1")
# Vegetation height and moss depth mixed up, we fix it
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2022, "Skj_3_1")
# Moss depth. No moss in the missing subplot, we change it to 0
# Subplot number. Vio_pal. It's subplot 14, not 13
turfplot(community_lavisdalen, "Skj_3_1")
# Car_nig_cf is probably Car_big (the recorder also noted the possibility)
# Car_pil_cf is Car_pal
# Epi_ana_cf is Epi_ana
# Ach_mil is Agr_cap
# Cer_cer is Cer_fon
# Geu_riv is Gen_niv

community_skj_3_1 <- community_skj_2_6 |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Skj_3_1" & species == "Cam_rot", 1, cover)) |> 
  mutate(vegetation_height_mm = ifelse(year == 2018 & plotID == "Skj_3_1" & subPlot == 10, 65, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2018 & plotID == "Skj_3_1" & subPlot == 12, 58, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2018 & plotID == "Skj_3_1" & subPlot == 24, 35, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2018 & plotID == "Skj_3_1" & subPlot == 26, 58, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Skj_3_1" & subPlot == 10, 18, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Skj_3_1" & subPlot == 12, 18, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Skj_3_1" & subPlot == 24, 9, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Skj_3_1" & subPlot == 26, 8, moss_depth_mm)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Skj_3_1" & subPlot == 24, 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2019 & plotID == "Skj_3_1" & subPlot == 13, 14, subPlot), 
         moss = ifelse(year == 2019 & plotID == "Skj_3_1" & subPlot == 14, 70, moss), 
         litter = ifelse(year == 2019 & plotID == "Skj_3_1" & subPlot == 14, 5, litter), 
         bare_ground = ifelse(year == 2019 & plotID == "Skj_3_1" & subPlot == 14, 2, bare_ground)) |> 
  mutate(species = ifelse(plotID == "Skj_3_1" & species == "Car_nig_cf", "Car_big", species), 
         cover = ifelse(year == 2023 & plotID == "Skj_3_1" & species == "Car_big", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_3_1" & species == "Car_pil_cf", "Car_pal", species), 
         cover = ifelse(year == 2021 & plotID == "Skj_3_1" & species == "Car_pal", 5, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_3_1" & species == "Epi_ana_cf", "Epi_ana", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_3_1" & species == "Epi_ana", 2, cover)) |> 
  mutate(species = ifelse(species == "Ach_mil" & plotID == "Skj_3_1", "Agr_cap", species)) |>
  mutate(species = ifelse(plotID == "Skj_3_1" & species == "Cer_cer", "Cer_fon", species)) |> 
  mutate(species = ifelse(plotID == "Skj_3_1" & species == "Geu_riv", "Gen_niv", species)) |> 
  filter(!(plotID == "Skj_3_1" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_skj_3_1, "Skj_3_1")


# Skj_3_3
find_errors_unknowns(community_lavisdalen, "Skj_3_3")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2021, "Skj_3_3")
# vegetation height and moss depth measured in subplot 31, since logger in 24
turfplot(community_lavisdalen, "Skj_3_3")
# Value cf. Sau_alp is correct
# Agr_cap_cf and Ach_mil are Agr_cap
# Gen_cam_cf is Gen_niv
# Fern. We remove it
# Car_pal is Car_pil
# Luz_spi is Luz_mul

community_skj_3_3 <- community_skj_3_1 |> 
  mutate(value = ifelse(year == 2022 & plotID == "Skj_3_3" & subPlot %in% c(10, 12) & species == "Sau_alp", 1, value)) |> 
  mutate(species = ifelse(plotID == "Skj_3_3" & species %in% c("Ach_mil", "Agr_cap_cf"), "Agr_cap", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_3_3" & species == "Agr_cap", 3, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_3_3" & species == "Gen_cam_cf", "Gen_niv", species)) |> 
  mutate(species = ifelse(plotID == "Skj_3_3" & species == "Car_pal" , "Car_pil", species))|> 
  mutate(species = ifelse(plotID == "Skj_3_3" & species == "Luz_spi", "Luz_mul", species)) |> 
  filter(!(plotID == "Skj_3_3" & species %in% c("Fern", "Nid_seedling")))

find_errors_unknowns(community_skj_3_3, "Skj_3_3")


# Skj_3_4
find_errors_unknowns(community_lavisdalen, "Skj_3_4")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2021, "Skj_3_4")
# Vegetation height and moss depth not registered
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2022, "Skj_3_4")
# Moss depth. Moss absent in the missing subplot, we change it to 0
turfplot(community_lavisdalen, "Skj_3_4")
# Value cf. Bar_alp is correct
# Epi_sp is probably a juvenile Ver_alp
# Oma_sp is Oma_sup
# Nid_orchid and Orchid are the same, were written twice, and it's Dac_vir
# Rum_acl is Rum_ace

community_skj_3_4 <- community_skj_3_3 |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Skj_3_4" & subPlot == 12, 0, moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2021 & plotID =="Skj_3_4" & subPlot == 22 & species == "Bar_alp", 1, value)) |> 
  mutate(species = ifelse(plotID =="Skj_3_4" & species == "Epi_sp", "Ver_alp", species), 
         value = ifelse(year == 2021 & plotID =="Skj_3_4" & subPlot == 1 & species == "Ver_alp", "1J", value), 
         cover = ifelse(year == 2021 & plotID =="Skj_3_4" & species == "Ver_alp", 3, cover)) |> 
  mutate(species = ifelse(plotID =="Skj_3_4" & species == "Oma_sp", "Oma_sup", species)) |> 
  filter(!(plotID == "Skj_3_4" & species == "Nid_orchid")) |> 
  mutate(species = ifelse(plotID =="Skj_3_4" & species == "Orchid", "Dac_vir", species)) |> 
  mutate(species = ifelse(plotID =="Skj_3_4" & species == "Rum_acl", "Rum_ace", species)) |> 
  filter(!(plotID == "Skj_3_4" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_skj_3_4, "Skj_3_4")


# Skj_3_6
find_errors_unknowns(community_lavisdalen, "Skj_3_6")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2021, "Skj_3_6")
# Vegetation height and moss depth mixed up, we fix it
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2022, "Skj_3_6")
# Vegetation cover, vegetation height and moss depth not recorded. Non-vascular plant cover recorded only in subplot 10, we remove them, since it's not representative for the whole plot (and we don't want the average for the plot to be calculated from only one subplot)
turfplot(community_lavisdalen, "Skj_3_6")
# Value cf. Vac_myr was probably Sal_her. And Vac_myr was also recorded in 2022, but not typed in. It was the wrong species though, it was probably Tha_alp
# Epi_sp is Epi_ana
# The orchid is Dac_vir
# Car_nor and Car_fla are changed to Car_big after turfmapper check in 2023. First cover added up, second estimated (14% would've been too much)
# Rum_acl is Rum_ace

skj_3_6_2022_tha_alp <- community_lavisdalen |> 
  filter(year == 2022 & plotID == "Skj_3_6" & subPlot %in% c(10, 12)) |> 
  mutate(species = "Tha_alp", value = "1", cover = "6") |> 
  distinct()

community_skj_3_6 <- community_skj_3_4 |> 
  mutate(vegetation_height_mm = ifelse(year == 2021 & plotID == "Skj_3_6" & subPlot == 17, 21, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Skj_3_6" & subPlot == 12, 24, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Skj_3_6" & subPlot == 24, 26, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Skj_3_6" & subPlot == 26, 25, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Skj_3_6" & subPlot == 17, 5, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Skj_3_6" & subPlot == 12, 10, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Skj_3_6" & subPlot == 24, 5, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Skj_3_6" & subPlot == 26, 4, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Skj_3_6" & species == "Vac_myr", "Sal_her", species), 
         value = ifelse(year == 2021 & plotID == "Skj_3_6" & subPlot == 1 & species == "Sal_her", "J", value), ) |> 
  bind_rows(skj_3_6_2022_tha_alp) |> 
  mutate(moss = ifelse(year == 2022 & plotID == "Skj_3_6" & subPlot == 10, NA, moss), 
         litter = ifelse(year == 2022 & plotID == "Skj_3_6" & subPlot == 10, NA, litter), 
         logger = ifelse(year == 2022 & plotID == "Skj_3_6" & subPlot == 10, NA, logger)) |> 
  mutate(species = ifelse(plotID == "Skj_3_6" & species == "Epi_sp", "Epi_ana", species)) |> 
  mutate(species = ifelse(plotID == "Skj_3_6" & species == "Orchid", "Dac_vir", species)) |> 
  mutate(species = ifelse(plotID == "Skj_3_6" & species == "Car_nor", "Car_big", species), 
         cover = ifelse(year == 2019 & plotID == "Skj_3_6" & species == "Car_big", 3, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_3_6" & species == "Car_fla", "Car_big", species), 
         cover = ifelse(year == 2021 & plotID == "Skj_3_6" & species == "Car_big", 10, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_3_6" & species == "Rum_acl", "Rum_ace", species)) |> 
  filter(!(plotID == "Skj_3_6" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_skj_3_6, "Skj_3_6")


# Skj_4_1
find_errors_unknowns(community_lavisdalen, "Skj_4_1")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2023, "Skj_4_1")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_lavisdalen, "Skj_4_1")
# Epi_ana_cf is Epi_ana
# Sag_sp is Sag_sag

community_skj_4_1 <- community_skj_3_6 |> 
  mutate(moss_depth_mm = ifelse(year == 2023 & plotID == "Skj_4_1" & subPlot == 10, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Skj_4_1" & species == "Epi_ana_cf", "Epi_ana", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_4_1" & species == "Epi_ana", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_4_1" & species == "Sag_sp", "Sag_sag", species)) |> 
  filter(!(plotID == "Skj_4_1" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_4_1, "Skj_4_1")


# Skj_4_2
find_errors_unknowns(community_lavisdalen, "Skj_4_2")
turfplot(community_lavisdalen, "Skj_4_2")
# Car_big_cf and Car_fla_CF are Car_big
# Epi_ana_cf is Epi_ana
# Cer_cer is Cer_fon
# Vio_bif is Vio_pal
# In 2019 we are missing info on logger cover, since it was written in the vegetation cover column

community_skj_4_2 <- community_skj_4_1 |> 
  mutate(species = ifelse(plotID == "Skj_4_2" & species %in% c("Car_big_cf", "Car_fla_CF"), "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_4_2" & species == "Car_big", 5, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_4_2" & species == "Epi_ana_cf", "Epi_ana", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_4_2" & species == "Epi_ana", 3, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_4_2" & species == "Cer_cer", "Cer_fon", species)) |> 
  mutate(species = ifelse(plotID == "Skj_4_2" & species == "Vio_bif", "Vio_pal", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_4_2" & species == "Vio_pal", 12, cover)) |> 
  mutate(logger = ifelse(year == 2019 & plotID == "Skj_4_2" & subPlot == 4, 20, logger), 
         logger = ifelse(year == 2019 & plotID == "Skj_4_2" & subPlot == 10, 20, logger), 
         logger = ifelse(year == 2019 & plotID == "Skj_4_2" & subPlot == 12, 15, logger), 
         logger = ifelse(year == 2019 & plotID == "Skj_4_2" & subPlot == 18, 20, logger)) |> 
  filter(!(plotID == "Skj_4_2" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_4_2, "Skj_4_2")


# Skj_4_3
find_errors_unknowns(community_lavisdalen, "Skj_4_3")
# Subplot number. 2021. Shifted to the left. We change 23 and 35 for 19 and 24
turfplot(community_lavisdalen, "Skj_4_3")
# Agr_cap_cf is Agr_cap
# Ran_acr_cf is Ran_acr
# Leo_sp is Leo_aut

community_skj_4_3 <- community_skj_4_2 |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Skj_4_3" & subPlot == 23, 19, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Skj_4_3" & subPlot == 19, 5, moss), 
         litter = ifelse(year == 2021 & plotID == "Skj_4_3" & subPlot == 19, 60, litter)) |> 
  mutate(subPlot = ifelse(year == 2021 & plotID == "Skj_4_3" & subPlot == 25, 24, subPlot), 
         moss = ifelse(year == 2021 & plotID == "Skj_4_3" & subPlot == 24, 5, moss), 
         litter = ifelse(year == 2021 & plotID == "Skj_4_3" & subPlot == 24, 40, litter), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Skj_4_3" & subPlot == 24, 100, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Skj_4_3" & subPlot == 24, 10, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Skj_4_3" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Skj_4_3" & species == "Ran_acr_cf", "Ran_acr", species)) |> 
  mutate(species = ifelse(plotID == "Skj_4_3" & species == "Leo_sp", "Leo_aut", species)) |> 
  filter(!(plotID == "Skj_4_3" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_4_3, "Skj_4_3")


# Skj_4_4
find_errors_unknowns(community_lavisdalen, "Skj_4_4")
# Species cover. 2023 Phl_alp not recorded, estimated to be 1
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2021, "Skj_4_4")
# Vegetation cover not typed in. Vegetation height and moss depth measured in subplot 31, since logger in 24
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2022, "Skj_4_4")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_lavisdalen, "Skj_4_4")
# Car_sp is Car_big
# Rum_acl is Rum_ace

community_skj_4_4 <- community_skj_4_3 |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Skj_4_4" & species == "Phl_alp", 1, cover)) |> 
  mutate(vegetation_cover = ifelse(year == 2021 & plotID == "Skj_4_4", 75, vegetation_cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Skj_4_4" & subPlot == 10, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Skj_4_4" & species == "Car_sp", "Car_big", species)) |> 
  mutate(species = ifelse(plotID == "Skj_4_4" & species == "Rum_acl", "Rum_ace", species))

find_errors_unknowns(community_skj_4_4, "Skj_4_4")


# Skj_4_5
find_errors_unknowns(community_lavisdalen, "Skj_4_5")
# Species cover. 2018, not recorded for Cam_rot, Car_cap, Oma_sup, Rum_ace and Sag_sag
turfplot(community_lavisdalen, "Skj_4_5")
# Car_big_cf is Car_big
# Car_sp is Car_cap
# Oma_sp is Oma_sup

community_skj_4_5 <- community_skj_4_4 |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Skj_4_5" & species == "Cam_rot", 1, cover)) |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Skj_4_5" & species == "Rum_ace", 2, cover)) |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Skj_4_5" & species == "Sag_sag", 1, cover)) |> 
  mutate(species= ifelse(plotID == "Skj_4_5" & species == "Car_big_cf", "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_4_5" & species == "Car_big", 2, cover)) |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Skj_4_5" & species == "Car_cap", 1, cover), 
         species = ifelse(plotID == "Skj_4_5" & species == "Car_sp", "Car_cap", species)) |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Skj_4_5" & species == "Oma_sup", 1, cover), 
         species = ifelse(year == 2019 & plotID == "Skj_4_5" & species == "Oma_sp", "Oma_sup", species))

find_errors_unknowns(community_skj_4_5, "Skj_4_5")


# Skj_5_1
find_errors_unknowns(community_lavisdalen, "Skj_5_1")
# Species value. 2021 Agr_mer is 1J, not u
# Species value. 2021 Sib_pro 34 is 2, not 12 (found afterwards, not with this function)
# Subplot number 2022 Par_pal was written F in subplot 9, but it should be O
turfplot(community_lavisdalen, "Skj_5_1")
# Value cf. Ver_alp is correct
# Agr_cap_cf is Agr_cap
# Alc_sp_cf is correct
# Fes_rub_cf_kanskje_Ave_fle is Fes_rub
# Sib_pro_cf is Si_pro
# Car_fla is Car_big
# Hyp_mac is probably wrong, we delete it

skj_5_1_2019_agr_mer <- fix_species_before(community_lavisdalen, 2018, "Skj_5_1", "Agr_mer")
skj_5_1_2022_agr_mer <- fix_species_before(community_lavisdalen, 2021, "Skj_5_1", "Agr_mer")

community_skj_5_1 <- community_skj_4_5 |> 
  mutate(value = ifelse(year == 2021 & plotID == "Skj_5_1" & subPlot == 31 & species == "Agr_mer", "1J", value)) |> 
  mutate(value = ifelse(year == 2021 & plotID == "Skj_5_1" & subPlot == 34 & species == "Sib_pro", "2f", value)) |> 
  mutate(value = ifelse(year == 2022 & plotID == "Skj_5_1" & subPlot == 9 & species == "Par_pal", "O", value)) |> 
  mutate(value = ifelse(year == 2021 & plotID == "Skj_5_1" & subPlot == 22 & species == "Ver_alp", "s", value)) |> 
  mutate(species = ifelse(plotID == "Skj_5_1" & species == "Agr_cap_cf", "Agr_cap", species)) |> 
  mutate(species = ifelse(plotID == "Skj_5_1" & species == "Alc_sp_cf", "Alc_sp", species)) |> 
  mutate(species = ifelse(plotID == "Skj_5_1" & species == "Fes_rub_cf_kanskje_Ave_fle", "Fes_rub", species)) |> 
  mutate(species = ifelse(plotID == "Skj_5_1" & species == "Sib_pro_cf", "Sib_pro", species)) |> 
  mutate(species = ifelse(plotID == "Skj_5_1" & species == "Car_fla", "Car_big", species)) |> 
  filter(!(plotID == "Skj_5_1" & species == "Hyp_mac")) |> 
  bind_rows(skj_5_1_2019_agr_mer) |> 
  bind_rows(skj_5_1_2022_agr_mer) |> 
  filter(!(plotID == "Skj_5_1" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_5_1, "Skj_5_1")


# Skj_5_2
find_errors_unknowns(community_lavisdalen, "Skj_5_2")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2019, "Skj_5_2")
# Vegetation cover, vegetation height and moss depth were not typed in
turfplot(community_lavisdalen, "Skj_5_2")
# Car_fla_CF and Car_fla are Car_vag [But comment from 2021 scan?]
# Jun_tri_CF is Jun_tri
# Leo_aut_cf is Leo_aut. Also registered in 9 in 2022, but that's only if the species was not present in the studied subplot, which it was
# Equ_sp is Eup_sp which is Eup_wet
# Fes_rub is Ave_fle

skj_5_2_2019_agr_mer <- fix_species_before(community_lavisdalen, 2018, "Skj_5_2", "Agr_mer")
skj_5_2_2022_agr_mer <- fix_species_before(community_lavisdalen, 2021, "Skj_5_2", "Agr_mer")

community_skj_5_2 <- community_skj_5_1 |> 
  bind_rows(skj_5_2_2019_agr_mer) |> 
  bind_rows(skj_5_2_2022_agr_mer) |> 
  mutate(vegetation_cover = ifelse(year == 2019 & plotID == "Skj_5_2", 70, vegetation_cover), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 10, 32, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 12, 40, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 24, 52, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 26, 38, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 10, 9, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 12, 10, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 24, 8, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 26, 11, moss_depth_mm), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 1, 10, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 2, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 3, 10, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 4, 5, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 5, 5, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 6, 3, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 7, 5, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 8, 1, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 10, 1, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 12, 7, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 14, 6, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 15, 2, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 16, 5, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 17, 5, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 18, 10, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 19, 7, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 20, 2, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 21, 1, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 22, 1, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 24, 10, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 26, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 28, 5, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 29, 20, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 30, 5, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 31, 5, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 32, 5, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 33, 8, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 34, 3, moss), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 35, 3, moss), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 1, 35, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 2, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 3, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 4, 25, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 5, 35, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 6, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 7, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 8, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 10, 3, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 12, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 14, 35, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 15, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 16, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 17, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 18, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 19, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 20, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 21, 25, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 22, 5, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 24, 5, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 26, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 28, 35, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 29, 25, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 30, 25, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 31, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 32, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 33, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 34, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 35, 25, litter), 
         logger = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 2, 25, logger), 
         logger = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 3, 15, logger), 
         logger = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 10, 75, logger), 
         logger = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 16, 40, logger), 
         logger = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 17, 35, logger), 
         fungus = ifelse(year == 2019 & plotID == "Skj_5_2" & subPlot == 22, 15, fungus)) |> 
  mutate(species = ifelse(plotID == "Skj_5_2" & species %in% c("Car_fla", "Car_fla_CF"), "Car_vag", species)) |> 
  mutate(species = ifelse(plotID == "Skj_5_2" & species == "Jun_tri_CF", "Jun_tri", species)) |> 
  mutate(species = ifelse(plotID == "Skj_5_2" & species == "Leo_aut_cf", "Leo_aut", species)) |> 
  filter(!(year == 2022 & plotID == "Skj_5_2" & subPlot == 9 & species == "Leo_aut")) |> 
  mutate(species = ifelse(plotID == "Skj_5_2" & species == "Equ_sp", "Eup_wet", species)) |> 
  mutate(species = ifelse(plotID == "Skj_5_2" & species == "Fes_rub", "Ave_fle", species)) |> 
  filter(!(plotID == "Skj_5_2" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_5_2, "Skj_5_2")


# Skj_5_3
find_errors_unknowns(community_lavisdalen, "Skj_5_3")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2021, "Skj_5_3")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_lavisdalen, "Skj_5_3")
# Leo_aut_cf is Leo_aut
# Car_sp is probably Car_cap
# Fern is Phe_con

skj_5_3_2019_agr_mer <- fix_species_before(community_lavisdalen, 2018, "Skj_5_3", "Agr_mer")
skj_5_3_2022_agr_mer <- fix_species_before(community_lavisdalen, 2021, "Skj_5_3", "Agr_mer")

community_skj_5_3 <- community_skj_5_2 |> 
  mutate(moss_depth_mm = ifelse(year == 2021 & plotID == "Skj_5_3" & subPlot == 10, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Skj_5_3" & species == "Leo_aut_cf", "Leo_aut", species)) |> 
  mutate(species = ifelse(plotID == "Skj_5_3" & species == "Car_sp", "Car_cap", species), 
         cover = ifelse(year == 2021 & plotID == "Skj_5_3" & species == "Car_cap", 3, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_5_3" & species == "Fern", "Phe_con", species), 
         value = ifelse(year == 2021 & plotID == "Skj_5_3" & subPlot == 18 & species == "Phe_con", 1, value)) |> 
  bind_rows(skj_5_3_2019_agr_mer) |> 
  bind_rows(skj_5_3_2022_agr_mer) |> 
  filter(!(plotID == "Skj_5_3" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_5_3, "Skj_5_3")


# Skj_5_4
find_errors_unknowns(community_lavisdalen, "Skj_5_4")
# Species cover. 2019 Ave_fle not recorded, estimated to be 1
# Species cover. 2021 Alc_sp not readable. Looking at other years, it was probably 1
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2021, "Skj_5_4")
# Vegetation height and moss depth measured in subplot 17, since logger was in 10
turfplot(community_lavisdalen, "Skj_5_4")
# Car_pal is Car_pil
# The date in 2019 is written wrong, it is 2019-08-09, not 2018-08-09

skj_5_4_2019_agr_mer <- fix_species_before(community_lavisdalen, 2018, "Skj_5_4", "Agr_mer")
skj_5_4_2022_agr_mer <- fix_species_before(community_lavisdalen, 2021, "Skj_5_4", "Agr_mer")

community_skj_5_4 <- community_skj_5_3 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Skj_5_4" & species == "Ave_fle", 1, cover)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Skj_5_4" & species == "Alc_sp", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_5_4" & species == "Car_pal", "Car_pil", species)) |> 
  bind_rows(skj_5_4_2019_agr_mer) |> 
  mutate(date = if_else(year == 2019 & plotID == "Skj_5_4", as.Date("2019-08-09"), date), 
         moss = ifelse(year == 2019 & plotID == "Skj_5_4" & subPlot == 35, 10, moss)) |> 
  bind_rows(skj_5_4_2022_agr_mer) |> 
  filter(!(plotID == "Skj_5_4" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_5_4, "Skj_5_4")


# Skj_5_5
find_errors_unknowns(community_lavisdalen, "Skj_5_5")
# Species value. 2021 Sel_sel is a crossed-out 1, so we remove it
turfplot(community_lavisdalen, "Skj_5_5")
# Agr_mer_CF is Agr_mer
# Jun_tri_CF is Jun_tri
# Hie_sp is Hie_pil

skj_5_5_2019_agr_mer <- fix_species_before(community_lavisdalen, 2018, "Skj_5_5", "Agr_mer")

community_skj_5_5 <- community_skj_5_4 |> 
  filter(!(year == 2021 & plotID == "Skj_5_5" & subPlot == 6 & species == "Sel_sel")) |> 
  mutate(species = ifelse(plotID == "Skj_5_5" & species == "Agr_mer_CF", "Agr_mer", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_5_5" & species == "Agr_mer", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_5_5" & species == "Jun_tri_CF", "Jun_tri", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_5_5" & species == "Jun_tri", 5, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_5_5" & species == "Hie_sp", "Hie_pil", species)) |> 
  bind_rows(skj_5_5_2019_agr_mer) |> 
  mutate(date = if_else(year == 2019 & plotID == "Skj_5_5", as.Date("2019-08-09"), date))

find_errors_unknowns(community_skj_5_5, "Skj_5_5")


# Skj_5_6
find_errors_unknowns(community_lavisdalen, "Skj_5_6")
# Species cover. 2019 Tha_alp. It was 5
turfplot(community_lavisdalen, "Skj_5_6")
# Phl_alp is Agr_cap

skj_5_6_2019_agr_mer <- fix_species_before(community_lavisdalen, 2018, "Skj_5_6", "Agr_mer")
skj_5_6_2022_agr_mer <- fix_species_before(community_lavisdalen, 2021, "Skj_5_6", "Agr_mer")

community_skj_5_6 <- community_skj_5_5 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Skj_5_6" & species == "Tha_alp", 5, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_5_6" & species == "Phl_alp", "Agr_cap", species), 
         cover = ifelse(year == 2018 & plotID == "Skj_5_6" & species == "Agr_cap", 4, cover)) |> 
  bind_rows(skj_5_6_2019_agr_mer) |> 
  bind_rows(skj_5_6_2022_agr_mer) |> 
  filter(!(plotID == "Skj_5_6" & species == "Nid_seedling")) |> 
  mutate(date = if_else(year == 2019 & plotID == "Skj_5_6", as.Date("2019-08-09"), date)) |> 
  distinct()

find_errors_unknowns(community_skj_5_6, "Skj_5_6")


# Skj_6_2
find_errors_unknowns(community_lavisdalen, "Skj_6_2")
# Species cover. 2023 Ver_alp is 1
turfplot(community_lavisdalen, "Skj_6_2")
# Car_big_cf is Car_big
# Car_vag_CF is Car_vag
# Hie_sp and Hie_pil are Hie_alp

community_skj_6_2 <- community_skj_5_6 |> 
  mutate(cover = ifelse(year == 2023 & plotID == "Skj_6_2" & species == "Ver_alp", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_6_2" & species == "Car_big_cf", "Car_big", species), 
         value = ifelse(year == 2021 & plotID == "Skj_6_2" & subPlot == 16 & species == "Car_big", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Skj_6_2" & species == "Car_big", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_6_2" & species == "Car_vag_CF", "Car_vag", species)) |> 
  mutate(species = ifelse(plotID == "Skj_6_2" & species %in% c("Hie_pil", "Hie_sp"), "Hie_alp", species)) |> 
  distinct()

find_errors_unknowns(community_skj_6_2, "Skj_6_2")


# Skj_6_3
find_errors_unknowns(community_lavisdalen, "Skj_6_3")
# Species cover. 2019 Leo_aut is 2 in the scan, but it does not seem right. We change it to 6, as in 2018 and 2021
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2022, "Skj_6_3")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_lavisdalen, "Skj_6_3")
# Cer_cer_cf is Cer_cer
# Ant_sp is Ant_dio
# Epi_sp is Epi_ana
# Car_vag is Car_big
# Unknown. Not enough information, we drop it

community_skj_6_3 <- community_skj_6_2 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Skj_6_3" & species == "Leo_aut", 6, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Skj_6_3" & subPlot == 26, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Skj_6_3" & species == "Cer_cer_cf", "Cer_cer", species)) |> 
  mutate(species = ifelse(plotID == "Skj_6_3" & species == "Ant_sp", "Ant_dio", species)) |> 
  mutate(species = ifelse(plotID == "Skj_6_3" & species == "Epi_sp", "Epi_ana", species)) |> 
  mutate(species = ifelse(plotID == "Skj_6_3" & species == "Car_vag", "Car_big", species), 
         cover = ifelse(year == 2023 & plotID == "Skj_6_3" & species == "Car_big", 2, cover)) |> 
  filter(!(plotID == "Skj_6_3" & species == "Unknown")) |> 
  distinct()

find_errors_unknowns(community_skj_6_3, "Skj_6_3")


# Skj_6_4
find_errors_unknowns(community_lavisdalen, "Skj_6_4")
# Species cover. 2023 Cam_rot not registered, we estimate it to be 1
# Species cover. 2023 Car_big not registered, we estimate it to be 1 
# Species cover. 2023 Oma_sup not registered, we estimate it to be 1 
# Species cover. 2023 Pyr_sp not registered, we estimate it to be 1
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2022, "Skj_6_4")
# Moss depth. No moss in the missing subplot, we change it to 0
turfplot(community_lavisdalen, "Skj_6_4")
# Car_big_cf and Car_vag are Car_big (after turfmapper check in 2023)
# Sel_sp is Sel_sel

community_skj_6_4 <- community_skj_6_3 |> 
  mutate(cover = ifelse(plotID == "Skj_6_4" & species %in% c("Cam_rot", "Car_big", "Oma_sup", "Pyr_sp"), 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Skj_6_4" & subPlot == 12, 0, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Skj_6_4" & species %in% c("Car_big_cf", "Car_vag"), "Car_big", species), 
         value = ifelse(year == 2021 & plotID == "Skj_6_4" & subPlot == 4 & species == "Car_big", 1, value), 
         cover = ifelse(year == 2019 & plotID == "Skj_6_4" & species == "Car_big", 1, cover), 
         cover = ifelse(year == 2021 & plotID == "Skj_6_4" & species == "Car_big", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_6_4" & species == "Sel_sp", "Sel_sel", species)) |> 
  distinct()

find_errors_unknowns(community_skj_6_4, "Skj_6_4")


# Skj_6_6
find_errors_unknowns(community_lavisdalen, "Skj_6_6")
# Species cover. 2019 Leo_aut was not recorded, we estimate it to be 3
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2022, "Skj_6_6")
# Moss depth. No moss in the missing subplots, we change it to 0
# Subplot number. Sal_her has been shifted one to the left (9 for 10, 11 for 12, 13 for 14), and is missing in 7. Alc_sp 25 is 24. We remove Oma_sup
turfplot(community_lavisdalen, "Skj_6_6")
# Car_vag_CF is Car_vag
# Sel_sp is Sel_sel

skj_6_6_7_2019_sal_her <- community_lavisdalen |>
  filter(year == 2019 & plotID == "Skj_6_6" & subPlot == 7) |>
  mutate(species = "Sal_her", value = "D", cover = "20") |> 
  distinct()

community_skj_6_6 <- community_skj_6_4 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Skj_6_6" & species == "Leo_aut", 3, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Skj_6_6" & subPlot %in% c(10, 12, 26), 0, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 9, 10, subPlot), 
         moss = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 10, 25, moss), 
         litter = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 10, 30, litter), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 10, 60, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 10, 15, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 11, 12, subPlot), 
         moss = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 12, 20, moss), 
         lichen = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 12, 3, lichen), 
         litter = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 12, 50, litter), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 12, 65, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 12, 19, moss_depth_mm)) |> 
  mutate(subPlot = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 13 & species == "Sal_her", 14, subPlot), 
         moss = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 14, 10, moss), 
         lichen = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 14, 25, lichen), 
         litter = ifelse(year == 2019 & plotID == "Skj_6_6" & subPlot == 14, 25, litter)) |> 
  bind_rows(skj_6_6_7_2019_sal_her) |> 
  filter(!(year == 2019 & plotID == "Skj_6_6" & subPlot == 13)) |> 
  mutate(subPlot = ifelse(year == 2022 & plotID == "Skj_6_6" & subPlot == 25, 24, subPlot), 
         moss = ifelse(year == 2022 & plotID == "Skj_6_6" & subPlot == 24, 10, moss), 
         lichen = ifelse(year == 2022 & plotID == "Skj_6_6" & subPlot == 24, 15, lichen), 
         litter = ifelse(year == 2022 & plotID == "Skj_6_6" & subPlot == 24, 10, litter), 
         vegetation_height_mm = ifelse(year == 2022 & plotID == "Skj_6_6" & subPlot == 24, 35, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2022 & plotID == "Skj_6_6" & subPlot == 24, 17, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Skj_6_6" & species == "Car_vag_CF", "Car_vag", species)) |> 
  mutate(species = ifelse(plotID == "Skj_6_6" & species == "Sel_sp", "Sel_sel", species), 
         cover = ifelse(year == 2019 & plotID == "Skj_6_6" & species == "Sel_sel", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_6_6" & species == "Fern", "Gym_dry", species), 
         value = ifelse(plotID == "Skj_6_6" & species == "Gym_dry", 1, value)) |> 
  distinct()

find_errors_unknowns(community_skj_6_6, "Skj_6_6")


# Skj_7_1
find_errors_unknowns(community_lavisdalen, "Skj_7_1")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2018, "Skj_7_1")
# Vegetation height and moss depth mixed up, we fix it
# Subplot number. 2018 Alc_alp 23 is wrong, we remove it
# Subplot number. 2019 Sib_pro also counted in those subplots, but for another study. We remove it
turfplot(community_lavisdalen, "Skj_7_1")
# Value cf. Car_vag is correct
# Value cf. Cer_fon is correct
# Car_cap_cf is Car_cap
# Oma_sp is Oma_sup
# jamne is Sel_sel
# Car_lep is Car_big

community_skj_7_1 <- community_skj_6_6 |> 
  mutate(vegetation_height_mm = ifelse(year == 2018 & plotID == "Skj_7_1" & subPlot == 10, 32, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2018 & plotID == "Skj_7_1" & subPlot == 12, 33, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2018 & plotID == "Skj_7_1" & subPlot == 24, 27, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2018 & plotID == "Skj_7_1" & subPlot == 26, 39, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Skj_7_1" & subPlot == 10, 16, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Skj_7_1" & subPlot == 12, 14, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Skj_7_1" & subPlot == 24, 11, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2018 & plotID == "Skj_7_1" & subPlot == 26, 9, moss_depth_mm)) |> 
  filter(!(year == 2018 & plotID == "Skj_7_1" & subPlot == 23)) |> 
  filter(!(year == 2019 & plotID == "Skj_7_1" & subPlot %in% c(9, 11, 13, 23, 25, 27))) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Skj_7_1" & species == "Car_vag" & subPlot == 21, 1, value)) |> 
  mutate(value = ifelse(year == 2019 & plotID == "Skj_7_1" & species == "Cer_fon" & subPlot == 2, 1, value)) |> 
  mutate(species = ifelse(plotID == "Skj_7_1" & species == "Car_cap_cf", "Car_cap", species)) |> 
  mutate(species = ifelse(plotID == "Skj_7_1" & species == "Oma_sp", "Oma_sup", species)) |> 
  mutate(species = ifelse(plotID == "Skj_7_1" & species == "jamne", "Sel_sel", species), 
         cover = ifelse(year == 2019 & plotID == "Skj_7_1" & species == "Sel_sel", 3, cover)) |> 
  mutate(species = ifelse(plotID == "Skj_7_1" & species == "Car_lep", "Car_big", species)) |> 
  mutate(species = ifelse(plotID == "Skj_7_1" & species == "Jun_tri", "Ave_fle", species)) |> 
  filter(!(plotID == "Skj_7_1" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_7_1, "Skj_7_1")


# Skj_7_2
find_errors_unknowns(community_lavisdalen, "Skj_7_2")
turfplot(community_lavisdalen, "Skj_7_2")
# Agr_mer_CF and Agr_cap are Ant_odo

community_skj_7_2 <- community_skj_7_1 |> 
  mutate(species = ifelse(plotID == "Skj_7_2" & species %in% c("Agr_cap", "Agr_mer_CF"), "Ant_odo", species)) |> 
  mutate(cover = ifelse(year %in% c(2019, 2022) & plotID == "Skj_7_2" & species == "Ant_odo", 4, cover)) |> 
  filter(!(plotID == "Skj_7_2" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_7_2, "Skj_7_2")


# Skj_7_5
find_errors_unknowns(community_lavisdalen, "Skj_7_5")
check_vegetation_cover_height_moss_depth(community_lavisdalen, 2022, "Skj_7_5")
# Moss depth. No moss in the missing subplots, we change it to 0
# Subplot number. 2019 Sib_pro and Ver_alp also counted in those subplots, but for another study. We remove them
turfplot(community_lavisdalen, "Skj_7_5")
# Car_cap is Car_big

community_skj_7_5 <- community_skj_7_2 |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Skj_7_5" & subPlot %in% c(10, 12, 26), 0, moss_depth_mm)) |> 
  filter(!(year == 2019 & plotID == "Skj_7_5" & subPlot %in% c(11, 13, 23, 25, 27))) |> 
  mutate(species = ifelse(plotID == "Skj_7_5" & species == "Car_cap", "Car_big", species)) |> 
  filter(!(plotID == "Skj_7_5" & species == "Nid_seedling"))

find_errors_unknowns(community_skj_7_5, "Skj_7_5")


# 14. We create an object and file only for Skjellingahaugen

community_skjellingahaugen <- community_skj_7_5 |> arrange(year, plotID, subPlot)
community_skjellingahaugen |> filter(site == "Skjellingahaugen") |> write.csv("data_cleaned/Skjellingahaugen.csv")


## Ulvehaugen----

# 15. We work on each plot in turn

community_skjellingahaugen |> filter(site == "Ulvehaugen") |> select(plotID) |> distinct() |> print(n = Inf)
# Ulv_1_1, Ulv_1_3, Ulv_1_4, Ulv_1_5, Ulv_2_1, Ulv_2_2, Ulv_2_3, Ulv_2_4, Ulv_2_5, Ulv_3_2, Ulv_3_3, Ulv_3_4, Ulv_3_5, Ulv_4_1, Ulv_4_3, Ulv_4_4, Ulv_5_1, Ulv_5_3, Ulv_5_4, Ulv_5_5, Ulv_6_1, Ulv_6_2, Ulv_6_3, Ulv_6_4, Ulv_6_5, Ulv_6_6, Ulv_7_2, Ulv_7_3, Ulv_7_4, Ulv_7_6


# Ulv_1_1
find_errors_unknowns(community_skjellingahaugen, "Ulv_1_1")
# Species cover. 2021 Phl_alp was not recorded, we estimate it to be 2
# Subplot number. 2021 Agr_cap 13 was a typo, we remove it
# Subplot number. 2022 Vio_bif 9 is wrong
turfplot(community_skjellingahaugen, "Ulv_1_1")
# Vio_sp is Vio_bif

community_ulv_1_1 <- community_skjellingahaugen |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Ulv_1_1" & species == "Phl_alp", 2, cover)) |> 
  filter(!(year == 2021 & plotID == "Ulv_1_1" & subPlot == 13)) |> 
  filter(!(year == 2022 & plotID == "Ulv_1_1" & species == "Vio_bif" & subPlot == 9)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_1" & species == "Vio_sp", "Vio_bif", species), 
         value = ifelse(year == 2021 & plotID == "Ulv_1_1" & subPlot == 35 & species == "Vio_bif", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Ulv_1_1" & species == "Vio_bif", 7, cover)) |> 
  filter(!(plotID == "Ulv_1_1" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_ulv_1_1, "Ulv_1_1")


# Ulv_1_3
find_errors_unknowns(community_skjellingahaugen, "Ulv_1_3")
# Species value. 2021 Vac_myr. This is the cover of the plot
# Species cover. 2021 some species had a cover a 0.1, we have standardized this to be 1
# Species cover. 2021 Tha_alp and Vac_vit were not recorded, we estimate them to be 1
# Species cover. 2022 Vio_bif was not typed in
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_1_3")
# Vegetation cover was written in the logger column in 2022
turfplot(community_skjellingahaugen, "Ulv_1_3")
# Ave_fle_cf was Ave_fle
# Car_vag_CF was Car_vag
# Hie_sp. There is Hie_pil in the plots next to it
# Tri_sp is Tri_rep
# Luz_spi was Luz_mul

community_ulv_1_3 <- community_ulv_1_1 |> 
  filter(!(year == 2021 & plotID == "Ulv_1_3" & subPlot == 35 & species == "Vac_myr")) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Ulv_1_3" & species == "Vac_myr", 4, cover)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Ulv_1_3" & cover == 0.1, 1, cover)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Ulv_1_3" & species %in% c("Tha_alp", "Vac_vit"), 1, cover)) |> 
  mutate(cover = ifelse(year == 2022 & plotID == "Ulv_1_3" & species == "Vio_bif", 10, cover)) |> 
  mutate(vegetation_cover = ifelse(year == 2022 & plotID == "Ulv_1_3", 100, vegetation_cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_3" & species == "Ave_fle_cf", "Ave_fle", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_3" & species == "Car_vag_CF", "Car_vag", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_3" & species == "Hie_sp", "Hie_pil", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_3" & species == "Tri_sp", "Tri_rep", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_3" & species == "Luz_spi", "Luz_mul", species)) |> 
  filter(!(plotID == "Ulv_1_3" & species == "Nid_seedling"))

find_errors_unknowns(community_ulv_1_3, "Ulv_1_3")


# Ulv_1_4
find_errors_unknowns(community_skjellingahaugen, "Ulv_1_4")
turfplot(community_skjellingahaugen, "Ulv_1_4")
# Alc_sp_cf is Alc_sp
# Cer_Sag_cf is Cer_cer
# Fes_sp is Fes_rub
# Hie_sp. There is Hie_pil in the plots next to it
# Unknown. Not enough information, we remove it
# Oma_nor is Oma_sup
# Poa_pra is Poa_alp
# Vac_myr and Vac_vit are Sal_her

community_ulv_1_4 <- community_ulv_1_3 |> 
  mutate(species = ifelse(plotID == "Ulv_1_4" & species == "Alc_sp_cf", "Alc_sp", species), 
         value = ifelse(year == 2019 & plotID == "Ulv_1_4" & subPlot == 16 & species == "Alc_sp", "1s", value), 
         cover = ifelse(year == 2019 & plotID == "Ulv_1_4" & species == "Alc_sp", 4, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_4" & species == "Cer_Sag_cf", "Cer_cer", species), 
         cover = ifelse(year == 2019 & plotID == "Ulv_1_4" & species == "Cer_cer", 4, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_4" & species == "Fes_sp", "Fes_rub", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_4" & species == "Hie_sp", "Hie_pil", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_4" & species == "Poa_pra", "Poa_alp", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_4" & species %in% c("Vac_myr", "Vac_vit"), "Sal_her", species), 
         cover = ifelse(year == 2019 & plotID == "Ulv_1_4" & species == "Sal_her", 15, cover)) |> 
  filter(!(plotID == "Ulv_1_4" & species == "Unknown")) |> 
  distinct()

find_errors_unknowns(community_ulv_1_4, "Ulv_1_4")


# Ulv_1_5
find_errors_unknowns(community_skjellingahaugen, "Ulv_1_5")
turfplot(community_skjellingahaugen, "Ulv_1_5")
# Cer_alp_cf is Cer_cer
# Epi_sp is Epi_ana
# Vio_sp is Vio_bif

community_ulv_1_5 <- community_ulv_1_4 |> 
  mutate(species = ifelse(plotID == "Ulv_1_5" & species == "Cer_alp_cf", "Cer_cer", species), 
         cover = ifelse(year == 2019 & plotID == "Ulv_1_5" & species == "Cer_cer", 6,cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_5" & species == "Epi_sp", "Epi_ana", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_1_5" & species == "Vio_sp", "Vio_bif", species), 
         value = ifelse(year == 2021 & plotID == "Ulv_1_5" & subPlot %in% c(24, 29) & species == "Vio_bif", "1s", value), 
         cover = ifelse(year == 2021 & plotID == "Ulv_1_5" & species == "Vio_bif", 6, cover)) |> 
  distinct()

find_errors_unknowns(community_ulv_1_5, "Ulv_1_5")


# Ulv_2_1
find_errors_unknowns(community_skjellingahaugen, "Ulv_2_1")
# Subplot number. 2022 Alc_alp is not in the subplots, so it must be OF
# Subplot number. 2023 Nar_str is subplot 28, not 27
turfplot(community_skjellingahaugen, "Ulv_2_1")
# Epi_sp is Epi_ana
# Vio_pal is Vio_bif

community_ulv_2_1 <- community_ulv_1_5 |> 
  mutate(value = ifelse(year == 2022 & plotID == "Ulv_2_1" & subPlot == 9 & species == "Alc_alp", "OF", value)) |> 
  mutate(subPlot = ifelse(year == 2023 & plotID == "Ulv_2_1" & subPlot == 27 , 28, subPlot), 
         moss = ifelse(year == 2023 & plotID == "Ulv_2_1" & subPlot == 28, 20, moss), 
         litter = ifelse(year == 2023 & plotID == "Ulv_2_1" & subPlot == 28, 10, litter)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_1" & species == "Epi_sp", "Epi_ana", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_1" & species == "Vio_pal", "Vio_bif", species), 
         cover = ifelse(year == 2019 & plotID == "Ulv_2_1" & species == "Vio_bif", 6, cover))

find_errors_unknowns(community_ulv_2_1, "Ulv_2_1")


# Ulv_2_2
find_errors_unknowns(community_skjellingahaugen, "Ulv_2_2")
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_2_2")
# Vegetation cover not typed in for 2022
turfplot(community_skjellingahaugen, "Ulv_2_2")
# Leo_aut_cf is Leo_aut
# Car_pal is Car_pil

community_ulv_2_2 <- community_ulv_2_1 |> 
  mutate(vegetation_cover = ifelse(year == 2022 & plotID == "Ulv_2_2", 90, vegetation_cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_2" & species == "Leo_aut_cf", "Leo_aut", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_2" & species == "Car_pal", "Car_pil", species))

find_errors_unknowns(community_ulv_2_2, "Ulv_2_2")


# Ulv_2_3
find_errors_unknowns(community_skjellingahaugen, "Ulv_2_3")
# Species cover. 2022 Alc_sp was not recorded, we estimate it to be 2
turfplot(community_skjellingahaugen, "Ulv_2_3")
# Agr_cap_cf is Agr_cap
# Epi_sp is Epi_ana

community_ulv_2_3 <- community_ulv_2_2 |> 
  mutate(cover = ifelse(year == 2022 & plotID == "Ulv_2_3" & species == "Alc_sp", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_3" & species == "Agr_cap_cf", "Agr_cap", species), 
         cover = ifelse(year == 2019 & plotID == "Ulv_2_3" & species == "Agr_cap", 19, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_3" & species == "Epi_sp", "Epi_ana", species)) |> 
  distinct()

find_errors_unknowns(community_ulv_2_3, "Ulv_2_3")


# Ulv_2_4
find_errors_unknowns(community_skjellingahaugen, "Ulv_2_4")
# Species cover. 2018 Leo_aut was not recorded, we estimate it to be 1
# Species cover. 2019 Phl_alp was not recorded, we estimate it to be 2
# Species cover. 2022 Car_big was not recorded, we estimate it to be 3 (based on previous year's cover)
turfplot(community_skjellingahaugen, "Ulv_2_4")
# Fes_rub_cf_kanskje_Ave_fle is Ave_fle
# Sib_pro_cf is Sib_pro
# Oma_sp is Oma_sup

community_ulv_2_4 <- community_ulv_2_3 |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Ulv_2_4" & species == "Leo_aut", 1, cover)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_2_4" & species == "Phl_alp", 2, cover)) |> 
  mutate(cover = ifelse(year == 2022 & plotID == "Ulv_2_4" & species == "Car_big", 3, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_4" & species == "Fes_rub_cf_kanskje_Ave_fle", "Ave_fle", species), 
         cover = ifelse(year %in% c (2019, 2022) & plotID == "Ulv_2_4" & species == "Ave_fle", 8, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_4" & species == "Sib_pro_cf", "Sib_pro", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_4" & species == "Oma_sp", "Oma_sup", species)) |> 
  distinct()

find_errors_unknowns(community_ulv_2_4, "Ulv_2_4")


# Ulv_2_5
find_errors_unknowns(community_skjellingahaugen, "Ulv_2_5")
# Species value. 2018 Car_big was 2, but for our analysis that is a 1
# Species value. 2022 Ran_acr 3 is cover, the plant was not present in that subplot
# Species cover. 2019 Sib_pro was not recorded, we use the same cover as the year before
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2021, "Ulv_2_5")
# Vegetation height and moss depth mixed up. We fix it
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_2_5")
# Vegetation cover not typed in. Moss not present in the missing subplot, we change it to 0
turfplot(community_skjellingahaugen, "Ulv_2_5")
# Value cf. Alc_sp is correct
# Car_cap_cf is Car_cap
# Leo_aut_cf is Leo_aut
# Lyc_sp and Sel_sp are Sel_sel
# Unknown. Not enough information
# Ver_cha is Ver_alp

community_ulv_2_5 <- community_ulv_2_4 |> 
  mutate(value = ifelse(year == 2018 & plotID == "Ulv_2_5" & subPlot == 21 & species == "Car_big", 1, value)) |> 
  filter(!(year == 2022 & plotID == "Ulv_2_5" & subPlot == 26 & species == "Ran_acr")) |> 
  mutate(cover = ifelse(year == 2022 & plotID == "Ulv_2_5" & species == "Ran_acr", 3, cover)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_2_5" & species == "Sib_pro", 5, cover)) |> 
  mutate(vegetation_height_mm = ifelse(year == 2021 & plotID == "Ulv_2_5" & subPlot == 10, 34, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Ulv_2_5" & subPlot == 12, 79, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Ulv_2_5" & subPlot == 24, 64, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Ulv_2_5" & subPlot == 26, 88, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Ulv_2_5" & subPlot == 10, 9, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Ulv_2_5" & subPlot == 12, 12, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Ulv_2_5" & subPlot == 24, 9, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Ulv_2_5" & subPlot == 26, 9, moss_depth_mm)) |> 
  mutate(vegetation_cover = ifelse(year == 2022 & plotID == "Ulv_2_5", 75, vegetation_cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Ulv_2_5" & subPlot == 26, 0 , moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_5" & species == "Alc_sp_cf", "Alc_sp", species), 
         value = ifelse(year == 2021 & plotID == "Ulv_2_5" & subPlot %in% c(6, 7) & species == "Alc_sp", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Ulv_2_5" & species =="Alc_sp", 30, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_5" & species == "Car_cap_cf", "Car_cap", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_5" & species == "Leo_aut_cf", "Leo_aut", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_5" & species %in% c("Lyc_sp", "Sel_sp"), "Sel_sel", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_2_5" & species == "Ver_cha", "Ver_alp", species), 
         cover = ifelse(year == 2019 & plotID == "Ulv_2_5" & species == "Ver_alp", 2, cover)) |> 
  filter(!(plotID == "Ulv_2_5" & species == "Unknown")) |> 
  distinct()

find_errors_unknowns(community_ulv_2_5, "Ulv_2_5")


# Ulv_3_2
find_errors_unknowns(community_skjellingahaugen, "Ulv_3_2")
# Species cover. 2019 Des_ces not recorded, we estimate it to be 1
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_3_2")
# Vegetation cover written in logger
turfplot(community_skjellingahaugen, "Ulv_3_2")
# Alc_sp_cf is Alc_sp
# Leo_aut_cf is Leo_aut
# Unknown is Dac_vir
# Agr_mer is Agr_cap
# Epi_nor is Epi_ana
# Fes_rub is Ave_fle

community_ulv_3_2 <- community_ulv_2_5 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_3_2" & species == "Des_ces", 1, cover)) |> 
  mutate(vegetation_cover = ifelse(year == 2022 & plotID == "Ulv_3_2", 95, vegetation_cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_2" & species == "Alc_sp_cf", "Alc_sp", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_2" & species == "Leo_aut_cf", "Leo_aut", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_2" & species == "Unknown", "Dac_vir", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_2" & species == "Agr_mer", "Agr_cap", species), 
         cover = ifelse(year == 2019 & plotID == "Ulv_3_2" & species == "Agr_cap", 14, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_2" & species == "Epi_nor", "Epi_ana", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_2" & species == "Fes_rub", "Ave_fle", species)) |> 
  filter(!(plotID == "Ulv_3_2" & species == "Nid_seedling"))

find_errors_unknowns(community_ulv_3_2, "Ulv_3_2")


# Ulv_3_3
find_errors_unknowns(community_skjellingahaugen, "Ulv_3_3")
# Species value. 2022 Ver_alp 1? is correct
# Species cover. 2022 Nar_str not registered, we use the cover from the previous year
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2023, "Ulv_3_3")
# Vegetation cover not recorded
turfplot(community_skjellingahaugen, "Ulv_3_3")
# Car_sp is Car_pal
# Epi_nor is Epi_ana
# Ver_cha is Ver_alp

community_ulv_3_3 <- community_ulv_3_2 |> 
  mutate(value = ifelse(year == 2022 & plotID == "Ulv_3_3" & subPlot == 10 & species == "Ver_alp", 1, value)) |> 
  mutate(cover = ifelse(year == 2022 & plotID == "Ulv_3_3" & species == "Nar_str", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_3" & species == "Car_sp", "Car_pal", species), 
         cover = ifelse(year == 2019 & plotID == "Ulv_3_3" & species == "Car_pal", 2, cover)) |>
  mutate(species = ifelse(plotID == "Ulv_3_3" & species == "Epi_nor", "Epi_ana", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_3" & species == "Ver_cha", "Ver_alp", species)) |> 
  filter(!(plotID == "Ulv_3_3" & species == "Nid_seedling"))

find_errors_unknowns(community_ulv_3_3, "Ulv_3_3")


# Ulv_3_4
find_errors_unknowns(community_skjellingahaugen, "Ulv_3_4")
# Species cover. In 2021 the recorded used 0.5 and 0.1, we standardize this to 1
# Subplot number. In 2021 species were also recorded in subplot 9, we delete them
turfplot(community_skjellingahaugen, "Ulv_3_4")
# Epi_nor is Epi_ana
# Fes_rub is Ave_fle

community_ulv_3_4 <- community_ulv_3_3 |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Ulv_3_4" & cover %in% c(0.1, 0.5), 1, cover)) |> 
  filter(!(year == 2021 & plotID == "Ulv_3_4" & subPlot == 9)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_4" & species == "Epi_nor", "Epi_ana", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_4" & species == "Fes_rub", "Ave_fle", species)) |> 
  filter(!(plotID == "Ulv_3_4" & species == "Nid_seedling"))

find_errors_unknowns(community_ulv_3_4, "Ulv_3_4")


# Ulv_3_5
find_errors_unknowns(community_skjellingahaugen, "Ulv_3_5")
# Species cover. In 2021 the recorded used 0.1, we standardize this to 1
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2021, "Ulv_3_5")
# 2021 Logger in subplot 10, so subplot 17 was measured instead. Subplot 12 was typed in in subplot 14
turfplot(community_skjellingahaugen, "Ulv_3_5")
# Car_sp is Car_big
# Epi_sp  is Epi_ana
# Hie_sp is Hie_pil
# Leo_sp is Leo_aut

community_ulv_3_5 <- community_ulv_3_4 |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Ulv_3_5" & cover == 0.1, 1, cover)) |> 
  mutate(vegetation_height_mm = ifelse(year == 2021 & plotID == "Ulv_3_5" & subPlot == 12, 60, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2021 & plotID == "Ulv_3_5" & subPlot == 14, NA, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Ulv_3_5" & subPlot == 12, 10, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2021 & plotID == "Ulv_3_5" & subPlot == 14, NA, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_5" & species == "Car_sp", "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Ulv_3_5" & species == "Car_big", 6, cover), ) |> 
  mutate(species = ifelse(plotID == "Ulv_3_5" & species == "Epi_sp", "Epi_ana", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_5" & species == "Hie_sp", "Hie_pil", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_3_5" & species == "Leo_sp", "Leo_aut", species))

find_errors_unknowns(community_ulv_3_5, "Ulv_3_5")


# Ulv_4_1
find_errors_unknowns(community_skjellingahaugen, "Ulv_4_1")
# Species cover. 2019 Poa_alp and Sal_her were not recorded, we estimate them to be 1
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2021, "Ulv_4_1")
# 2021 Logger in subplot 24, 31 was recorded instead
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_4_1")
# No moss in the missing subplots, we change it to 0
turfplot(community_skjellingahaugen, "Ulv_4_1")
# Car_big_cf and Car_sp are Car_big
# Vio_sp is Vio_bif
# Cer_alp is Cer_fon
# Rum_ace is Rum_acl
# Unknown. Not enough information

community_ulv_4_1 <- community_ulv_3_5 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_4_1" & species %in% c("Poa_alp", "Sal_her"), 1, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Ulv_4_1" & subPlot %in% c(10, 12, 24, 26), 0 , moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Ulv_4_1" & species %in% c("Car_big_cf", "Car_sp"), "Car_big", species), 
         cover = ifelse(year == 2019 & plotID == "Ulv_4_1" & species == "Car_big", 4, cover), 
         value = ifelse(year == 2021 & plotID == "Ulv_4_1" & species == "Car_big", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Ulv_4_1" & species == "Car_big", 4, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_4_1" & species == "Vio_sp", "Vio_bif", species), 
         value = ifelse(year == 2021 & plotID == "Ulv_4_1" & subPlot == 32 & species == "Vio_bif", "1s", value), 
         cover = ifelse(year == 2021 & plotID == "Ulv_4_1" & species == "Vio_bif", 5, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_4_1" & species == "Cer_alp", "Cer_fon", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_4_1" & species == "Rum_ace", "Rum_acl", species)) |> 
  filter(!(plotID == "Ulv_4_1" & species == "Unknown")) |> 
  distinct()

find_errors_unknowns(community_ulv_4_1, "Ulv_4_1")


# Ulv_4_3
find_errors_unknowns(community_skjellingahaugen, "Ulv_4_3")
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2018, "Ulv_4_3")
# Vegetation cover not recorded, vegetation height for plot 10 was written as vegetation cover
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2021, "Ulv_4_3")
# Vegetation cover not typed in. No moss in the missing subplots, we change it to 0
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_4_3")
# No moss in the missing subplots, we change it to 0
turfplot(community_skjellingahaugen, "Ulv_4_3")
# Car_big_cf and Car_sp are Car_big. In 2021 we estimate the total cover (not realistic that it would be the sum of both)
# Gen_sp is Gen_ama
# Pot_ere is Pot_cra
# Poa_alp is Poa_pra

community_ulv_4_3 <- community_ulv_4_1 |> 
  mutate(vegetation_cover = ifelse(year == 2018 & plotID == "Ulv_4_3", NA, vegetation_cover), 
         vegetation_height_mm = ifelse(year == 2018 & plotID == "Ulv_4_3" & subPlot == 10, 60, vegetation_height_mm)) |> 
  mutate(vegetation_cover = ifelse(year == 2021 & plotID == "Ulv_4_3", 100, vegetation_cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2021 & plotID == "Ulv_4_3" & subPlot %in% c(10, 12), 0 , moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2022 & plotID == "Ulv_4_3" & subPlot %in% c(10, 12, 24, 26), 0 , moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Ulv_4_3" & species %in% c("Car_big_cf", "Car_sp"), "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Ulv_4_3" & species == "Car_big", 17, cover), 
         cover = ifelse(year == 2021 & plotID == "Ulv_4_3" & species == "Car_big", 30, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_4_3" & species == "Gen_sp", "Gen_ama", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_4_3" & species == "Pot_ere", "Pot_cra", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_4_3" & species == "Poa_alp", "Poa_pra", species)) |> 
  filter(!(plotID == "Ulv_4_3" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_ulv_4_3, "Ulv_4_3")


# Ulv_4_4
find_errors_unknowns(community_skjellingahaugen, "Ulv_4_4")
# Species cover. 2019 Poa_alp was not recorded, we use cover from the previous year
# Species cover. 2022 Des_ces was not recorded, we use cover from the previous year
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_4_4")
# Logger in subplot 26, vegetation height not measured. No moss in the missing subplots, we change it to 0
turfplot(community_skjellingahaugen, "Ulv_4_4")
# Car_pil_cf is Car_pil
# Sib_pro_cf is Sib_pro
# Unknown. Not enough information
# Vio_pal is Vio_bif

community_ulv_4_4 <- community_ulv_4_3 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_4_4" & species == "Poa_alp", 1, cover)) |> 
  mutate(cover = ifelse(year == 2022 & plotID == "Ulv_4_4" & species == "Des_ces", 8, cover)) |>  
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Ulv_4_4" & subPlot %in% c(10, 12, 24, 26), 0 , moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Ulv_4_4" & species == "Car_pil_cf", "Car_pil", species), 
         value = ifelse(year == 2021 & plotID == "Ulv_4_4" & species == "Car_pil", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Ulv_4_4" & species == "Car_pil", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_4_4" & species == "Sib_pro_cf", "Sib_pro", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_4_4" & species == "Vio_pal", "Vio_bif", species), 
         cover = ifelse(year == 2022 & plotID == "Ulv_4_4" & species == "Vio_bif", 6, cover)) |> 
  filter(!(plotID == "Ulv_4_4" & species %in% c("Nid_seedling", "Unknown"))) |> 
  distinct()

find_errors_unknowns(community_ulv_4_4, "Ulv_4_4")


# Ulv_5_1
find_errors_unknowns(community_skjellingahaugen, "Ulv_5_1")
# Species cover. 2019 Cam_rot was not recorded, we use cover from the previous year
# Species cover. 2021 Tar_sp was not recorded, we use cover from the previous year
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2019, "Ulv_5_1")
# Vegetation height and moss depth not recorded
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_5_1")
# No moss in the missing subplot, we change it to 0
turfplot(community_skjellingahaugen, "Ulv_5_1")
# Value cf. Rum_ace is correct
# Car_vag_CF is Car_vag
# Eri_uni_cf and Unknown are Eri_uni
# Car_sp in 2018 subplot 14 and Car_big are Car_atr
# Car_sp in 2018 subplot 22 is Car_vag
# Hie_sp and Hie_alp are Hie_vul

community_ulv_5_1 <- community_ulv_4_4 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_5_1" & species == "Cam_rot", 2, cover)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Ulv_5_1" & species == "Tar_sp", 2, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Ulv_5_1" & subPlot == 10, 0 , moss_depth_mm)) |> 
  mutate(value = ifelse(year == 2021 & plotID == "Ulv_5_1" & subPlot %in% c(33, 34) & species == "Rum_ace", "J", value)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_1" & species == "Car_vag_CF", "Car_vag", species), 
         value = ifelse(year == 2021 & plotID == "Ulv_5_1" & subPlot == 15 & species == "Car_vag", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Ulv_5_1" & species == "Car_vag", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_1" & species %in% c("Unknown", "Eri_uni_cf"), "Eri_uni", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_1" & subPlot == 14 & species == "Car_sp", "Car_atr", species), 
         cover = ifelse(year == 2018 & plotID == "Ulv_5_1" & species == "Car_atr", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_1" & species == "Car_big", "Car_atr", species), 
         cover = ifelse(year == 2019 & plotID == "Ulv_5_1" & species == "Car_atr", 2, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_1" & subPlot == 22 & species == "Car_sp", "Car_vag", species), 
         cover = ifelse(year == 2018 & plotID == "Ulv_5_1" & species == "Car_vag", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_1" & species %in% c("Hie_sp", "Hie_alp"), "Hie_vul", species)) |> 
  filter(!(plotID == "Ulv_5_1" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_ulv_5_1, "Ulv_5_1")


# Ulv_5_3
find_errors_unknowns(community_skjellingahaugen, "Ulv_5_3")
turfplot(community_skjellingahaugen, "Ulv_5_3")
# Car_sp and Car_big are Car_vag
# Unknown. Not enough info in 2019, in 2021 the recorder suggests it may have come with the transplants. We remove it
# Rum_acl is Rum_ace

community_ulv_5_3 <- community_ulv_5_1 |> 
  mutate(species = ifelse(plotID == "Ulv_5_3" & species %in% c("Car_sp", "Car_big"), "Car_vag", species), 
         cover = ifelse(year == 2018 & plotID == "Ulv_5_3" & species == "Car_vag", 8, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_3" & species == "Rum_acl", "Rum_ace", species)) |> 
  filter(!(plotID == "Ulv_5_3" & species == "Unknown")) |> 
  distinct()

find_errors_unknowns(community_ulv_5_3, "Ulv_5_3")


# Ulv_5_4
find_errors_unknowns(community_skjellingahaugen, "Ulv_5_4")
# Species cover. 2019 Tar_sp was not recorded, we estimate it to be 1
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2021, "Ulv_5_4")
# Logger in subplot 10, vegetation height and moss depth recorded in subplot 17
turfplot(community_skjellingahaugen, "Ulv_5_4")
# Ant_sp and Ant_dio are Ant_alp
# Car_sp and Car_big are Car_vag
# Jun_sp is Jun_tri

community_ulv_5_4 <- community_ulv_5_3 |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_5_4" & species == "Tar_sp", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_4" & species %in% c("Ant_sp", "Ant_dio"), "Ant_alp", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_4" & species %in% c("Car_big", "Car_sp"), "Car_vag", species), 
         cover = ifelse(year == 2018 & plotID == "Ulv_5_4" & species == "Car_vag", 10, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_4" & species == "Jun_sp", "Jun_tri", species), 
         cover = ifelse(year == 2018 & plotID == "Ulv_5_4" & species == "Jun_tri", 2, cover)) |> 
  distinct()

find_errors_unknowns(community_ulv_5_4, "Ulv_5_4")


# Ulv_5_5
find_errors_unknowns(community_skjellingahaugen, "Ulv_5_5")
# Species cover. 2019 Car_big was not recorded, we estimate it to be 1
# Species cover. 2021 Hyp_mac was not typed in
# Species cover. 2021 Suc_pra was not typed in
turfplot(community_skjellingahaugen, "Ulv_5_5")
# Car_nor_cf is Car_nor
# Fes_rub_cf_kanskje_Ave_fle is Fes_rub
# Sib_pro_cf is Sib_pro
# Car_sp is Car_vag
# Gal_sp is Gal_bor
# Luz_spi is Luz_mul

ulv_5_5_2018_agr_mer <- fix_species_after(community_skjellingahaugen, 2019, "Ulv_5_5", "Agr_mer")
ulv_5_5_2021_agr_mer <- fix_species_after(community_skjellingahaugen, 2022, "Ulv_5_5", "Agr_mer")

community_ulv_5_5 <- community_ulv_5_4 |> 
  bind_rows(ulv_5_5_2018_agr_mer) |> 
  bind_rows(ulv_5_5_2021_agr_mer) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_5_5" & species == "Car_big", 1, cover)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Ulv_5_5" & species == "Hyp_mac", 1, cover)) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Ulv_5_5" & species == "Suc_pra", 18, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_5" & species == "Car_nor_cf", "Car_nor", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_5" & species == "Fes_rub_cf_kanskje_Ave_fle", "Fes_rub", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_5" & species == "Sib_pro_cf", "Sib_pro", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_5" & species == "Car_sp", "Car_vag", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_5" & species == "Gal_sp", "Gal_bor", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_5_5" & species == "Luz_spi", "Luz_mul", species)) |> 
  filter(!(plotID == "Ulv_5_5" & species == "Nid_seedling"))

find_errors_unknowns(community_ulv_5_5, "Ulv_5_5")


# Ulv_6_1
find_errors_unknowns(community_skjellingahaugen, "Ulv_6_1")
turfplot(community_skjellingahaugen, "Ulv_6_1")
# Value cf. Alc_alp is Alc_sp
# Leo_aut_cf is Leo_aut
# Epi_sp is Ver_alp
# Unknown. Not enough information

community_ulv_6_1 <- community_ulv_5_5 |> 
  mutate(value = ifelse(year == 2021 & plotID == "Ulv_6_1" & subPlot == 34 & species =="Alc_alp", "J", value)) |> 
  mutate(species = ifelse(plotID == "Ulv_6_1" & species == "Leo_aut_cf", "Leo_aut", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_6_1" & species == "Epi_sp", "Ver_alp", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_6_1" & species == "Leo_sp", "Leo_aut", species)) |> 
  filter(!(plotID == "Ulv_6_1" & species %in% c("Nid_seedling", "Unknown")))

find_errors_unknowns(community_ulv_6_1, "Ulv_6_1")


# Ulv_6_2
find_errors_unknowns(community_skjellingahaugen, "Ulv_6_2")
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2019, "Ulv_6_2")
# Vegetation cover, vegetation height and moss depth not typed in for 2019
turfplot(community_skjellingahaugen, "Ulv_6_2")
# Value cf. Phl_alp is correct
# Car_sp_smal is Car_big

community_ulv_6_2 <- community_ulv_6_1 |> 
  mutate(vegetation_cover = ifelse(year == 2019 & plotID == "Ulv_6_2", 70, vegetation_cover), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 10, 60, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 12, 60, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 24, 40, vegetation_height_mm), 
         vegetation_height_mm = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 26, 55, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 10, 38, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 12, 22, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 24, 25, moss_depth_mm), 
         moss_depth_mm = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 26, 40, moss_depth_mm), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 1, 80, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 2, 20, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 3, 40, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 4, 20, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 5, 15, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 6, 30, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 7, 90, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 8, 35, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 10, 90, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 12, 30, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 14, 50, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 15, 75, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 16, 50, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 17, 90, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 18, 80, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 19, 80, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 20, 95, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 21, 90, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 22, 90, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 24, 65, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 26, 75, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 28, 60, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 29, 75, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 30, 60, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 31, 80, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 32, 85, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 33, 90, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 34, 40, moss), 
         moss = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 35, 45, moss), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 1, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 2, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 3, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 4, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 5, 25, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 6, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 7, 8, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 8, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 10, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 12, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 14, 25, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 15, 5, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 16, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 17, 45, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 18, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 19, 25, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 20, 50, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 21, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 22, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 24, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 26, 30, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 28, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 29, 15, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 30, 20, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 31, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 32, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 33, 10, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 34, 40, litter), 
         litter = ifelse(year == 2019 & plotID == "Ulv_6_2" & subPlot == 35, 30, litter)) |> 
  mutate(value = ifelse(year == 2021 & plotID == "Ulv_6_2" & subPlot %in% c(6, 7, 28) & species == "Phl_alp", 1, value)) |> 
  mutate(species = ifelse(plotID == "Ulv_6_2" & species == "Car_sp_smal", "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Ulv_6_2" & species =="Car_big", 2, cover)) |> 
  filter(!(plotID == "Ulv_6_2" & species == "Nid_seedling"))

find_errors_unknowns(community_ulv_6_2, "Ulv_6_2")


# Ulv_6_3
find_errors_unknowns(community_skjellingahaugen, "Ulv_6_3")
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2021, "Ulv_6_3")
# Vegetation height not measured at those subplots
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_6_3")
# Logger in subplot 10
turfplot(community_skjellingahaugen, "Ulv_6_3")
# Car_sp is Car_big
# Hie_sp. Not enough information

community_ulv_6_3 <- community_ulv_6_2 |> 
  mutate(species = ifelse(plotID == "Ulv_6_3" & species == "Car_sp", "Car_big", species)) |>
  filter(!(plotID == "Ulv_6_3" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_ulv_6_3, "Ulv_6_3")


# Ulv_6_4
find_errors_unknowns(community_skjellingahaugen, "Ulv_6_4")
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_6_4")
# No moss in the missing subplots, we change it to 0
# Subplot number. 2022 Eup_wet 11 is 10
turfplot(community_skjellingahaugen, "Ulv_6_4")
# Alc_sp_cf is Alc_sp
# Car_nor_cf, Car_sp, Car_sp_smal and Car_cap are Car_big
# Hie_sp. Not enough information, we keep it

community_ulv_6_4 <- community_ulv_6_3 |> 
  mutate(subPlot = ifelse(year == 2022 & plotID == "Ulv_6_4" & subPlot == 11, 10, subPlot), 
         litter = ifelse(year == 2022 & plotID == "Ulv_6_4" & subPlot == 10, 50, litter), 
         vegetation_height_mm = ifelse(year == 2022 & plotID == "Ulv_6_4" & subPlot == 10, 110, vegetation_height_mm)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Ulv_6_4" & subPlot %in% c(10, 12, 24, 26), 0 , moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Ulv_6_4" & species == "Alc_sp_cf", "Alc_sp", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_6_4" & species %in% c("Car_sp_smal", "Car_nor_cf", "Car_sp", "Car_cap"), "Car_big", species), 
         cover = ifelse(year == 2018 & plotID == "Ulv_6_4" & species =="Car_big", 2, cover), 
         cover = ifelse(year == 2021 & plotID == "Ulv_6_4" & species =="Car_big", 1, cover), 
         cover = ifelse(year == 2023 & plotID == "Ulv_6_4" & species =="Car_big", 2, cover)) |> 
  filter(!(plotID == "Ulv_6_4" & species == "Nid_seedling")) |>  
  distinct()

find_errors_unknowns(community_ulv_6_4, "Ulv_6_4")


# Ulv_6_5
find_errors_unknowns(community_skjellingahaugen, "Ulv_6_5")
# Species value. 2021 Bis_viv 6. This is cover, values for the last three subplots were shifted
turfplot(community_skjellingahaugen, "Ulv_6_5")
# Car_nor_cf is Car_big, based on turfmapper in the field
# Leo_aut_cf is Leo_aut

community_ulv_6_5 <- community_ulv_6_4 |> 
  filter(!(year == 2021 & plotID == "Ulv_6_5" & subPlot == 32 & species == "Bis_viv")) |> 
  mutate(value = ifelse(year == 2021 & plotID == "Ulv_6_5" & subPlot == 35 & species == "Bis_viv", 1, value), 
         cover = ifelse(year == 2021 & plotID == "Ulv_6_5" & species == "Bis_viv", 6, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_6_5" & species == "Car_nor_cf", "Car_big", species), 
         cover = ifelse(year == 2021 & plotID == "Ulv_6_5" & species =="Car_big", 4, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_6_5" & species == "Leo_aut_cf", "Leo_aut", species))

find_errors_unknowns(community_ulv_6_5, "Ulv_6_5")


# Ulv_6_6
find_errors_unknowns(community_skjellingahaugen, "Ulv_6_6")
turfplot(community_skjellingahaugen, "Ulv_6_6")
# Car_sp_smal is Car_big
# In the previous script all Agr_cap (82 occurrences) was changed to Agr_mer (1 occurrence). I don't know why it was so, but I don't do it

community_ulv_6_6 <- community_ulv_6_5 |> 
  mutate(species = ifelse(plotID == "Ulv_6_6" & species == "Car_sp_smal", "Car_big", species))

find_errors_unknowns(community_ulv_6_6, "Ulv_6_6")


# Ulv_7_2
find_errors_unknowns(community_skjellingahaugen, "Ulv_7_2")
# Species value. 2021 Car_vag 2. This was cover
# Species cover. 2019 Car_pil, Vio_can and Ver_off were not recorded, we estimate them
# Subplot number. 2022 Biv_viv is not 25, but 24
turfplot(community_skjellingahaugen, "Ulv_7_2")
# Car_big_cf is Car_big
# Fes_rub_cf_kanskje_Ave_fle is Fes_rub
# Hie_sp is Hie_pil
# Cer_alp is Cer_fon

community_ulv_7_2 <- community_ulv_6_6 |> 
  filter(!(year == 2021 & plotID == "Ulv_7_2" & subPlot == 35 & species == "Car_vag")) |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Ulv_7_2" & species == "Car_vag", 2, cover)) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_7_2" & species == "Car_pil", 1, cover)) |> #Adding missing cover
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_7_2" & species == "Ver_off", 1, cover)) |>
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_7_2" & species == "Vio_can", 1, cover)) |> 
  mutate(subPlot = ifelse(year == 2022 & plotID == "Ulv_7_2" & subPlot == 25, 24, subPlot), 
         moss = ifelse(year == 2022 & plotID == "Ulv_7_2" & subPlot == 24, 10, moss), 
         litter = ifelse(year == 2022 & plotID == "Ulv_7_2" & subPlot == 24, 50, litter), 
         vegetation_height_mm = ifelse(year == 2022 & plotID == "Ulv_7_2" & subPlot == 24, 80, vegetation_height_mm), 
         moss_depth_mm = ifelse(year == 2022 & plotID == "Ulv_7_2" & subPlot == 24, 15, moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Ulv_7_2" & species == "Car_big_cf", "Car_big", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_7_2" & species %in% c("Ave_fle", "Fes_rub_cf_kanskje_Ave_fle"), "Fes_rub", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_7_2" & species == "Hie_sp", "Hie_pil", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_7_2" & species == "Cer_alp", "Cer_fon", species), 
         cover = ifelse(year == 2021 & plotID == "Ulv_7_2" & species == "Cer_fon", 3, cover))

find_errors_unknowns(community_ulv_7_2, "Ulv_7_2")


# Ulv_7_3
find_errors_unknowns(community_skjellingahaugen, "Ulv_7_3")
# Species cover not registered in 2018. We have decided to give it the same cover as the year after. Alc_alp is Alc_sp. Fes_ovi was not present in 2019, and only one subplot in 2018, so we estimate the cover to be 1. When making the data long we lost total vegetation cover for this plot in 2018, we add it
# Species cover. 2019 Hyp_mac 0. It was present in the transplant subplots, but not in any of the others
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2019, "Ulv_7_3")
# Vegetation height and moss depth registered in subplot 31 instead of 24 (because of logger)
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_7_3")
# No moss in the missing subplots, we change it to 0
turfplot(community_skjellingahaugen, "Ulv_7_3")
# Fes_rub_cf_kanskje_Ave_fle and Ave_fle are Fes_rub

ulv_7_3_2019_cover <- community_skjellingahaugen |> 
  filter(year == 2019 & 
           plotID == "Ulv_7_3" & 
           species %in% (community_skjellingahaugen |> 
                           filter(year == 2018 & plotID == "Ulv_7_3") |> 
                           select (year, plotID, species) |> 
                           distinct() |> 
                           pull(species))) |> 
  select(year, plotID, species, cover) |> 
  mutate(year = 2018) |> 
  distinct() |> 
  rename(imputed_cover = cover)

community_ulv_7_3_cover <- community_ulv_7_2 |>
  left_join(ulv_7_3_2019_cover, by = c("year", "plotID", "species")) |> 
  mutate(cover = ifelse(year == 2018 & plotID == "Ulv_7_3", imputed_cover, cover)) |> 
  select(-imputed_cover) |>
  mutate(cover = ifelse(year == 2018 & plotID == "Ulv_7_3" & species == "Fes_ovi", 1, cover)) |> 
  mutate(vegetation_cover = ifelse(year == 2018 & plotID == "Ulv_7_3", 55, vegetation_cover))

community_ulv_7_3 <- community_ulv_7_3_cover |> 
  mutate(species = ifelse(plotID == "Ulv_7_3" & species == "Alc_alp", "Alc_sp", species), 
         cover = ifelse(year == 2018 & plotID == "Ulv_7_3" & species == "Alc_sp", 10, cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Ulv_7_3" & subPlot %in% c(24, 26), 0 , moss_depth_mm)) |> 
  mutate(species = ifelse(plotID == "Ulv_7_3" & species %in% c("Ave_fle", "Fes_rub_cf_kanskje_Ave_fle"), "Fes_rub", species))

find_errors_unknowns(community_ulv_7_3, "Ulv_7_3")


# Ulv_7_4
find_errors_unknowns(community_skjellingahaugen, "Ulv_7_4")
# Species cover. 2019 Ach_mil was not present in the plot, but it seems the cell in the csv file is not empty. We remove it
# Species cover. 2019 Tha_alp was not recorded, we estimate it to be 1
turfplot(community_skjellingahaugen, "Ulv_7_4")
# Fes_rub_cf_kanskje_Ave_fle, Ave_fle and Fes_ovi are Fes_rub
# Sil_aca_cif is Sil_aca
# Vio_can_cf is Vio_bif

community_ulv_7_4 <- community_ulv_7_3 |> 
  filter(!(year == 2019 & plotID == "Ulv_7_4" & species == "Ach_mil")) |> 
  mutate(cover = ifelse(year == 2019 & plotID == "Ulv_7_4" & species == "Tha_alp", 1, cover)) |> 
  mutate(species = ifelse(plotID == "Ulv_7_4" & species %in% c("Ave_fle", "Fes_rub_cf_kanskje_Ave_fle", "Fes_ovi"), "Fes_rub", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_7_4" & species == "Sil_aca_cf", "Sil_aca", species)) |> 
  mutate(species = ifelse(plotID == "Ulv_7_4" & species == "Vio_can_cf", "Vio_bif", species), 
         value = ifelse(year == 2023 & plotID == "Ulv_7_4" & subPlot == 30 & species == "Vio_bif", "1j", value), 
         cover = ifelse(year == 2023 & plotID == "Ulv_7_4" & species == "Vio_bif", 7, cover)) |> 
  filter(!(plotID == "Ulv_7_4" & species == "Nid_seedling")) |> 
  distinct()

find_errors_unknowns(community_ulv_7_4, "Ulv_7_4")


# Ulv_7_6
find_errors_unknowns(community_skjellingahaugen, "Ulv_7_6")
# Species cover. 2021 Cer_fon cover not typed in
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2021, "Ulv_7_6")
# Vegetation cover not typed in
check_vegetation_cover_height_moss_depth(community_skjellingahaugen, 2022, "Ulv_7_6")
# No moss in the missing subplot, we change it to 0
turfplot(community_skjellingahaugen, "Ulv_7_6")

community_ulv_7_6 <- community_ulv_7_4 |> 
  mutate(cover = ifelse(year == 2021 & plotID == "Ulv_7_6" & species == "Cer_fon", 1, cover)) |> 
  mutate(vegetation_cover = ifelse(year == 2021 & plotID == "Ulv_7_6", 90, vegetation_cover)) |> 
  mutate(moss_depth_mm = ifelse(year == 2022 & plotID == "Ulv_7_6" & subPlot == 10, 0 , moss_depth_mm)) |> 
  filter(!(plotID == "Ulv_7_6" & species == "Nid_seedling"))

find_errors_unknowns(community_ulv_7_6, "Ulv_7_6")


# 16. We create an object and file only for Ulvehaugen

community_ulvehaugen <- community_ulv_7_6 |> arrange(year, plotID, subPlot)
community_ulvehaugen |> filter(site == "Ulvehaugen") |> write.csv("data_cleaned/Ulvehaugen.csv")


## Adding some few extra columns with information----

# Making new variables were different signs in the dataset give the same value based on which category it goes under
# In theory Sibbaldia procumbens (Sib_pro) and Veronica alpina (Ver_alp) should have registered dominance in all plots at Skjellingahaugen every year. Which means that 1 = 0-25% cover in the subplot, 2 = 25-50%, 3 = 50-75% and 4 = 75-100%. However, this may not always have been the case. We know that plots where there are 2, 3 and 4s the 1s represent dominance level, we include it in the code

community_presence <- community_ulvehaugen |> 
  mutate(presence = str_detect(value, "(?i)[1234odsjf]"), 
         presence = case_when(presence == "TRUE" ~ 1), 
         fertile = str_detect(value, "(?i)[f]"), 
         juvenile = str_detect(value, "(?i)[j]"), 
         seedling = str_detect(value, "(?i)[s]")) |> 
  group_by(year, plotID) |> 
  mutate(dominance = if_else(str_detect(value, "(?i)[d]"), value, 
                             ifelse(site == "Skjellingahaugen" & species %in% c("Sib_pro", "Ver_alp") & any(str_detect(value, "[234]")), value, NA))) |> 
  ungroup() |> 
  relocate(c(presence, fertile, juvenile, seedling, dominance), .after = value) |> 
  mutate(subPlot = as.factor(subPlot), 
         cover = as.numeric(cover))


# Establishing the different functional groups

community_functional <- community_presence |> 
  mutate(functional_group = case_when(
    species %in% c("Bet_nan", "Bet_pub", "Dry_oct", "Sal_her", "Sal_lan", "Vac_myr", "Vac_uli") ~ "Deciduous_shrubs", 
    species %in% c("Cal_vul", "Emp_nig", "Pin_syl", "Vac_vit") ~ "Evergreen_shrubs", 
    species %in% c("Ach_mil", "Aco_sep", "Alc_alp", "Alc_sp", "Ane_nem", "Ant_alp", "Ant_dio", "Ast_alp", "Bar_alp", "Bis_viv", "Bot_lun", "Cam_rot", "Cer_alp", "Cer_cer",  "Cer_fon", "Dac_vir", "Dip_alp", "Epi_ana", "Equ_arv", "Equ_sci", "Eri_bor", "Eri_uni", "Eup_wet", "Fern", "Gal_bor", "Gal_sp", "Gen_ama", "Gen_cam", "Gen_niv", "Ger_syl", "Geu_riv", "Gym_dry", "Hie_alp", "Hie_pil", "Hie_sp",  "Hie_vul", "Hup_sel", "Hyp_mac", "Leo_aut", "Leu_vul", "Lot_cor", "Lys_eur", "Oma_nor", "Oma_sup", "Ort_sec", "Oxa_ace", "Oxy_dig", "Par_pal", "Phe_con", "Pin_vul", "Pot_cra", "Pot_ere", "Pru_vul", "Pyr_sp", "Ran_acr", "Ran_aur", "Ran_pyg", "Rhi_min", "Rum_ace", "Rum_acl",  "Sag_sag", "Sau_alp", "Sel_sel", "Sib_pro", "Sil_aca", "Sil_vul", "Sol_vir", "Ste_gra", "Suc_pra", "Tar_sp", "Tha_alp", "Tof_pus", "Tri_pra", "Tri_rep", "Vah_atr", "Ver_alp", "Ver_off", "Vio_bif", "Vio_can", "Vio_pal", "Vio_tri") ~ "Forbs", 
    species %in% c("Agr_cap", "Agr_mer", "Ant_odo", "Ave_fle", "Car_atr", "Car_big", "Car_cap", "Car_fla", "Car_lac", "Car_nor", "Car_pal", "Car_pil", "Car_sax", "Car_vag",  "Des_alp", "Des_ces", "Fes_ovi", "Fes_rub", "Fes_viv", "Jun_tri", "Luz_mul", "Luz_spi", "Nar_str", "Phl_alp", "Poa_alp", "Poa_pra", "Tri_ces", "Tri_spi") ~ "Graminoids"
  )) |> 
  relocate(functional_group, .before = species) |> 
  group_by(year, plotID) |> 
  mutate(recorder = paste(unique(recorder), collapse = " & "), 
         writer = paste(unique(writer), collapse = " & ")) |> 
  ungroup() |> 
  group_by(plotID, year) |>
  mutate(date = min(date)) #If data collection was done over several days pick the first date


# Calculating cover per plot of non-vascular material (we don't take into account presence of temperature logger)

total_cover <- community_functional |> 
  select(year, plotID, subPlot, measure, moss, lichen, litter, bare_ground, rock, poo, fungus) |> 
  filter(measure == "subPlot") |> 
  distinct() |>  
  group_by(year, plotID) |> 
  mutate(moss = ifelse(moss == 0, NA_real_, moss), 
         litter = ifelse(litter == 0, NA_real_, litter), 
         lichen = ifelse(lichen == 0, NA_real_, lichen), 
         bare_ground = ifelse(bare_ground == 0, NA_real_, bare_ground), 
         rock = ifelse(rock == 0, NA_real_, rock), 
         poo = ifelse(poo == 0, NA_real_, poo), 
         fungus = ifelse(fungus == 0, NA_real_, fungus)) |> 
  mutate(total_moss_cover = ifelse(year == 2022, sum(moss, na.rm = TRUE) / 9, sum(moss, na.rm = TRUE) / 29), 
         total_litter_cover = ifelse(year == 2022, sum(litter, na.rm = TRUE) / 9, sum(litter, na.rm = TRUE) / 29), 
         total_lichen_cover = ifelse(year == 2022, sum(lichen, na.rm = TRUE) / 9, sum(lichen, na.rm = TRUE) / 29), 
         total_bare_ground_cover = ifelse(year == 2022, sum(bare_ground, na.rm = TRUE) / 9, sum(bare_ground, na.rm = TRUE) / 29), 
         total_poo_cover = ifelse(year == 2022, sum(poo, na.rm = TRUE) / 9, sum(poo, na.rm = TRUE) / 29), 
         total_rock_cover = ifelse(year == 2022, sum(rock, na.rm = TRUE) / 9, sum(rock, na.rm = TRUE) / 29), 
         total_fungus_cover = ifelse(year == 2022, sum(fungus, na.rm = TRUE) / 9, sum(fungus, na.rm = TRUE) / 29)) |> 
  mutate(total_moss_cover = case_when(is.na(total_moss_cover) ~ 0, 
                                      total_moss_cover == 0 ~ 0, 
                                      total_moss_cover < 1 ~ 1, 
                                      total_moss_cover > 0 ~ round(total_moss_cover, digits = 0)), 
         total_litter_cover = case_when(is.na(total_litter_cover) ~ 0, 
                                        total_litter_cover == 0 ~ 0, 
                                        total_litter_cover < 1 ~ 1, 
                                        total_litter_cover > 0 ~ round(total_litter_cover, digits = 0)), 
         total_lichen_cover = case_when( is.na(total_lichen_cover) ~ 0, 
                                         total_lichen_cover == 0 ~ 0, 
                                         total_lichen_cover < 1 ~ 1, 
                                         total_lichen_cover > 0 ~ round(total_lichen_cover, digits = 0)), 
         total_bare_ground_cover = case_when(is.na(total_bare_ground_cover) ~ 0, 
                                             total_bare_ground_cover == 0 ~ 0, 
                                             total_bare_ground_cover < 1 ~ 1, 
                                             total_bare_ground_cover > 0 ~ round(total_bare_ground_cover, digits = 0)), 
         total_poo_cover = case_when(is.na(total_poo_cover) ~ 0, 
                                     total_poo_cover == 0 ~ 0, 
                                     total_poo_cover < 1 ~ 1, 
                                     total_poo_cover > 0 ~ round(total_poo_cover, digits = 0)), 
         total_rock_cover = case_when(is.na(total_rock_cover) ~ 0, 
                                      total_rock_cover == 0 ~ 0, 
                                      total_rock_cover < 1 ~ 1, 
                                      total_rock_cover > 0 ~ round(total_rock_cover, digits = 0)), 
         total_fungus_cover = case_when(is.na(total_fungus_cover) ~ 0, 
                                        total_fungus_cover == 0 ~ 0, 
                                        total_fungus_cover < 1 ~ 1, 
                                        total_fungus_cover > 0 ~ round(total_fungus_cover, digits = 0))) |> 
  select( -c(measure, moss, lichen, litter, bare_ground, rock, poo, fungus))


# Calculating mean vegetation height and moss depth per plot

vegetation_height_and_moss_depth_mean <- community_functional |> 
  select(year, plotID, subPlot, vegetation_height_mm, moss_depth_mm) |> 
  distinct() |>
  group_by(year, plotID) |> 
  mutate(vegetation_height_mean = mean(vegetation_height_mm, na.rm = TRUE), 
         moss_depth_mean = mean(moss_depth_mm, na.rm = TRUE)) |> 
  select(-c(vegetation_height_mm, moss_depth_mm, subPlot)) |> 
  mutate(vegetation_height_mean = round(vegetation_height_mean, digits = 0)) |> 
  mutate(moss_depth_mean = round(moss_depth_mean, digits = 0)) |> 
  ungroup() |> 
  distinct() |> 
  mutate(vegetation_height_mean = ifelse(is.na(vegetation_height_mean), NA_real_, vegetation_height_mean), 
         moss_depth_mean = ifelse(is.na(moss_depth_mean), NA_real_, moss_depth_mean))


## Creating the clean files----

community_clean <- community_functional |> 
  left_join(total_cover, by = c("subPlot","plotID", "year")) |> 
  left_join(vegetation_height_and_moss_depth_mean, by = c("year", "plotID")) |> 
  relocate(c(total_moss_cover, total_lichen_cover, total_litter_cover, total_bare_ground_cover, total_poo_cover, total_rock_cover, total_fungus_cover, vegetation_height_mean, moss_depth_mean), .after = moss_depth_mm) |> 
  filter(subPlot != 9)

# Making the 3 final datasets that are cleaned and put out in OSF

community_clean_subplot <- community_clean |> 
  filter(measure == "subPlot") |> 
  select(year:subPlot, functional_group:logger, vegetation_height_mm, moss_depth_mm, recorder, writer, date)

community_clean_species_cover <- community_clean |> 
  select(year:treatment, functional_group, species, cover, recorder, writer, date) |> 
  distinct()

community_clean_plotlevel_info <- community_clean |> 
  select(year:treatment, vegetation_cover, total_moss_cover:moss_depth_mean, recorder, writer, date) |> 
  distinct() |>
  pivot_longer(cols = vegetation_cover:moss_depth_mean, names_to = "name", values_to = "value")

# Saving the datasets

ifelse(!dir.exists("data_cleaned"), dir.create("data_cleaned"), FALSE)
write.csv(community_clean_species_cover, file = "data_cleaned/INCLINE_community_species_cover.csv", row.names= FALSE)
write.csv(community_clean_subplot, file = "data_cleaned/INCLINE_community_subplot.csv", row.names= FALSE)
write.csv(community_clean_plotlevel_info, file = "data_cleaned/INCLINE_community_plotlevel_info.csv", row.names= FALSE)

## Notes----

# We have removed Car_nig, Lyc_alp and Ver_cha from the dataset
