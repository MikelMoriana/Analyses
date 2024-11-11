## Libraries and files----

library(tidyverse)
library(rstatix)

# Microclimate 2019-2021
dataDownloader::get_file(node = "zhk3m",
                         file = "INCLINE_microclimate.zip",
                         path = "data_cleaned",
                         remote_path = "Climate")

# Soil moisture 2022
dataDownloader::get_file(node = "zhk3m",
                         file = "INCLINE_soil-moisture_2022.csv",
                         path = "raw_data",
                         remote_path = "Climate")


soil_m_download <- read.csv("data_cleaned/INCLINE_microclimate_soil_moisture.csv")
air_t_download <-   read_csv("data_cleaned/INCLINE_microclimate_air_temperature.csv")
ground_t_download <-   read_csv("data_cleaned/INCLINE_microclimate_ground_temperature.csv")
soil_t_download <-   read_csv("data_cleaned/INCLINE_microclimate_soil_temperature.csv")

plotlv <- read.csv("data_cleaned/INCLINE_community_plotlevel_info.csv")
plotlv_date <- plotlv |> 
  select(year, site, date) |> 
  mutate(date = ymd(date), 
         site = factor(site, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) |> 
  rename(fieldwork = date) |> 
  group_by(year, site) |> 
  slice(which.min(fieldwork))
  


## Soil moisture----

soil_m_19_21 <- soil_m_download |> 
  rename(site = siteID) |> 
  mutate(site = factor(site, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) |> 
  mutate(date = str_sub(datetime, 1, 10), 
         date = ymd(date)) |> 
  mutate(year = str_sub(datetime, 1, 4), 
         year = as.numeric(year)) |> 
  mutate(month = str_sub(datetime, 6, 7)) |> 
  mutate(block = str_sub(plotID, 1, 5),
         block = as.factor(block)) |> 
  mutate(soil_moisture = as.numeric(soil_moisture))

soil_m_field <- soil_m_19_21 |> 
  filter(year != 2020) |> 
  filter(treatment != "R") |> 
  filter(!is.na(soil_moisture)) |> 
  inner_join(plotlv_date, by = c("year", "site")) |> 
  filter(date < fieldwork)

hist(soil_m_field$soil_moisture)
anova(aov(soil_moisture ~ site, soil_m_field))
aov(soil_moisture ~ site, soil_m_field) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = soil_m_field, aes(x = site, y = soil_moisture)) + 
  facet_grid(treatment ~ OTC)

model_test <- glmmTMB::glmmTMB(soil_moisture ~ site * OTC * treatment + ar1(as.factor(date)-1|block) + (1|block), family = beta_family(link = "logit"), data = soil_m_field)
summary(model_test)


# Daily

soil_m_field_day <- soil_m_field |> 
  group_by(date, site, block, plotID, OTC, treatment) |> 
  summarise(soil_moisture = mean(soil_moisture))

hist(soil_m_field_day$soil_moisture)
anova(aov(soil_moisture ~ site, soil_m_field_day))
aov(soil_moisture ~ site, soil_m_field_day) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = soil_m_field_day, aes(x = site, y = soil_moisture))


# Monthly

soil_m_field_month <- soil_m_field |> 
  group_by(year, month, site, block, plotID, OTC, treatment) |> 
  summarise(soil_moisture = mean(soil_moisture))

hist(soil_m_field_month$soil_moisture)
anova(aov(soil_moisture ~ site, soil_m_field_month))
aov(soil_moisture ~ site, soil_m_field_month) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = soil_m_field_month, aes(x = site, y = soil_moisture))


# Yearly

soil_m_field_year <- soil_m_field |> 
  group_by(year, site, block, plotID, OTC, treatment) |> 
  summarise(soil_moisture = mean(soil_moisture))

hist(soil_m_field_year$soil_moisture)
anova(aov(soil_moisture ~ site, soil_m_field_year))
aov(soil_moisture ~ site, soil_m_field_year) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = soil_m_field_year, aes(x = site, y = soil_moisture))



## Air temperature----

air_t_19_21 <- air_t_download |> 
  rename(site = siteID) |> 
  mutate(site = factor(site, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) |> 
  mutate(date = str_sub(datetime, 1, 10), 
         date = ymd(date)) |> 
  mutate(year = str_sub(datetime, 1, 4), 
         year = as.numeric(year)) |> 
  mutate(month = str_sub(datetime, 6, 7)) |> 
  mutate(block = str_sub(plotID, 1, 5),
         block = as.factor(block)) |> 
  mutate(air_temperature = as.numeric(air_temperature))

air_t_field <- air_t_19_21 |> 
  filter(year != 2020) |> 
  filter(treatment != "R") |> 
  filter(!is.na(air_temperature)) |> 
  inner_join(plotlv_date, by = c("year", "site")) |> 
  filter(date < fieldwork)

hist(air_t_field$air_temperature)
anova(aov(air_temperature ~ site, air_t_field))
aov(air_temperature ~ site, air_t_field) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_year, aes(x = site, y = air_temperature)) + 
  facet_grid(~OTC)


# Daily

air_t_field_day <- air_t_field |> 
  group_by(date, site, block, plotID, OTC, treatment) |> 
  summarise(air_temperature = mean(air_temperature))

hist(air_t_field_day$air_temperature)
anova(aov(air_temperature ~ site, air_t_field_day))
aov(air_temperature ~ site, air_t_field_day) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_day, aes(x = site, y = air_temperature))


# Monthly

air_t_field_month <- air_t_field |> 
  group_by(year, month, site, block, plotID, OTC, treatment) |> 
  summarise(air_temperature = mean(air_temperature))

hist(air_t_field_month$air_temperature)
anova(aov(air_temperature ~ site, air_t_field_month))
aov(air_temperature ~ site, air_t_field_month) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_month, aes(x = site, y = air_temperature))


# Yearly

air_t_field_year <- air_t_field |> 
  group_by(year, site, block, plotID, OTC, treatment) |> 
  summarise(air_temperature = mean(air_temperature))

hist(air_t_field_year$air_temperature)
anova(aov(air_temperature ~ site, air_t_field_year))
aov(air_temperature ~ site, air_t_field_year) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_year, aes(x = site, y = air_temperature))



## Ground temperature----

ground_t_19_21 <- ground_t_download |> 
  rename(site = siteID) |> 
  mutate(site = factor(site, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) |> 
  mutate(date = str_sub(datetime, 1, 10), 
         date = ymd(date)) |> 
  mutate(year = str_sub(datetime, 1, 4), 
         year = as.numeric(year)) |> 
  mutate(month = str_sub(datetime, 6, 7)) |> 
  mutate(block = str_sub(plotID, 1, 5),
         block = as.factor(block)) |> 
  mutate(ground_temperature = as.numeric(ground_temperature))

ground_t_field <- ground_t_19_21 |> 
  filter(year != 2020) |> 
  filter(treatment != "R") |> 
  filter(!is.na(ground_temperature)) |> 
  inner_join(plotlv_date, by = c("year", "site")) |> 
  filter(date < fieldwork)

hist(ground_t_field$ground_temperature)
anova(aov(ground_temperature ~ site, ground_t_field))
aov(ground_temperature ~ site, ground_t_field) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_year, aes(x = site, y = ground_temperature))


# Daily

ground_t_field_day <- ground_t_field |> 
  group_by(date, site, block, plotID, OTC, treatment) |> 
  summarise(ground_temperature = mean(ground_temperature))

hist(ground_t_field_day$ground_temperature)
anova(aov(ground_temperature ~ site, ground_t_field_day))
aov(ground_temperature ~ site, ground_t_field_day) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_year, aes(x = site, y = ground_temperature))


# Monthly

ground_t_field_month <- ground_t_field |> 
  group_by(year, month, site, block, plotID, OTC, treatment) |> 
  summarise(ground_temperature = mean(ground_temperature))

hist(ground_t_field_month$ground_temperature)
anova(aov(ground_temperature ~ site, ground_t_field_month))
aov(ground_temperature ~ site, ground_t_field_month) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_year, aes(x = site, y = ground_temperature))


# Yearly

ground_t_field_year <- ground_t_field |> 
  group_by(year, site, block, plotID, OTC, treatment) |> 
  summarise(ground_temperature = mean(ground_temperature))

hist(ground_t_field_year$ground_temperature)
anova(aov(ground_temperature ~ site, ground_t_field_year))
aov(ground_temperature ~ site, ground_t_field_year) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_year, aes(x = site, y = ground_temperature))



## Soil temperature----

soil_t_19_21 <- soil_t_download |> 
  rename(site = siteID) |> 
  mutate(site = factor(site, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) |> 
  mutate(date = str_sub(datetime, 1, 10), 
         date = ymd(date)) |> 
  mutate(year = str_sub(datetime, 1, 4), 
         year = as.numeric(year)) |> 
  mutate(month = str_sub(datetime, 6, 7)) |> 
  mutate(block = str_sub(plotID, 1, 5),
         block = as.factor(block)) |> 
  mutate(soil_temperature = as.numeric(soil_temperature))

soil_t_field <- soil_t_19_21 |> 
  filter(year != 2020) |> 
  filter(treatment != "R") |> 
  filter(!is.na(soil_temperature)) |> 
  inner_join(plotlv_date, by = c("year", "site")) |> 
  filter(date < fieldwork)

hist(soil_t_field$soil_temperature)
anova(aov(soil_temperature ~ site, soil_t_field))
aov(soil_temperature ~ site, soil_t_field) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_year, aes(x = site, y = soil_temperature))


# Daily

soil_t_field_day <- soil_t_field |> 
  group_by(date, site, block, plotID, OTC, treatment) |> 
  summarise(soil_temperature = mean(soil_temperature))

hist(soil_t_field_day$soil_temperature)
anova(aov(soil_temperature ~ site, soil_t_field_day))
aov(soil_temperature ~ site, soil_t_field_day) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_year, aes(x = site, y = soil_temperature))


# Monthly

soil_t_field_month <- soil_t_field |> 
  group_by(year, month, site, block, plotID, OTC, treatment) |> 
  summarise(soil_temperature = mean(soil_temperature))

hist(soil_t_field_month$soil_temperature)
anova(aov(soil_temperature ~ site, soil_t_field_month))
aov(soil_temperature ~ site, soil_t_field_month) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_year, aes(x = site, y = soil_temperature))


# Yearly

soil_t_field_year <- soil_t_field |> 
  group_by(year, site, block, plotID, OTC, treatment) |> 
  summarise(soil_temperature = mean(soil_temperature))

hist(soil_t_field_year$soil_temperature)
anova(aov(soil_temperature ~ site, soil_t_field_year))
aov(soil_temperature ~ site, soil_t_field_year) |> tukey_hsd()

ggplot() + 
  geom_boxplot(data = air_t_field_year, aes(x = site, y = soil_temperature))




