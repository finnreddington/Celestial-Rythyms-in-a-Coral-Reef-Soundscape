# Kimbe Island data preparation ----
library(tidyverse)
library(lubridate)
library(ggplot2)
library(lunar)

# Load and wrangle indices ----
df_indices <- read_csv("Data/indices.csv")
df_indices <- df_indices %>% 
  select(-'...1') %>% 
  rename("filename" = minute)

df_indices <- df_indices %>% 
  mutate(filename = str_remove(filename, "//.wav$"))

df_indices <- df_indices %>%
  mutate("site" = filename) %>% 
  separate(site, into = c("HM", "datetime"), sep = "_", extra = "merge", fill = "right")

sum(is.na(df_indices$datetime))

df_indices %>% 
  group_by(HM) %>% 
  summarise(n()) 

## Rename site ----
df_indices <- df_indices %>% 
  mutate(HM = str_replace(HM, "^HH", "HM_"))

df_indices <- df_indices %>%
  mutate(location = HM) %>% 
  mutate(location = case_when(
    HM == "HM_01" ~ "SE_slope",
    HM == "HM_02" ~ "lagoon_A",
    HM == "HM_03" ~ "NE_slope",
    HM == "HM_04" ~ "lagoon_C",
    HM == "HM_05" ~ "NW_slope",
    HM == "HM_06" ~ "lagoon_D",
    HM == "HM_07" ~ "W_slope_lagoon_E",
    HM == "HM_08" ~ "lagoon_E",
    HM == "HM_09" ~ "E_slope",
    HM == "HM_10" ~ "S_slope",
    HM == "HM_11" ~ "SW_slope",
    HM == "HM_12" ~ "W_slope",
    TRUE ~ HM
  ))

df_indices$location <- as_factor(df_indices$location)


df_indices <- df_indices %>%
  mutate(habitat = HM) %>% 
  mutate(habitat = case_when(
    HM == "HM_01" ~ "slope",
    HM == "HM_02" ~ "lagoon",
    HM == "HM_03" ~ "slope",
    HM == "HM_04" ~ "lagoon",
    HM == "HM_05" ~ "slope",
    HM == "HM_06" ~ "lagoon",
    HM == "HM_07" ~ "slope",
    HM == "HM_08" ~ "lagoon",
    HM == "HM_09" ~ "slope",
    HM == "HM_10" ~ "slope",
    HM == "HM_11" ~ "slope",
    HM == "HM_12" ~ "slope",
    TRUE ~ HM
  ))

df_indices$habitat <- as_factor(df_indices$habitat)

df_indices <- df_indices %>%
  mutate(rec_time = case_when(
    HM %in% c("HM_03", "HM_05", "HM_09") ~ "early",
    TRUE ~ "full"
  ))

df_indices$rec_time <- as_factor(df_indices$rec_time)

df_indices %>% 
  group_by(rec_time) %>%  
  summarise(n())

## Format the datetime col ----
df_indices <- df_indices %>% 
  mutate(datetime_posix = ymd_hms(datetime, tz="Pacific/Port_Moresby")) %>% 
  select(filename, HM, location, habitat, rec_time, datetime, datetime_posix, everything())

## Removing unwanted variables ----
cols_to_remove <- grep("_std", names(df_indices)) # remove std dev variables
df_indices <- df_indices[-cols_to_remove]
df_indices <- df_indices %>% select(-c(shrimp_Hf, fish_Hf))# remove Hf 
df_indices <- df_indices %>% select(-c(fish_ADI, shrimp_ADI))# remove ADI as collinear

# rename indices to be less suggestive
df_indices <- df_indices %>% # 'fish' becomes 'low_freq'
  rename_with(~ gsub("^fish", "low_freq", .x), starts_with("fish")) 

df_indices <- df_indices %>% # 'shrimp' becomes 'high_freq'
  rename_with(~ gsub("^shrimp", "high_freq", .x), starts_with("shrimp"))

rm(cols_to_remove)

# Dates ----
# Includes deployment sound that needs trimming
df_indices %>% 
  group_by(location) %>% 
  reframe('start' = min(datetime_posix),
          'end' = max(datetime_posix)) %>% 
  write_csv("kimbe_dates.csv")

cutoff_datetime <- as.POSIXct("2023-04-23T00:00:00", tz="Pacific/Port_Moresby")

df_indices <- df_indices %>%
  filter(datetime_posix >= cutoff_datetime)

min(df_indices$datetime_posix)

# trim the last 1-2 days
max(df_indices$datetime_posix)
cutoff_datetime <- as.POSIXct("2023-06-26T00:00:00", tz="Pacific/Port_Moresby")

df_indices <-
  df_indices %>%
  filter(datetime_posix <= cutoff_datetime)

df_summary <- df_indices %>% 
  filter(habitat=="slope") %>% 
  group_by(location) %>% 
  reframe('start' = min(datetime_posix),
          'end' = max(datetime_posix)) 

## Plot a Gant----
ggplot(df_summary, aes(x = start, xend = end, y = location, yend = location)) +
  geom_segment(size = 5, color = "blue") +
  labs(title = "Recording Periods by Site",
       x = "Date",
       y = "Site") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Lunar data ----
df_indices <- df_indices %>% 
  mutate(
    date = as.Date(datetime_posix),
    lunar_phase4 = lunar.phase(date, shift = -10, name = 4), 
    lunar_phase8 = lunar.phase(date, shift = -10, name = 8),
    lunar_phase_rad = lunar.phase(date, shift = -10),
    lunar_illum = lunar.illumination(date, shift = -10)
  ) %>% 
  select(filename, HM, location, habitat, 
         rec_time, datetime, datetime_posix, date,
         lunar_phase4, lunar_phase8, lunar_phase_rad, lunar_illum,
         everything())

rm(list = setdiff(ls(), "df_indices"))


