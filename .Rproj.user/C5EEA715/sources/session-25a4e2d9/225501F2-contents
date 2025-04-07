# Kimbe Current data

# Load data ----
load('Data/CurrentMeters.RData')
df_currents <- dat
rm(dat)

# first filter out 6m and 12m depths and keep 9m only:
df_currents2 <- 
  df_currents %>% 
  filter(depth == 9) %>% 
  mutate(HM = str_replace(hydromoth, "^HH", "HM_")) %>% 
  mutate(HM = case_when(
    HM == "HM_1" ~ "HM_01",
    HM == "HM_2" ~ "HM_02",
    HM == "HM_3" ~ "HM_03",
    HM == "HM_4" ~ "HM_04",
    HM == "HM_5" ~ "HM_05",
    HM == "HM_6" ~ "HM_06",
    HM == "HM_7" ~ "HM_07",
    HM == "HM_8" ~ "HM_08",
    HM == "HM_9" ~ "HM_09",
    HM == "HM_10" ~ "HM_10",
    HM == "HM_11" ~ "HM_11",
    TRUE ~ HM 
  )) %>% 
  select(-hydromoth) %>% 
  mutate(datetime_posix = as.POSIXct(date, format='%Y-%m-%d %H:%M:%OS', tz="Pacific/Port_Moresby"))
  
# loses HM_03 and first and last min at 00.00 for others (4,912 rows lost)
full_data <- df_indices %>% inner_join(df_currents2, by = c("HM", "datetime_posix"))

# clean up a little:
colnames(full_data)
sum(!is.na(full_data$notes)) # all na
sum(!is.na(full_data$use)) # all na
setequal(full_data$site, full_data$location) # not the same
unique(full_data$site)
unique(full_data$location) # different naminf conventions for the slopes.

full_data <- 
  full_data %>% 
  select(-habitat.y) %>% 
  rename('habitat'= habitat.x) %>% 
  rename(speed = speed..m.s.) %>% 
  rename(speed_upper = speed.upper..m.s.) %>% 
  rename(tilt = tilt..radians.) %>% 
  rename(battery = `batt..volts.`) %>% 
  rename(direction = `direction..radians.CCW.from.East.`) %>% 
  select(-c(notes, use, site, datetime, date.x, date.y, `speed.lower..m.s.`))

# Check how many recordings there are for the 4 non-lagoon sites that recorded for the whole duration:
subdata <- full_data %>% 
  filter(habitat == "slope") %>% 
  filter(! location %in% c("E_slope", "NW_slope")) 

# save the final dataframe ----
rm(list=setdiff(ls(), "full_data"))
save(full_data, file = "Data/full_data.RData", version = 2)


