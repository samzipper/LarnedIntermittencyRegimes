## Meteorology_03-Update.R
# This script is supposed to take the existing cleaned meteorological dataset and update it to 'last_date'.
# It does not need to be run if you have just completed Meteorology_01 and Meteorology_02 up to the last_date.
#
# Note: you will need to delete all cached rnoaa files that were created from previous data downloads,
#  otherwise R will just try to use those instead of downloading updated data.
#  Delete these manually from the cache location 
#  You can determine the cache location using rnoaa:::lcd_cache$cache_path_get()
#  For my work desktop, it is C:\Users\samzipper\AppData\Local\cache\R\noaa_ghcnd

source(file.path("code", "paths+packages.R"))

## load current clean data
df_clean_old <- read_csv(file.path("data", "Meteorology_Daily_Clean.csv"), lazy = F)
df_raw_old <- read_csv(file.path("data", "Meteorology_Daily_Raw.csv"), lazy = F)
next_date <- max(df_raw_old$date) + days(1)  # day that we need new met data to start

## relevant NOAA GHCN-D stations
# two closest are Larned:
# - USC00144531 LARNED NUMBER 2, KS US: 2009-06-30 to 2020-12-28, 8.35 km
# - USC00144530 LARNED, KS US: 1903-05-31 to 2008-07-30, 8.82 km
# some close but old ones
# - USC00144932 MACKSVILLE 8 NNE, KS US: 1893-03-31 to 1977-03-30, 16.82 km
# - USC00149121 MACKSVILLE 8 NNE, KS US: 1941-01-31 to 1971-08-30, 16.82 km
# - USC00142993 GARFIELD, KS US: 1893-12-31 to 1900-05-30, 25.46 km
# upstream in the pawnee watershed:
# - USC00147192 SANFORD 1 NW, KS US: 2005-08-31 to 2015-10-30, 28.92 km
# - USC00147021 ROZEL NEAR, KS US: 1940-08-01 to 1942-04-30, 34.7 km
# long-term but upstream or out of watershed:
# - USC00143218 GREAT BEND 3 W, KS US: 1909-03-31 to 2020-12-28, 23.30 km
# - USC00143847 HUDSON, KS US: 1922-04-07 to 2020-11-29, 31.64 km
# - USC00148245 TROUSDALE 1 NE, KS US: 1916-07-31 to 2020-09-29, 42.52 km
# - USC00140865 BISON 3 NW, KS US: 1916-07-31 to 2020-09-29, 42.52 km
# other upstream stations for older data
# - USC00140119 ALBERT 5 SE, KS US: 2001-08-31 to 2020-12-17, 21.29 km
# - USC00140117 ALBERT, KS US: 1959-07-31 to 1994-12-30, 28.73 km
# - USC00143679 HOISINGTON NEAR, KS US: 1934-08-31 to 1948-10-15, 31.33 km

## set station priority (1 = top priority)
station_priority <-
  tibble::tibble(StationName = c("LARNED NUMBER 2", "LARNED", 
                                 "MACKSVILLE 8 NNE", "MACKSVILLE 8 NNE", "GARFIELD",
                                 "SANFORD 1 NW", "ROZEL",
                                 "GREAT BEND 3 W", "HUDSON", 
                                 "ALBERT 5 SE", "ALBERT", "HOISINGTON",
                                 "TROUSDALE 1 NE", "BISON 3 NW"),
                 Station = c("USC00144531", "USC00144530", 
                             "USC00144932", "USC00149121", "USC00142993",
                             "USC00147192", "USC00147021",
                             "USC00143218", "USC00143847", 
                             "USC00140119", "USC00140117", "USC00143679",
                             "USC00148245", "USC00140865"),
                 Priority = seq(1, length(StationName)))

## get and combine data
for (i in 1:length(station_priority$Station)){
  # download raw data
  df_station <- rnoaa::meteo_tidy_ghcnd(stationid = station_priority$Station[i], 
                                        var = c("PRCP", "TMAX", "TMIN"),
                                        date_min = next_date,
                                        date_max = last_date)
  
  # combine
  if (i == 1){
    df_raw <- df_station
  } else {
    df_raw <- dplyr::bind_rows(df_raw, df_station)
  }
}

# rename and add station priority
df <- 
  df_raw %>% 
  dplyr::rename(Station = id) %>% 
  # set units
  dplyr::mutate(prcp_mm = prcp/10, 
                tmax_c = tmax/10,
                tmin_c = tmin/10) %>% 
  dplyr::select(Station, date, prcp_mm, tmax_c, tmin_c) %>% 
  dplyr::left_join(station_priority, by = "Station")

# add to previous df_raw data frame
df_raw_new <- 
  dplyr::bind_rows(df_raw_old, df) %>% 
  arrange(Priority, date)

# save the raw data, overwriting the previous file
write_csv(df_raw_new, file.path("data", "Meteorology_Daily_Raw.csv"))

## now, clean the new data and add to the clean data frame
df_trimmed <-
  df %>% 
  tidyr::pivot_longer(cols = c("prcp_mm", "tmax_c", "tmin_c"),
                      names_to = "variable") %>% 
  subset(complete.cases(.)) %>% 
  dplyr::group_by(date, variable) %>% 
  dplyr::filter(Priority == min(Priority)) %>% 
  dplyr::ungroup()

# separate for each variable to get a single station and value for each
df_prcp <- 
  subset(df_trimmed, variable == "prcp_mm") %>% 
  dplyr::select(date, Station, value) %>% 
  dplyr::rename(prcp_mm = value, station_prcp = Station) %>% 
  # add in gaps for missing dates
  dplyr::right_join(tibble::tibble(date = seq(min(df$date), max(df$date), by = 1)), by = "date")

df_tmax <- 
  subset(df_trimmed, variable == "tmax_c") %>% 
  dplyr::select(date, Station, value) %>% 
  dplyr::rename(tmax_c = value, station_tmax = Station) %>% 
  dplyr::right_join(tibble::tibble(date = seq(min(df$date), max(df$date), by = 1)), by = "date")

df_tmin <- 
  subset(df_trimmed, variable == "tmin_c") %>% 
  dplyr::select(date, Station, value) %>% 
  dplyr::rename(tmin_c = value, station_tmin = Station) %>% 
  dplyr::right_join(tibble::tibble(date = seq(min(df$date), max(df$date), by = 1)), by = "date")

# join them all back together again, by date
df_all <-
  dplyr::left_join(df_prcp, df_tmax, by = "date") %>% 
  dplyr::left_join(df_tmin, by = "date") %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate(Year = year(date),
                Month = month(date))

# check for completeness
rnoaa::vis_miss(df_all)
df_all$date[is.na(df_all$prcp_mm)]
df_all$date[is.na(df_all$tmax_c)]
df_all$date[is.na(df_all$tmin_c)]

# note: if there are missing data in a future update, copy over the gap-filling
#       routine from Meteorology_02-Clean.R

# add to old data frame
df_clean_new <-
  dplyr::bind_rows(df_clean_old, df_all) %>% 
  dplyr::mutate(WaterYear = year(date + days(92))) %>% 
  dplyr::arrange(date) %>% 
  dplyr::select(-Year, -Month)

# inspect
ggplot(subset(df_clean_new, WaterYear >= 2020), aes(x = date, y = tmax_c)) + geom_line()
ggplot(subset(df_clean_new, WaterYear >= 2020), aes(x = date, y = tmin_c)) + geom_line()
ggplot(subset(df_clean_new, WaterYear >= 2020), aes(x = date, y = prcp_mm)) + geom_line()

# write updated file
write_csv(df_clean_new, file.path("data", "Meteorology_Daily_Clean.csv"))
