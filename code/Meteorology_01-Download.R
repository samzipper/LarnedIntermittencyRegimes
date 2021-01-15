## Data_Meteorology_01-Download.R

source(file.path("code", "paths+packages.R"))

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

# save the raw data
write_csv(df, file.path("data", "Meteorology_Daily_Raw.csv"))
