## Meteorology-Pawnee_01-Download.R
# This set of code mirrors the Larned meteorological data collection, but for the Pawnee River watershed.

source(file.path("code", "paths+packages.R"))

## relevant NOAA GHCN-D stations
# - USC00144087 JETMORE 8 NNW, KS US: 1965-10-01 to 2022-01-31
# - USC00144081 JETMORE, KS US: 1900-12-01 to 1985-05-31
# - USC00144085 JETMORE 14 NW, KS US: 1949-05-01 to 1965-09-14
# - USC00141141 BURDETT 3 S, KS US: 1941-02-01 to 2022-01-26
# - USC00144161 KALVESTA 1 W, KS US: 1963-09-01 to 2021-12-31 (98%)
# - USC00142497 KALVESTA 13 NW, KS US: 1971-07-01 to 1989-01-31 (81%)
# - USC00144166 KALVESTA 13 NW, KS US: 1989-03-01 to 2021-12-31 (96%)
# - USC00144159 KALVESTA NEAR, KS US: 1935-08-01 to 1943-02-28 (99%)
# - USC00140620 BAZINE 13 SSW, KS US: 1948-08-01 to 2004-02-29 (31%)
# - USC00144531 LARNED NUMBER 2, KS US: 2009-06-30 to 2020-12-28, 8.35 km
# - USC00144530 LARNED, KS US: 1903-05-31 to 2008-07-30, 8.82 km

## set station priority (1 = top priority)
station_priority <-
  tibble::tibble(StationName = c("JETMORE 8 NNW", "JETMORE", "JETMORE 14 NW", "BURDETT 3 S",
                                 "KALVESTA 1 W", "KALVESTA 13 NW", "KALVESTA 13 NW", "KALVESTA NEAR", 
                                 "BAZINE 13 SSW", "LARNED NUMBER 2", "LARNED"), 
                 Station = c("USC00144087", "USC00144081", "USC00144085", "USC00141141",
                             "USC00144161", "USC00142497", "USC00144166", "USC00144159", 
                             "USC00140620", "USC00144531", "USC00144530"),
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

# inspect completeness
df %>% 
  group_by(date) %>% 
  summarize(prcp_data = sum(is.finite(prcp_mm)),
            tmax_data = sum(is.finite(tmax_c)),
            tmin_data = sum(is.finite(tmin_c))) %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~name, ncol = 1)

# save the raw data
write_csv(df, file.path("data", "Meteorology-Pawnee_Daily_Raw.csv"))
