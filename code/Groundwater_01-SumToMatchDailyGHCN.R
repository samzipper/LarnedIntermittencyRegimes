## Groundwater_01-SumToMatchDailyGHCN.R
# This script is intended to load raw groundwater level data and 
# aggregate it to a daily average at the same time resolution as
# the NOAA GHCN dataset for the area.
#
# The raw data are on the geohydrology snap server (\\GHFILES\Common\Larned Research Site\data)


## prep workspace
source(file.path("code", "paths+packages.R"))

## which wells to summarize?
wells_all <- c("LWPH4a", "LWPH4b", "LWPH4c")

## what time should a day start/end?
# use 0700 AM local time to match the GHCN-D daily dataset
# in other words, the value for a day should reflect 7 AM the previous day to 7 AM the day of interest
hr_start <- 7

## load data
for (w in wells_all){
  # load raw subdaily data
  df_w <- readr::read_csv(file.path(dir_data, "monitoring_wells", "processed", paste0(w, "_Subdaily.csv")))
  
  # identify the day matching ghcn data
  df_w$hr <- lubridate::hour(df_w$datetime)
  df_w$date_ghcn <- lubridate::date(df_w$datetime)
  df_w$date_ghcn[df_w$hr >= hr_start] <- df_w$date_ghcn[df_w$hr >= hr_start] + days(1)
  
  # summarize to mean by date
  df_w_day <-
    df_w %>% 
    dplyr::group_by(date_ghcn) %>% 
    dplyr::summarize(waterlevel_m = round(mean(waterlevel_m, na.rm = T), 4))
  
  # combine output
  if (w == wells_all[1]){
    df_day <- df_w_day
    colnames(df_day)[colnames(df_day)=="waterlevel_m"] <- w
  } else {
    df_day <- dplyr::left_join(df_day, df_w_day, by = "date_ghcn")
    colnames(df_day)[colnames(df_day)=="waterlevel_m"] <- w
  }
  
}

# save
readr::write_csv(df_day, file.path("data", "Groundwater_WaterLevels-DailyGHCN.csv"))

# plot
df_day %>% 
  tidyr::pivot_longer(-date_ghcn, names_to = "well", values_to = "waterlevel_m") %>% 
  ggplot(aes(x = date_ghcn, y = waterlevel_m, color = well)) +
  geom_line()
