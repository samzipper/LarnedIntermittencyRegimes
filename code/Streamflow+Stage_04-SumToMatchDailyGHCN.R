## Streamflow+Stage_04-SumToMatchDailyGHCN.R
# This script is intended to load raw stream stage data and 
# aggregate it to a daily average at the same time resolution as
# the NOAA GHCN dataset for the area.
#
# The raw data are on the geohydrology snap server (\\GHFILES\Common\Larned Research Site\data)

## prep workspace
source(file.path("code", "paths+packages.R"))

## what time should a day start/end?
# use 0700 AM local time to match the GHCN-D daily dataset
# in other words, the value for a day should reflect 7 AM the previous day to 7 AM the day of interest
hr_start <- 7

## load data
df_w <- readr::read_csv(file.path(dir_data, "streamflow_stage", "processed", "Streamflow+Stage_Inst_Clean.csv"))

# identify the day matching ghcn data
df_w$hr <- lubridate::hour(df_w$datetime_local)
df_w$date_ghcn <- 
  lubridate::date(df_w$datetime_local)
df_w$date_ghcn[df_w$hr >= hr_start] <- df_w$date_ghcn[df_w$hr >= hr_start] + days(1)
df_w <- subset(df_w, date_ghcn <= ymd(last_date))

# replace nodata flag with NAs
nodata_value <- -9999
df_w$stage_masl[df_w$stage_masl == nodata_value] <- NA

# summarize to mean by date
df_day <-
  df_w %>% 
  dplyr::group_by(date_ghcn) %>% 
  dplyr::summarize(discharge_cms = round(mean(discharge_cms, na.rm = T), 4),
                   stage_masl = round(mean(stage_masl, na.rm = T), 4)) %>% 
  dplyr::mutate(WaterYear = year(date_ghcn + days(92)))

# save
readr::write_csv(df_day, file.path("data", "Streamflow+Stage_DailyGHCN.csv"))

# plot
df_day %>% 
  tidyr::pivot_longer(-date_ghcn, names_to = "variable") %>% 
  ggplot(aes(x = date_ghcn, y = value)) +
  geom_line() +
  facet_wrap(~variable, scale = "free_y", ncol = 1)

ggplot(df_day, aes(x = stage_masl, y = discharge_cms, color = discharge_cms <= 0)) +
  geom_point() +
  geom_vline(xintercept = 593.45, color = "blue")
