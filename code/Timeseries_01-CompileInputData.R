## Timeseries_01-CompileInputData.R
# This script collects all input data for time series analysis into a single file.

source(file.path("code", "paths+packages.R"))

## load data
df_met <- readr::read_csv(file.path("data", "Meteorology_Daily_WithETo.csv")) %>% 
  dplyr::rename(date_ghcn = date)

df_gw <- readr::read_csv(file.path("data", "Groundwater_WaterLevels-DailyGHCN.csv"))

df_stream <- readr::read_csv(file.path("data", "Streamflow+Stage_DailyGHCN.csv"))

## combine into single data frame
df_all <- 
  dplyr::left_join(df_met, df_stream, by = "date_ghcn") %>% 
  dplyr::left_join(df_gw, by = "date_ghcn")

## met data goes back to 1904... start data 1 year before earliest streamflow measurement
date_min <- min(df_stream$date_ghcn)

## trim
df_out <- 
  df_all %>% 
  subset(date_ghcn >= (date_min - years(1)))

## save output
readr::write_csv(df_out, file.path("data", "Timeseries_InputData.csv"))
