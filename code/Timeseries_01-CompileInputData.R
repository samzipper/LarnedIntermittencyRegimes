## Timeseries_01-CompileInputData.R
# This script collects all input data for time series analysis into a single file.

source(file.path("code", "paths+packages.R"))

## load data
df_met <- readr::read_csv(file.path("data", "Meteorology_Daily_WithETo.csv")) %>% 
  dplyr::rename(date_ghcn = date)

df_gw <- readr::read_csv(file.path("data", "Groundwater_WaterLevels-DailyGHCN-LWPH4-GapFill.csv")) %>% 
  dplyr::select(date_ghcn, LWPH4a, LWPH4a_source, LWPH4b, LWPH4b_source, LWPH4c, LWPH4c_source)

df_stream <- readr::read_csv(file.path("data", "Streamflow+Stage_DailyGHCN.csv"))

df_wuse <- readr::read_csv(file.path("data", "WaterUse_Daily.csv"))

## combine into single data frame
df_all <- 
  dplyr::left_join(df_met, df_stream, by = "date_ghcn") %>% 
  dplyr::left_join(df_wuse, by = "date_ghcn") %>% 
  dplyr::left_join(df_gw, by = "date_ghcn")

## met data goes back to 1904... start data before earliest streamflow measurement
yrs_buffer <- 5
date_min <- min(df_stream$date_ghcn)

## trim
df_out <- 
  df_all %>% 
  subset(date_ghcn >= (date_min - years(yrs_buffer)))

## save output
readr::write_csv(df_out, file.path("data", "Timeseries_InputData.csv"))
