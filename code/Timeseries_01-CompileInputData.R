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

## add regime classification
df_regimes <- readr::read_csv(file.path("data", "RegimeShifts_rstars.csv"))
df_regime_dates <- subset(df_regimes, ts_regime_shift != 0)
df_regimes_startend <- tibble(
  date_start = c(min(df_regimes$date_mid), df_regime_dates$date_mid),
  date_end = c(df_regime_dates$date_mid, max(df_regimes$date_mid)),
  regime_category = c("Wet", "Dry", "Wet", "Dry", "Wet")
) %>% 
  mutate(WaterYear_start = decimal_date(date_start + days(92)),
         WaterYear_end = decimal_date(date_end + days(92)))

df_all$regime_number <- NA
df_all$regime_category <- "Unknown"
n_regimes <- length(df_regimes_startend$date_start)
for (r in 1:n_regimes){
  if (r == 1){
    i_r_start <- which(df_all$date_ghcn == first_date)
  } else {
    i_r_start <- which(df_all$date_ghcn == df_regimes_startend$date_start[r])
  }
  
  if (r == n_regimes){
    i_r_end <- length(df_all$date_ghcn)
  } else {
    i_r_end <- which(df_all$date_ghcn == df_regimes_startend$date_end[r])
  }
  
  df_all$regime_number[i_r_start:i_r_end] <- r
  df_all$regime_category[i_r_start:i_r_end] <- df_regimes_startend$regime_category[r]
  
}

## met data goes back to 1904... start data before earliest streamflow measurement
yrs_buffer <- 10
date_min <- min(df_stream$date_ghcn)

## trim
df_out <- 
  df_all %>% 
  subset(date_ghcn >= (date_min - years(yrs_buffer)))

## save output
readr::write_csv(df_out, file.path("data", "Timeseries_InputData.csv"))
