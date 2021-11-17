## Timeseries_05-CompareFullWetDryModels.R

source(file.path("code", "paths+packages.R"))

## read timeseries model output
df_full <- read_csv(file.path("data", "Timeseries_Output_FullModel.csv")) %>% 
  set_names(c("date", "obs", "resid")) %>% 
  mutate(sim = obs - resid,
         model = "Full")

df_wet <- read_csv(file.path("data", "Timeseries_Output_WetModel.csv")) %>% 
  set_names(c("date", "obs", "resid")) %>% 
  mutate(sim = obs - resid,
         model = "Wet")

df_dry <- read_csv(file.path("data", "Timeseries_Output_DryModel.csv")) %>% 
  set_names(c("date", "obs", "resid")) %>% 
  mutate(sim = obs - resid,
         model = "Dry")

# combine wet and dry models
df_combo <- 
  bind_rows(df_wet, df_dry) %>% 
  arrange(date)

## read underlying time series model data
df_in <- file.path("data", "Timeseries_InputData.csv") %>% 
  readr::read_csv(col_types = "Ddddddddddcdcdc")

## date to start plotting
date_start <- ymd(first_date) #ymd("2002-10-01")
date_end <- max(df_in$date_ghcn[is.finite(df_in$LWPH4b)]) #last_date #ymd("2021-09-30")

df_plot <- subset(df_in, date_ghcn >= date_start & date_ghcn <= date_end)

## set up regime shifts
df_regimes <- read_csv(file.path("data", "RegimeShifts_rstars.csv"))
df_regime_dates <- subset(df_regimes, ts_regime_shift != 0)
df_regimes_startend <- tibble(
  date_start = c(min(df_regimes$date_mid), df_regime_dates$date_mid),
  date_end = c(df_regime_dates$date_mid, max(df_regimes$date_mid)),
  regime_category = c("Wet", "Dry", "Wet", "Dry", "Wet")
) %>% 
  mutate(WaterYear_start = decimal_date(date_start + days(92)),
         WaterYear_end = decimal_date(date_end + days(92)))

# last regime extends beyond the end of groundwater data - adjust end date for plotting
df_regimes_startend$date_end[length(df_regimes_startend$date_end)] <- date_end


## plot
ggplot() +
  geom_rect(data = df_regimes_startend, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf, 
                                            fill = regime_category), alpha = 0.25) +
  geom_point(data = df_full, aes(x = date, y = obs)) +
  geom_line(data = df_full, aes(x = date, y = sim), color = "red") +
  geom_line(data = df_combo, aes(x = date, y = sim), color = "blue") +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu)) +
  theme(legend.position = "bottom")

## fit statistics
hydroGOF::KGE(df_full$sim, df_full$obs)
hydroGOF::KGE(df_combo$sim, df_combo$obs)
