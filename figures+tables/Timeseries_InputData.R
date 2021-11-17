## Timeseries_InputData.R
# This plots input data for time series model.

source(file.path("code", "paths+packages.R"))

## read data
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

## plot - facet
df_hline <- tibble::tibble(name = c("defc_mm", "WaterUse_1e4m3d", "stage_masl", "LWPH4b"),
                           intercept = c(0, 0, 593.23, 593.23)) %>% 
  dplyr::mutate(name_factor = factor(name, levels = c("defc_mm", "WaterUse_1e4m3d", "stage_masl", "LWPH4b"), 
                                     labels = c("Climatic Water Balance (Precip - ETo) [mm]", 
                                                "Pumping [x10\u2074 m\u00b3/d]", 
                                                "River Stage [masl]", 
                                                "Alluvial Aquifer Head [masl]")))

df_plot %>% 
  dplyr::mutate(defc_mm = prcp_mm - ETo_mm,
                WaterUse_1e4m3d = WaterUse_m3d/1e4) %>% 
  dplyr::select(date_ghcn, defc_mm, WaterUse_1e4m3d, stage_masl, LWPH4b) %>% 
  tidyr::pivot_longer(-date_ghcn) %>% 
  dplyr::mutate(name_factor = factor(name, levels = c("defc_mm", "WaterUse_1e4m3d", "stage_masl", "LWPH4b"), 
                                     labels = c("Climatic Water Balance (Precip - ETo) [mm]", 
                                                "Pumping [x10\u2074 m\u00b3/d]", 
                                                "River Stage [masl]", 
                                                "Alluvial Aquifer Head [masl]"))) %>% 
  ggplot() +
  geom_rect(data = df_regimes_startend, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf, 
                                            fill = regime_category), alpha = 0.25) +
  geom_line(aes(x = date_ghcn, y = value)) +
  geom_hline(data = df_hline, aes(yintercept = intercept), color = col.gray) +
  facet_wrap(~name_factor, ncol = 1, scales = "free_y") +
  scale_x_date(name = "Date [daily data]", expand = c(0,0)) +
  scale_y_continuous(name = NULL) +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu)) +
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "Timeseries_InputData.png"),
       width = 130, height = 150, units = "mm")
