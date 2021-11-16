## Timeseries_InputData.R
# This plots input data for time series model.

source(file.path("code", "paths+packages.R"))

## read data
df_in <- file.path("data", "Timeseries_InputData.csv") %>% 
  readr::read_csv(col_types = "Ddddddddddcdcdc")

## date to start plotting
date_start <- ymd("2003-10-01")
date_end <- ymd("2021-09-30")

df_plot <- subset(df_in, date_ghcn >= date_start & date_ghcn <= date_end)

## plot - facet
df_hline <- tibble::tibble(name = c("defc_mm", "WaterUse_1e4m3d", "stage_masl", "LWPH4a"),
                           intercept = c(0, 0, 593.23, 593.23)) %>% 
  dplyr::mutate(name_factor = factor(name, levels = c("defc_mm", "WaterUse_1e4m3d", "stage_masl", "LWPH4a"), 
                                     labels = c("Climatic Water Balance (Precip - ETo) [mm]", 
                                                "Pumping [x10\u2074 m\u00b3/d]", 
                                                "River Stage [masl]", 
                                                "Alluvial Aquifer Head [masl]")))

df_plot %>% 
  dplyr::mutate(defc_mm = prcp_mm - ETo_mm,
                WaterUse_1e4m3d = WaterUse_m3d/1e4) %>% 
  dplyr::select(date_ghcn, defc_mm, WaterUse_1e4m3d, stage_masl, LWPH4a) %>% 
  tidyr::pivot_longer(-date_ghcn) %>% 
  dplyr::mutate(name_factor = factor(name, levels = c("defc_mm", "WaterUse_1e4m3d", "stage_masl", "LWPH4a"), 
                                     labels = c("Climatic Water Balance (Precip - ETo) [mm]", 
                                                "Pumping [x10\u2074 m\u00b3/d]", 
                                                "River Stage [masl]", 
                                                "Alluvial Aquifer Head [masl]"))) %>% 
  ggplot(aes(x = date_ghcn, y = value)) +
  geom_line(color = col.cat.blu) +
  geom_hline(data = df_hline, aes(yintercept = intercept), color = col.gray) +
  facet_wrap(~name_factor, ncol = 1, scales = "free_y") +
  scale_x_date(name = "Date [daily data]", expand = c(0,0)) +
  scale_y_continuous(name = NULL)

ggsave(file.path("figures+tables", "Timeseries_InputData.png"),
       width = 130, height = 130, units = "mm")
