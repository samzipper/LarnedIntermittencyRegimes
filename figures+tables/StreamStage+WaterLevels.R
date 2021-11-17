## StreamStage+GroundwaterLevels.R

source(file.path("code", "paths+packages.R"))

## load data
df_Q_day <- read_csv(file.path("data", "Streamflow+Stage_DailyGHCN.csv"))
df_GW_day <- readr::read_csv(file.path("data", "Groundwater_WaterLevels-DailyGHCN-LWPH4-GapFill.csv")) %>% 
  dplyr::select(date_ghcn, LWPH4a, LWPH4a_source, LWPH4b, LWPH4b_source, LWPH4c, LWPH4c_source)

df_dry_periods <- readr::read_csv(file.path("data", "Streamflow_DryPeriods.csv"))

## combine
df_Q.GW_day <-
  left_join(df_Q_day, df_GW_day, by = "date_ghcn") %>% 
  dplyr::select(date_ghcn, stage_masl, LWPH4b, LWPH4c) %>% 
  pivot_longer(-date_ghcn, names_to = "Site", values_to = "level_masl") %>% 
  mutate(site_factor = factor(Site, levels = c("stage_masl", "LWPH4b", "LWPH4c"), 
                              labels = c("River", "Alluvial Aq.", "High Plains Aq")))

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

## plot
p_levels <-
  ggplot() +
  geom_rect(data = df_regimes_startend, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf, 
                                            fill = regime_category), alpha = 0.25) +
  geom_line(data = df_Q.GW_day, aes(x = date_ghcn, y = level_masl, color = site_factor)) +
  scale_x_date(name = "Date", expand = c(0,0), date_labels = "%Y") +
  scale_y_continuous(name = "Water Level [masl]") +
  scale_color_manual(name = NULL, values = c(col.cat.blu, col.cat.org, col.gray)) +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu)) +
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "StreamStage+GroundwaterLevels.png"),
       p_levels, width = 160, height = 80, units = "mm")
