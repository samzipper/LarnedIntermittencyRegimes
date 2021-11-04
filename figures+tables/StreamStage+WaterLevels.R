## StreamStage+GroundwaterLevels.R

source(file.path("code", "paths+packages.R"))

## load data
df_Q_day <- read_csv(file.path("data", "Streamflow+Stage_DailyGHCN.csv"))
df_GW_day <- read_csv(file.path("data", "Groundwater_WaterLevels-DailyGHCN.csv"))

df_dry_periods <- readr::read_csv(file.path("data", "Streamflow_DryPeriods.csv"))

## combine
df_Q.GW_day <-
  left_join(df_Q_day, df_GW_day, by = "date_ghcn") %>% 
  dplyr::select(date_ghcn, stage_masl, LWPH4b, LWPH4c) %>% 
  pivot_longer(-date_ghcn, names_to = "Site", values_to = "level_masl") %>% 
  mutate(site_factor = factor(Site, levels = c("stage_masl", "LWPH4b", "LWPH4c"), 
                              labels = c("River", "Alluvial Aq.", "High Plains Aq")))

## plot
p_levels <-
  ggplot() +
  geom_rect(data = df_dry_periods, aes(xmin = first_noflow_date, xmax = last_noflow_date,
                                       ymin = -Inf, ymax = Inf), 
            color = "transparent", fill = col.cat.yel, alpha = 0.3) +
  geom_line(data = df_Q.GW_day, aes(x = date_ghcn, y = level_masl, color = site_factor)) +
  scale_x_date(name = "Date", expand = c(0,0), date_labels = "%Y") +
  scale_y_continuous(name = "Water Level [masl]") +
  scale_color_manual(name = NULL, values = c(col.cat.blu, col.cat.org, col.gray)) +
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "StreamStage+GroundwaterLevels.png"),
       p_levels, width = 160, height = 80, units = "mm")
