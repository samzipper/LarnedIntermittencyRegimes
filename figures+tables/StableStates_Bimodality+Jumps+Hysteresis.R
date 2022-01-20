## StableStates_Bimodality+Jumps+Hysteresis.R
# Investigate hints of alternate stable states: jumps in timeseries, bimodality, and hysteresis.
# Look at both response variables (streamflow, no-flow days) and drivers (precip, pumping).

source(file.path("code", "paths+packages.R"))

#### Prep workspace

## load data
df_Q_day <- 
  readr::read_csv(file.path("data", "Streamflow+Stage_Daily_Clean.csv")) %>% 
  subset(Date >= first_date & Date <= last_date)

df_met_day <- 
  read_csv(file.path("data", "Meteorology_Daily_Clean.csv")) %>% 
  subset(date >= first_date & date <= last_date)

df_met_mo <- 
  read_csv(file.path("data", "Meteorology_Monthly+SPEI.csv")) %>% 
  subset(date_mid >= first_date & date_mid <= last_date)

rad_best <- 4000  # this is determined in script WaterUse_03-DisaggregateToDaily.R
df_wuse_yr <- read_csv(file.path("data", "WaterUse_AnnualByRadius.csv")) %>% 
  subset(radius_m == rad_best & Year >= min(df_Q_day$WaterYear) & Year <= max(df_Q_day$WaterYear))

# regime shifts
df_regimes <- read_csv(file.path("data", "RegimeShifts_rstars.csv"))
df_regime_dates <- subset(df_regimes, ts_regime_shift != 0)
df_regimes_startend <- tibble(
  date_start = c(min(df_regimes$date_mid), df_regime_dates$date_mid),
  date_end = c(df_regime_dates$date_mid, max(df_regimes$date_mid)),
  regime_category = c("Wet", "Dry", "Wet", "Dry", "Wet")
) %>% 
  mutate(WaterYear_start = decimal_date(date_start + days(92)),
         WaterYear_end = decimal_date(date_end + days(92)))

## summarize discharge by water year and month
df_Q_yr <-
  df_Q_day %>% 
  dplyr::group_by(WaterYear) %>% 
  dplyr::summarize(n_noflow = sum(discharge_cms == 0),
                   n_total = sum(is.finite(discharge_cms)),
                   prc_noflow = n_noflow/n_total)

df_Q_mo <-
  df_Q_day %>% 
  mutate(Year = year(Date), 
         Month = month(Date)) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarize(date_mid = mean(Date),
                   n_noflow = sum(discharge_cms == 0),
                   n_total = sum(is.finite(discharge_cms)),
                   prc_noflow = n_noflow/n_total)

## summarize precip by water year and month
df_met_yr <- 
  df_met_day %>% 
  dplyr::group_by(WaterYear) %>% 
  dplyr::summarize(prcp_mm = sum(prcp_mm))

#### Bimodality analysis

## plots - annual histograms
p_Q_yr <- 
  ggplot(df_Q_yr, aes(x = prc_noflow)) +
  geom_histogram(breaks = seq(0, 1, 0.1), fill = col.cat.blu, color = "white") +
  scale_y_continuous(name = "Number of Water Years", expand = expansion(mult = c(0, 0.02)),
                     breaks = seq(0, 10, 2)) +
  scale_x_continuous(name = "No-Flow Days [% of Water Year]", 
                     breaks = seq(0, 1, 0.2),
                     minor_breaks = seq(0, 1, 0.1),
                     labels = scales::percent, 
                     expand = expansion(mult = 0)) +
  theme(plot.margin = margin(t = 1, r = 10, b = 1, l = 1, unit = "pt"))

min(df_met_yr$prcp_mm)
max(df_met_yr$prcp_mm)
prcp_breaks <- c(seq(350, 1050, 50), 1101)
p_met_yr <-
  ggplot(df_met_yr, aes(x = prcp_mm)) +
  geom_histogram(fill = col.cat.blu, 
                 breaks = prcp_breaks, 
                 color = "white") +
  scale_y_continuous(name = "Number of Water Years", expand = expansion(mult = c(0, 0.02)),
                     breaks = seq(0, 10, 1)) +
  scale_x_continuous(name = "Precipitation [mm]",
                     limits = c(min(prcp_breaks), max(prcp_breaks)),
                     breaks = seq(400, 1000, 200),
                     expand = expansion(mult = 0)) +
  theme(plot.margin = margin(t = 1, r = 10, b = 1, l = 1, unit = "pt"))

min(df_wuse_yr$WaterUse_m3/1e6)
max(df_wuse_yr$WaterUse_m3/1e6)
wuse_breaks <- seq(4, 8.5, 0.5)
p_wuse_yr <-
  ggplot(df_wuse_yr, aes(x = WaterUse_m3/1e6)) +
  geom_histogram(fill = col.cat.blu, 
                 color = "white", 
                 breaks = wuse_breaks) +
  scale_y_continuous(name = "Number of Years", expand = expansion(mult = c(0, 0.02)),
                     breaks = seq(0, 10, 1)) +
  scale_x_continuous(name = "Annual Pumping [million m\u00b3]",
                     limits = c(min(wuse_breaks), max(wuse_breaks)),
                     expand = expansion(mult = 0)) +
  theme(plot.margin = margin(t = 1, r = 10, b = 1, l = 1, unit = "pt"))

p_hist_yr <-
  (p_Q_yr + p_met_yr + p_wuse_yr) +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

ggsave(file.path("figures+tables", "StableStates_Bimodality-Hist-Yr.png"),
       p_hist_yr, width = 95, height = 160, units = "mm")

# stats on yearly skew
sum(df_Q_yr$prc_noflow <= 0.1) + sum(df_Q_yr$prc_noflow >= 0.9)
length(df_Q_yr$prc_noflow)

## plots - monthly histograms
p_Q_mo <- 
  ggplot(df_Q_mo, aes(x = prc_noflow)) +
  geom_histogram(breaks = seq(0, 1, 0.1), fill = col.cat.blu, color = "white") +
  scale_y_continuous(name = "Number of Months", expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(name = "No-Flow Days [% of Month]", 
                     breaks = seq(0, 1, 0.2),
                     minor_breaks = seq(0, 1, 0.1),
                     labels = scales::percent, 
                     expand = expansion(mult = 0)) +
  theme(plot.margin = margin(t = 1, r = 10, b = 1, l = 1, unit = "pt"))

min(df_met_mo$prcp_mm)
max(df_met_mo$prcp_mm)
prcp_mo_breaks <- seq(0, 400, 50)
p_met_mo <-
  ggplot(df_met_mo, aes(x = prcp_mm)) +
  geom_histogram(fill = col.cat.blu, 
                 breaks = prcp_mo_breaks, 
                 color = "white") +
  scale_y_continuous(name = "Number of Months", expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(name = "Precipitation [mm]",
                     limits = c(min(prcp_mo_breaks), max(prcp_mo_breaks)),
                     expand = expansion(mult = 0)) +
  theme(plot.margin = margin(t = 1, r = 10, b = 1, l = 1, unit = "pt"))

p_hist_mo <-
  (p_Q_mo + p_met_mo) +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

ggsave(file.path("figures+tables", "StableStates_Bimodality-Hist-Mo.png"),
       p_hist_mo, width = 190, height = 120, units = "mm")

# combine annual water use with monthly discharge and met
p_hist_mo.yr <-
  (p_Q_mo + p_met_mo + p_wuse_yr) +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

ggsave(file.path("figures+tables", "StableStates_Bimodality-Hist-Mo+Yr.png"),
       p_hist_mo.yr, width = 95, height = 130, units = "mm")

# stats on monthly skew
sum(df_Q_mo$prc_noflow <= 0.1)
sum(df_Q_mo$prc_noflow >= 0.9)
sum(df_Q_mo$prc_noflow <= 0.1) + sum(df_Q_mo$prc_noflow >= 0.9)
length(df_Q_mo$prc_noflow)

sum(df_met_mo$prcp_mm <= 50)
sum(df_met_mo$prcp_mm <= 100)

mean(df_wuse_yr$WaterUse_m3)
median(df_wuse_yr$WaterUse_m3)

#### Timeseries jumps analysis

## monthly plots 

p_Q_ts_mo <-
  ggplot() +
  geom_rect(data = df_regimes_startend, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf, 
                                            fill = regime_category), alpha = 0.25) +
  geom_line(data = df_Q_mo, aes(x = date_mid, y = prc_noflow)) +
  scale_y_continuous(name = "No-Flow Days\n[% of Month]", labels = scales::percent) +
  scale_x_date(name = "Date", limits = c(min(df_Q_mo$date_mid), max(df_Q_mo$date_mid)), expand = c(0,0)) +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu))

p_met_ts_mo <-
  ggplot() +
  geom_rect(data = df_regimes_startend, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf, 
                                            fill = regime_category), alpha = 0.25) +
  geom_col(data = df_met_mo, aes(x = date_mid, y = prcp_mm), fill = "black", color = "black") +
  scale_y_continuous(name = "Monthly Precipitation\n[mm]", expand = expansion(mult = c(0,0.02))) +
  scale_x_date(name = "Date", limits = c(min(df_Q_mo$date_mid), max(df_Q_mo$date_mid)), expand = c(0,0)) +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu))

p_SPEI_3mo <-
  ggplot() +
  geom_rect(data = df_regimes_startend, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf, 
                                            fill = regime_category), alpha = 0.25) +
  geom_col(data = df_met_mo, aes(x = date_mid, y = SPEI_3mo), color = "black") +
  geom_hline(yintercept = 0, color = col.gray) +
  scale_y_continuous(name = "SPEI [3 mo]", limits = c(-2.6, 2.6)) +
  scale_x_date(name = "Date", limits = c(min(df_Q_mo$date_mid), max(df_Q_mo$date_mid)), expand = c(0,0)) +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu))

p_SPEI_12mo <- 
  ggplot() +
  geom_rect(data = df_regimes_startend, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf, 
                                            fill = regime_category), alpha = 0.25) +
  geom_col(data = df_met_mo, aes(x = date_mid, y = SPEI_12mo), color = "black") +
  geom_hline(yintercept = 0, color = col.gray) +
  scale_y_continuous(name = "SPEI [12 mo]", limits = c(-2.6, 2.6)) +
  scale_x_date(name = "Date", limits = c(min(df_Q_mo$date_mid), max(df_Q_mo$date_mid)), expand = c(0,0)) +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu))

p_ts_mo <-
  (p_Q_ts_mo + p_met_ts_mo) +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "StableStates_Jumps-Ts-Mo.png"),
       p_ts_mo, width = 190, height = 120, units = "mm")

# combine annual water use with monthly met and no-flow
p_wuse_ts_mo.yr <-
  ggplot() +
  geom_rect(data = df_regimes_startend, aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf, 
                                            fill = regime_category), alpha = 0.25) +
  geom_line(data = df_wuse_yr, aes(x = ymd(paste0(Year,"-01-01")) + days(182), y = WaterUse_m3/1e6), color = "black") +
  geom_point(data = df_wuse_yr, aes(x = ymd(paste0(Year,"-01-01")) + days(182), y = WaterUse_m3/1e6), color = "black") +
  scale_y_continuous(name = "Annual Pumping\n[million m\u00b3]") +
  scale_x_date(name = "Date", limits = c(min(df_Q_mo$date_mid), max(df_Q_mo$date_mid)), expand = c(0,0)) +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu))

p_ts_mo.yr <-
  (p_Q_ts_mo + p_met_ts_mo + p_wuse_ts_mo.yr) +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "right")

ggsave(file.path("figures+tables", "StableStates_Jumps-Ts-Mo+Yr.png"),
       p_ts_mo.yr, width = 190, height = 120, units = "mm")

## yearly plots
df_regimes_startend$WaterYear_start[1] <- -Inf
df_regimes_startend$WaterYear_end[5] <- Inf
p_Q_ts_yr <-
  ggplot() +
  geom_rect(data = df_regimes_startend, aes(xmin = WaterYear_start, xmax = WaterYear_end, ymin = -Inf, ymax = Inf, 
                                            fill = regime_category), alpha = 0.25) +
  geom_line(data = df_Q_yr, aes(x = WaterYear, y = prc_noflow), color = "black") +
  geom_point(data = df_Q_yr, aes(x = WaterYear, y = prc_noflow), color = "black") +
  scale_y_continuous(name = "No-Flow Days [% of Water Year]", labels = scales::percent) +
  scale_x_continuous(name = "Water Year") +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu))

p_met_ts_yr <-
  ggplot() +
  geom_rect(data = df_regimes_startend, aes(xmin = WaterYear_start, xmax = WaterYear_end, ymin = -Inf, ymax = Inf, 
                                            fill = regime_category), alpha = 0.25) +
  geom_line(data = df_met_yr, aes(x = WaterYear, y = prcp_mm)) +
  geom_point(data = df_met_yr, aes(x = WaterYear, y = prcp_mm)) +
  scale_y_continuous(name = "Annual Precipitation [mm]") +
  scale_x_continuous(name = "Water Year") +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu))

p_wuse_ts_yr <-
  ggplot() +
  geom_rect(data = df_regimes_startend, aes(xmin = WaterYear_start, xmax = WaterYear_end, ymin = -Inf, ymax = Inf, 
                                            fill = regime_category), alpha = 0.25) +
  geom_line(data = df_wuse_yr, aes(x = Year, y = WaterUse_m3/1e6), color = "black") +
  geom_point(data = df_wuse_yr, aes(x = Year, y = WaterUse_m3/1e6), color = "black") +
  scale_y_continuous(name = "Pumping within 4 km [million m\u00b3]") +
  scale_x_continuous(name = "Year", limits = c(min(df_Q_yr$WaterYear), max(df_Q_yr$WaterYear))) +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu))

p_ts_yr <-
  (p_Q_ts_yr + p_met_ts_yr + p_wuse_ts_yr) +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom")

ggsave(file.path("figures+tables", "StableStates_Jumps-Ts-Yr.png"),
       p_ts_yr, width = 190, height = 190, units = "mm")

#### hysteresis analysis
df_met.Q_mo <-
  df_Q_mo %>% 
  dplyr::select(Year, Month, prc_noflow) %>% 
  left_join(df_met_mo, by = c("Year", "Month")) %>% 
  mutate(WaterYear_dec = decimal_date(date_mid + days(92)),
         Cycle1 = WaterYear_dec < 2009)

# try smoothing no-flow days
for (m in 1:24){
  df_met.Q_mo$prc_noflow_smooth <- stats::filter(df_met.Q_mo$prc_noflow, filter = rep(1/m, m), sides = 1)
  colnames(df_met.Q_mo)[colnames(df_met.Q_mo) == "prc_noflow_smooth"] <- paste0("prc_noflow_", m, "mo")
}

spei_limits <- c(min(df_met.Q_mo$SPEI_1mo, na.rm = T), max(df_met.Q_mo$SPEI_1mo, na.rm = T))
spei_breaks <- seq(-2, 2, 1)

p_hyst_cycle1 <- 
  ggplot(subset(df_met.Q_mo, Cycle1), aes(x = SPEI_12mo, y = prc_noflow_12mo, color = WaterYear_dec, group = 1)) +
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point() +
  geom_path() +
  scale_x_continuous(name = "SPEI [12 month]", limits = spei_limits, breaks = spei_breaks) +
  scale_y_continuous(name = "No-Flow Days [% of month, smoothed]", labels = scales::percent) +
  scale_color_viridis_c(name = "Water Year", breaks = seq(2000, 2009, 4)) +
  labs(title = "(a) 1998-2008 water years") +
  theme(legend.position = "bottom")

p_hyst_cycle2 <-
  ggplot(subset(df_met.Q_mo, !Cycle1), aes(x = SPEI_12mo, y = prc_noflow_12mo, color = WaterYear_dec, group = 1)) +
  geom_vline(xintercept = 0, color = col.gray) +
  geom_point() +
  geom_path() +
  scale_x_continuous(name = "SPEI [12 month]", limits = spei_limits, breaks = spei_breaks) +
  scale_y_continuous(name = "No-Flow Days [% of month, smoothed]", labels = scales::percent) +
  scale_color_viridis_c(name = "Water Year", breaks = seq(2010, 2020, 5)) +
  labs(title = "(b) 2009-2021 water years") +
  theme(legend.position = "bottom")

p_hyst_combo <- 
  (p_hyst_cycle1 + p_hyst_cycle2) +
  plot_layout(ncol = 2)

ggsave(file.path("figures+tables", "StableStates_Hyst-12mo_NoArrows.png"),
       p_hyst_combo, width = 190, height = 95, units = "mm")
ggsave(file.path("figures+tables", "StableStates_Hyst-12mo.pdf"),
       p_hyst_combo, width = 190, height = 95, units = "mm", device = cairo_pdf)
