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
  scale_y_continuous(name = "Number of Water Years", expand = expansion(mult = c(0, 0.02)),
                     breaks = seq(0, 10, 1)) +
  scale_x_continuous(name = "Water Use within 4 km [million m\u00b3]",
                     limits = c(min(wuse_breaks), max(wuse_breaks)),
                     expand = expansion(mult = 0)) +
  theme(plot.margin = margin(t = 1, r = 10, b = 1, l = 1, unit = "pt"))

p_hist_yr <-
  (p_Q_yr + p_met_yr + p_wuse_yr) +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

ggsave(file.path("figures+tables", "StableStates_Bimodality-Hist-Yr.png"),
       p_hist_yr, width = 190, height = 190, units = "mm")

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

#### Timeseries jumps analysis

## monthly plots 

p_Q_ts_mo <-
  ggplot(df_Q_mo, aes(x = date_mid, y = prc_noflow)) +
  geom_line(color = col.cat.blu) +
  scale_y_continuous(name = "No-Flow Days [% of Month]", labels = scales::percent) +
  scale_x_date(name = "Date", limits = c(min(df_Q_mo$date_mid), max(df_Q_mo$date_mid)), expand = c(0,0))

p_met_ts_mo <-
  ggplot(df_met_mo, aes(x = date_mid, y = prcp_mm)) +
  geom_col(fill = col.cat.blu, color = col.cat.blu) +
  scale_y_continuous(name = "Monthly Precipitation [mm]", expand = expansion(mult = c(0,0.02))) +
  scale_x_date(name = "Date", limits = c(min(df_Q_mo$date_mid), max(df_Q_mo$date_mid)), expand = c(0,0))

p_SPEI_3mo <-
  ggplot(df_met_mo, aes(x = date_mid, y = SPEI_3mo, fill = SPEI_3mo, color = SPEI_3mo)) +
  geom_col() +
  geom_hline(yintercept = 0, color = col.gray) +
  scale_y_continuous(name = "SPEI [3 mo]", limits = c(-2.6, 2.6)) +
  scale_x_date(name = "Date", limits = c(min(df_Q_mo$date_mid), max(df_Q_mo$date_mid)), expand = c(0,0)) +
  scale_fill_gradient2(low = col.cat.red, mid = col.gray, high = col.cat.blu, limits = c(-2.6, 2.6), guide = "none") +
  scale_color_gradient2(low = col.cat.red, mid = col.gray, high = col.cat.blu, limits = c(-2.6, 2.6), guide = "none")

p_SPEI_12mo <- 
  ggplot(df_met_mo, aes(x = date_mid, y = SPEI_12mo, fill = SPEI_12mo, color = SPEI_12mo)) +
  geom_col() +
  geom_hline(yintercept = 0, color = col.gray) +
  scale_y_continuous(name = "SPEI [12 mo]", limits = c(-2.6, 2.6)) +
  scale_x_date(name = "Date", limits = c(min(df_Q_mo$date_mid), max(df_Q_mo$date_mid)), expand = c(0,0)) +
  scale_fill_gradient2(low = col.cat.red, mid = col.gray, high = col.cat.blu, limits = c(-2.6, 2.6), guide = "none") +
  scale_color_gradient2(low = col.cat.red, mid = col.gray, high = col.cat.blu, limits = c(-2.6, 2.6), guide = "none") 

p_ts_mo <-
  (p_Q_ts_mo + p_met_ts_mo + p_SPEI_3mo + p_SPEI_12mo) +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

ggsave(file.path("figures+tables", "StableStates_Jumps-Ts-Mo.png"),
       p_ts_mo, width = 190, height = 160, units = "mm")

## yearly plots
p_Q_ts_yr <-
  ggplot(df_Q_yr, aes(x = WaterYear, y = prc_noflow)) +
  geom_line(color = col.cat.blu) +
  geom_point(color = col.cat.blu) +
  scale_y_continuous(name = "No-Flow Days [% of Water Year]", labels = scales::percent) +
  scale_x_continuous(name = "Water Year")

p_met_ts_yr <-
  ggplot(df_met_yr, aes(x = WaterYear, y = prcp_mm)) +
  geom_col(fill = col.cat.blu, color = col.cat.blu) +
  scale_y_continuous(name = "Annual Precipitation [mm]", expand = expansion(mult = c(0,0.02))) +
  scale_x_continuous(name = "Water Year")

p_wuse_ts_yr <-
  ggplot(df_wuse_yr, aes(x = Year, y = WaterUse_m3/1e6)) +
  geom_line(color = col.cat.blu) +
  geom_point(color = col.cat.blu) +
  scale_y_continuous(name = "Water Use within 4 km [million m\u00b3]") +
  scale_x_continuous(name = "Year")

p_ts_yr <-
  (p_Q_ts_yr + p_met_ts_yr + p_wuse_ts_yr) +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

ggsave(file.path("figures+tables", "StableStates_Jumps-Ts-Yr.png"),
       p_ts_yr, width = 190, height = 190, units = "mm")

#### hysteresis analysis
df_met.Q_mo <-
  df_Q_mo %>% 
  dplyr::select(Year, Month, prc_noflow) %>% 
  left_join(df_met_mo, by = c("Year", "Month"))

ggplot(subset(df_met.Q_mo, Year < 2010), aes(x = SPEI_12mo, y = prc_noflow, color = date_mid, group = 1)) +
  geom_point() +
  geom_path()

ggplot(subset(df_met.Q_mo, Year > 2010), aes(x = SPEI_12mo, y = prc_noflow, color = date_mid, group = 1)) +
  geom_point() +
  geom_path()
