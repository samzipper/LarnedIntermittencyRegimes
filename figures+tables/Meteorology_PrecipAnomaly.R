## Meteorology_PrecipAnomaly.R
# This plot is intended to show the cumulative departure from monthly mean conditions through time.

source(file.path("code", "paths+packages.R"))

## load data
# met data
df_mo <- readr::read_csv(file.path("data", "Meteorology_Monthly+SPEI.csv"))

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

## calculate long-term mean
start_date <- ymd("1998-10-01")
df_mo_mean <-
  df_mo %>% 
  subset(date_mid >= start_date) %>% 
  group_by(Month) %>% 
  summarize(prcp_mean = mean(prcp_mm),
            ETo_mean = mean(ETo_mm))

## join and set regime
df_regimes$Regime <- cut(df_regimes$ts_regime_mean, c(-1, 0.5, 1.5), labels = c("Wet", "Dry"))

## calculate cumulative departure
df_departure <-
  left_join(df_mo, df_mo_mean, by = "Month") %>% 
  left_join(df_regimes, by = "date_mid") %>% 
  subset(date_mid >= start_date) %>% 
  mutate(prcp_anom_mm = prcp_mm - prcp_mean,
         prcp_cumanom_mm = cumsum(prcp_anom_mm))

## plot
ggplot(df_departure, aes(x = date_mid, y = prcp_cumanom_mm)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_vline(data = df_regimes_startend[1:4, ], aes(xintercept = date_end), linetype = "dashed") +
  geom_col(aes(fill = Regime)) +
  scale_y_continuous(name = "Monthly Precipitation [mm]", expand = expansion(mult = c(0,0.02))) +
  scale_x_date(name = "Date", limits = c(min(df_departure$date_mid), max(df_departure$date_mid)), expand = c(0,0)) +
  scale_fill_manual(name = "Regime", values = c("Dry" = col.cat.red, "Wet" = col.cat.blu))
