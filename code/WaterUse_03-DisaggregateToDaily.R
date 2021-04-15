## WaterUse_03-DisaggregateToDaily.R
# This script will convert annual water use estimates to daily water use estimates.
# As a first cut, annual water use will be distributed uniformly across the growing
# season (constant daily pumping rate).

## prep workspace
source(file.path("code", "paths+packages.R"))

## load data
df_wuse_yr <- readr::read_csv(file.path("data", "WaterUse_AnnualByRadius.csv"))
df_LWPH4 <- readr::read_csv(file.path("data", "Groundwater_WaterLevels-DailyGHCN-LWPH4-GapFill.csv"))

## figure out which radius is best correlated with annual amplitude in HPA (LWPH4c)
# smooth with rolling average
df_LWPH4$LWPH4c_smooth <- zoo::rollmean(df_LWPH4$LWPH4c, k = 7, na.pad = T)

# remove the major recharge event
df_LWPH4$LWPH4c_smooth[df_LWPH4$date_ghcn >= ymd("2006-08-22") & df_LWPH4$date_ghcn <= ymd("2007-12-31")] <- NA

# remove early 2008 spike
df_LWPH4$LWPH4c_smooth[df_LWPH4$date_ghcn >= ymd("2010-06-05") & df_LWPH4$date_ghcn <= ymd("2010-06-23")] <- NA

ggplot(df_LWPH4, aes(x = date_ghcn, y = LWPH4c_smooth)) +
  geom_line()

# summarize by year
df_LWPH4_yr <- 
  df_LWPH4 %>% 
  dplyr::mutate(Year = lubridate::year(date_ghcn)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(waterlevel_m_max = max(LWPH4c_smooth, na.rm = T),
                   waterlevel_m_min = min(LWPH4c_smooth, na.rm = T),
                   waterlevel_m_range = waterlevel_m_max - waterlevel_m_min)

ggplot(df_LWPH4_yr, aes(x = Year, y = waterlevel_m_range)) +
  geom_point()

# combine with water use
df_wuse.waterlevel_yr <- 
  dplyr::left_join(df_wuse_yr, df_LWPH4_yr, by = "Year") %>% 
  subset(Year >= 2003)

# inspect different radius options
ggplot(df_wuse.waterlevel_yr, aes(x = WaterUse_z, y = waterlevel_m_range, color = factor(radius_m))) +
  geom_point() +
  facet_wrap(~factor(radius_m)) +
  stat_smooth(method = "lm")

# summarize correlation by radius
for (rad in seq(2000, 10000, 1000)){
  lm_rad <- 
    df_wuse.waterlevel_yr %>% 
    subset(radius_m == rad & is.finite(waterlevel_m_range)) %>% 
    lm(waterlevel_m_range ~ WaterUse_z, data = .)
  
  print(paste0(rad, " radius, R2 = ", summary(lm_rad)$r.squared))
}
# peak is at 4000 m - use this

## disaggregate to daily
rad_best <- 4000
df_wuse_rad <- 
  subset(df_wuse_yr, radius_m == rad_best)

# plot HPA water levels by day of year
df_LWPH4$DOY <- lubridate::yday(df_LWPH4$date_ghcn)
df_LWPH4$Year <- lubridate::year(df_LWPH4$date_ghcn)

# set start and end of irrigation
DOY_start <- lubridate::yday("2005-05-01")
DOY_end <- lubridate::yday("2005-09-30")

ggplot(subset(df_LWPH4, Year != 2007), aes(x = DOY, y = LWPH4c_smooth, color = factor(Year))) +
  geom_line() +
  geom_vline(xintercept = DOY_start, color = "red") +
  geom_vline(xintercept = DOY_end, color = "red")

n_irr_days <- DOY_end - DOY_start + 1
df_wuse_rad$WaterUse_m3d <- df_wuse_rad$WaterUse_m3/n_irr_days

# create daily data frame
df_wuse_daily <- tibble::tibble(date_ghcn = seq(from = lubridate::ymd("1990-01-01"), 
                                                to = lubridate::ymd("2020-12-31"), 
                                                by = "day"),
                                Year = lubridate::year(date_ghcn),
                                DOY = lubridate::yday(date_ghcn)) %>% 
  dplyr::left_join(df_wuse_rad, by = "Year")

df_wuse_daily$WaterUse_m3d[df_wuse_daily$DOY < DOY_start | df_wuse_daily$DOY > DOY_end] <- 0

ggplot(df_wuse_daily, aes(x = date_ghcn, y = WaterUse_m3d)) +
  geom_line()

# save output
df_wuse_daily %>% 
  dplyr::select(date_ghcn, WaterUse_m3d) %>% 
  readr::write_csv(file.path("data", "WaterUse_Daily.csv"))

