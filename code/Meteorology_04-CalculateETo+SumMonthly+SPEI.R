## Meteorology_04-CalculateETo+SumMonthly+SPEI.R

source(file.path("code", "paths+packages.R"))

## load data
df_daily <- 
  readr::read_csv(file.path("data", "Meteorology_Daily_Clean.csv")) %>% 
  dplyr::mutate(Year = year(date),
                Month = month(date),
                Day = day(date))

## calculate ET
# Hargreaves-Samani using evapotranspiration package: https://cran.r-project.org/package=Evapotranspiration
df_forET <-
  df_daily %>% 
  dplyr::rename(Tmax = tmax_c, Tmin = tmin_c, Precip = prcp_mm)

# define constants - for Hargreaves-Samani need Elev, lambda, lat_rad,  Gsc
ET_constants <- list(Elev = 593,    # elevation [masl]
                     lat_rad = 38.203611*pi/180, # latitude converted to radians
                     lambda = 2.45, # use default
                     Gsc = 0.082)   # use default

# convert to format needed for package
df_preppedforET <- 
  ReadInputs(varnames = c("Tmax", "Tmin", "Precip"),
             climatedata = df_forET,
             stopmissing = c(1, 1, 1))

# calculate reference ET
df_ET <- ET.HargreavesSamani(data = df_preppedforET,
                             constants = ET_constants,
                             timestep = "daily")

# add to daily data frame
df_daily$ETo_mm <- as.numeric(df_ET[["ET.Daily"]])

## load mesonet data (only 2010-present) to evaluate Hargreaves-Samani performance
df_mesonet <- 
  file.path("data", "Meteorology_Mesonet-Daily-Radium_20100101-20201119.csv") %>% 
  readr::read_csv() %>% 
  dplyr::left_join(df_daily, by = c("Date" = "date"), suffix = c("_meso", "_ghcn"))

# rescale GHCN ETo based on mesonet ETo
ETo_rescale <- function(ETo, lower_orig, upper_orig, lower_new, upper_new, adj_factor = 1, bound = T){
  # ETo = value(s) that should be rescaled - can be scalar or vector
  # lower_orig = lower bound for original data
  # upper_orig = upper bound for original data
  # lower_new = lower bound for rescaled data
  # upper_new = upper bound for rescaled data
  # adj_factor = additional coefficient by which to multiply the scaled output
  # bound = should data exceeding bounds be set to the bound values (T) or extrapolated (F)
  
  scale_factor <- (upper_new - lower_new)/(upper_orig - lower_orig)
  ETo_new <- ETo*scale_factor*adj_factor
  if (bound){
    ETo_new[ETo_new > upper_new] <- upper_new
    ETo_new[ETo_new < lower_new] <- lower_new
  }
  return(ETo_new)
  
}

# i played with different combinations of bounds and factors, and found this to have the best performance
df_mesonet$ETo_mm_ghcn_scaled <- ETo_rescale(ETo = df_mesonet$ETo_mm_ghcn,
                                             lower_orig = 0,
                                             upper_orig = quantile(df_mesonet$ETo_mm_ghcn, 0.99, na.rm = T),
                                             lower_new = 0,
                                             upper_new = quantile(df_mesonet$ETo_mm_meso, 0.99, na.rm = T),
                                             adj_factor = (1/0.638),
                                             # the adj_factor was determine by scaling GHCN with adj_factor=1,
                                             # then using the slope of a linear relationship between meso and ghcn_scaled_shifted
                                             bound = T)

# because of the differences in measurements, we would expect GHCN ETo to best match MESO ETo from the preceding day
df_mesonet$ETo_mm_ghcn_shifted <- c(df_mesonet$ETo_mm_ghcn[2:dim(df_mesonet)[1]], NA)
df_mesonet$ETo_mm_ghcn_scaled_shifted <- c(df_mesonet$ETo_mm_ghcn_scaled[2:dim(df_mesonet)[1]], NA)

# plot to compare and determine adj_factor
lm(ETo_mm_ghcn_shifted ~ ETo_mm_meso, data = df_mesonet) %>% summary()
lm(ETo_mm_ghcn_scaled_shifted ~ ETo_mm_meso, data = df_mesonet) %>% summary() # use slope here as the denominator in adj_factor

ggplot(df_mesonet, aes(x = ETo_mm_meso, y = ETo_mm_ghcn_shifted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")
ggplot(df_mesonet, aes(x = ETo_mm_meso, y = ETo_mm_ghcn_scaled_shifted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

dplyr::select(df_mesonet, Date, ETo_mm_meso, ETo_mm_ghcn, ETo_mm_ghcn_scaled) %>% 
  tidyr::pivot_longer(!Date) %>% 
  ggplot(aes(x = value, color = name)) +
  geom_density()

# also compare ETo monthly sums to evaluate performance
df_meso_mo <-
  df_mesonet %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarize(prcp_meso_sum = sum(prcp_mm_meso),
                   prcp_ghcn_sum = sum(prcp_mm_ghcn),
                   ETo_meso_sum = sum(ETo_mm_meso),
                   ETo_ghcn_sum = sum(ETo_mm_ghcn),
                   ETo_ghcn_scaled_sum = sum(ETo_mm_ghcn_scaled))

ggplot(df_meso_mo, aes(x = ETo_meso_sum, y = ETo_ghcn_scaled_sum)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

lm(ETo_ghcn_sum ~ ETo_meso_sum, data = df_meso_mo) %>% summary()
lm(ETo_ghcn_scaled_sum ~ ETo_meso_sum, data = df_meso_mo) %>% summary() # showing improved performance of scaling

# also compare precip; just like ETo, need to shift
df_mesonet$prcp_mm_ghcn_shifted <- c(df_mesonet$prcp_mm_ghcn[2:dim(df_mesonet)[1]], NA)
ggplot(df_mesonet, aes(x = prcp_mm_meso, y = prcp_mm_ghcn_shifted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")
ggplot(df_mesonet, aes(x = prcp_mm_meso, y = prcp_mm_ghcn)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")
ggplot(df_meso_mo, aes(x = prcp_meso_sum, y = prcp_ghcn_sum)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")
# daily match is not great but monthly match is decent, this is because of 
# differences in observation time and daily aggregation: https://twitter.com/jpshanno/status/1349387552118218765

## save plots showing ETo comparison between the two data sources
p_ETo_mo <-
  ggplot(df_meso_mo, aes(x = ETo_meso_sum, y = ETo_ghcn_scaled_sum)) +
  geom_point(shape = 1) +
  geom_abline(intercept = 0, slope = 1, color = col.cat.red) +
  scale_x_continuous(name = "Monthly ETo, Mesonet\nASCE Standardized Grass [mm]") +
  scale_y_continuous(name = "Monthly ETo, GHCN\nCorrected Hargreaves-Samani [mm]")

p_ETo_d <-
  ggplot(df_mesonet, aes(x = ETo_mm_meso, y = ETo_mm_ghcn_scaled_shifted)) +
  geom_point(shape = 1) +
  geom_abline(intercept = 0, slope = 1, color = col.cat.red) +
  scale_x_continuous(name = "Daily ETo, Mesonet\nASCE Standardized Grass [mm]") +
  scale_y_continuous(name = "Daily ETo, GHCN\nCorrected Hargreaves-Samani [mm]")

p_ETo_combo <-
  (p_ETo_mo + p_ETo_d) +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")
ggsave(file.path("figures+tables", "Meteorology_03-EToComparison.png"),
       p_ETo_combo, width = 190, height = 95, units = "mm")

# stats for overall fit between mesonet and ghcn
lm(ETo_mm_ghcn_scaled_shifted ~ ETo_mm_meso, data = df_mesonet) %>% summary() # daily ETo; R2 = 0.69, slope = 0.93
lm(prcp_mm_ghcn_shifted ~ prcp_mm_meso, data = df_mesonet) %>% summary() # daily precip; R2 = 0.29, slope = 0.62
lm(ETo_ghcn_scaled_sum ~ ETo_meso_sum, data = df_meso_mo) %>% summary() # monthly ETo; R2 = 0.94, slope = 1.01
lm(prcp_ghcn_sum ~ prcp_meso_sum, data = df_meso_mo) %>% summary() # monthly precip; R2 = 0.87, slope = 1.08

## apply scaling to full meteorological record
df_daily$ETo_mm_scaled <- round(ETo_rescale(ETo = df_daily$ETo_mm,
                                            lower_orig = 0,
                                            upper_orig = quantile(df_mesonet$ETo_mm_ghcn, 0.99, na.rm = T),
                                            lower_new = 0,
                                            upper_new = quantile(df_mesonet$ETo_mm_meso, 0.99, na.rm = T),
                                            adj_factor = (1/0.638),
                                            # the adj_factor was determine by scaling GHCN with adj_factor=1,
                                            # then using the slope of a linear relationship between meso and ghcn_scaled_shifted
                                            bound = T), 
                                2)

## sum to monthly and annual
# monthly
df_mo <- 
  df_daily %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarize(date_mid = mean(date),
                   prcp_mm_sum = sum(prcp_mm),
                   ETo_mm_sum = sum(ETo_mm_scaled),
                   defc_mm = prcp_mm_sum - ETo_mm_sum,
                   tmax_c_mean = mean(tmax_c),
                   tmin_c_mean = mean(tmin_c))
# annual
df_yr <- 
  df_daily %>% 
  mutate(WaterYear = year(date + days(92))) %>% 
  dplyr::group_by(WaterYear) %>% 
  dplyr::summarize(prcp_mm_sum = sum(prcp_mm),
                   ETo_mm_sum = sum(ETo_mm_scaled),
                   tmax_mm_mean = mean(tmax_c),
                   tmin_mm_mean = mean(tmin_c))

# plot and inspect
ggplot(df_yr, aes(x = WaterYear, y = prcp_mm_sum)) +
  geom_point() +
  stat_smooth()

ggplot(df_yr, aes(x = WaterYear, y = ETo_mm_sum)) +
  geom_point() +
  stat_smooth()

# panel plot by month
ggplot(df_mo, aes(x = Year, y = prcp_mm_sum)) +
  geom_point() +
  facet_wrap(~ Month, scales = "free") +
  stat_smooth(method = "lm")

ggplot(df_mo, aes(x = Year, y = ETo_mm_sum)) +
  geom_point() +
  facet_wrap(~ Month, scales = "free") +
  stat_smooth(method = "lm")

ggplot(df_mo, aes(x = Year, y = defc_mm)) +
  geom_point() +
  facet_wrap(~ Month, scales = "free") +
  stat_smooth(method = "lm")

## calculate SPEI at 1-24 months timescales
spei_ts <- seq(1,24)
for (ts in spei_ts){
  spei_calc <- SPEI::spei(df_mo$defc_mm, scale = ts)
  df_mo$spei_new <- round(c(spei_calc$fitted), 2)
  colnames(df_mo)[colnames(df_mo)=="spei_new"] <- paste0("SPEI_", ts, "mo")
}

## save output
# daily data
df_daily %>% 
  dplyr::select(date, prcp_mm, tmax_c, tmin_c, ETo_mm_scaled) %>% 
  dplyr::rename(ETo_mm = ETo_mm_scaled) %>% 
  readr::write_csv(file.path("data", "Meteorology_Daily_WithETo.csv"))

# monthly data
df_mo %>% 
  dplyr::rename(prcp_mm = prcp_mm_sum, ETo_mm = ETo_mm_sum, tmax_c = tmax_c_mean, tmin_c = tmin_c_mean) %>% 
  readr::write_csv(file.path("data", "Meteorology_Monthly+SPEI.csv"))
