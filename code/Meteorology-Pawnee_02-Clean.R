## Meteorology-Pawnee_02-Clean.R
# This will clean and compile data. It will not do any gap-filling - there are no gaps in precip data
# from early 1900s onwards, but some gaps in the temperature data in recent years. 
# Currently only plan to use precip data - if temp data are needed, will have to gapfill.

source(file.path("code", "paths+packages.R"))

# load output from Meteorology_01-Download.R
df <- readr::read_csv(file.path("data", "Meteorology-Pawnee_Daily_Raw.csv"))

# retain the highest priority measurement for each date and variable
df_trimmed <-
  df %>% 
  tidyr::pivot_longer(cols = c("prcp_mm", "tmax_c", "tmin_c"),
                      names_to = "variable") %>% 
  subset(complete.cases(.)) %>% 
  dplyr::group_by(date, variable) %>% 
  dplyr::filter(Priority == min(Priority)) %>% 
  dplyr::ungroup()

# separate for each variable to get a single station and value for each
df_prcp <- 
  subset(df_trimmed, variable == "prcp_mm") %>% 
  dplyr::select(date, Station, value) %>% 
  dplyr::rename(prcp_mm = value, station_prcp = Station) %>% 
  # add in gaps for missing dates
  dplyr::right_join(tibble::tibble(date = seq(min(df$date), max(df$date), by = 1)), by = "date")

df_tmax <- 
  subset(df_trimmed, variable == "tmax_c") %>% 
  dplyr::select(date, Station, value) %>% 
  dplyr::rename(tmax_c = value, station_tmax = Station) %>% 
  dplyr::right_join(tibble::tibble(date = seq(min(df$date), max(df$date), by = 1)), by = "date")

df_tmin <- 
  subset(df_trimmed, variable == "tmin_c") %>% 
  dplyr::select(date, Station, value) %>% 
  dplyr::rename(tmin_c = value, station_tmin = Station) %>% 
  dplyr::right_join(tibble::tibble(date = seq(min(df$date), max(df$date), by = 1)), by = "date")

# join them all back together again, by date
df_all <-
  dplyr::left_join(df_prcp, df_tmax, by = "date") %>% 
  dplyr::left_join(df_tmin, by = "date") %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate(Year = year(date),
                Month = month(date))

## gap-filling
# for temperature, fill in gaps <= 5 days by linear interpolation
df_all$tmax_c <- na.approx(df_all$tmax_c, maxgap = 5)
df_all$tmin_c <- na.approx(df_all$tmin_c, maxgap = 5)
df_all$station_tmax[is.na(df_all$station_tmax) & is.finite(df_all$tmax_c)] <- "Gapfill"
df_all$station_tmin[is.na(df_all$station_tmin) & is.finite(df_all$tmin_c)] <- "Gapfill"

# inspect for completeness
rnoaa::vis_miss(df_all)
df_all$date[is.na(df_all$prcp_mm)]
df_all$date[is.na(df_all$tmax_c)]
df_all$date[is.na(df_all$tmin_c)]
df_full <- subset(df_all, Year >= 1907) # all are complete 1907-present

#### now, QA/QC. steps are to:
# - identify outliers
# - if other stations have measurements from the outlier date, replace
# - if not, set to NA and deal with during gap-filling

## identify outliers: tmin > tmax (5 dates) - manually fix
i_backwards <- which(df_full$tmin_c > df_full$tmax_c)
dates_backwards <- df_full$date[i_backwards]
# 1907-02-05: replace with next highest priority station
df[df$date == dates_backwards[1], ]
df_full$tmax_c[i_backwards[1]] <- df$tmax_c[df$date == dates_backwards[1] & df$Priority == 11]
df_full$tmin_c[i_backwards[1]] <- df$tmin_c[df$date == dates_backwards[1] & df$Priority == 11]
df_full[i_backwards[1], c("station_tmax", "station_tmin")] <- 
  df$Station[df$date == dates_backwards[1] & df$Priority == 11]
df_full[i_backwards[1],]

# 1912-01-07: replace with next highest priority station
df[df$date == dates_backwards[2], ]
df_full$tmax_c[i_backwards[2]] <- df$tmax_c[df$date == dates_backwards[2] & df$Priority == 11]
df_full$tmin_c[i_backwards[2]] <- df$tmin_c[df$date == dates_backwards[2] & df$Priority == 11]
df_full[i_backwards[2], c("station_tmax", "station_tmin")] <- 
  df$Station[df$date == dates_backwards[2] & df$Priority == 11]
df_full[i_backwards[2],]

# 1912-04-19: replace with next highest priority station
df[df$date == dates_backwards[3], ]
df_full$tmax_c[i_backwards[3]] <- df$tmax_c[df$date == dates_backwards[3] & df$Priority == 11]
df_full$tmin_c[i_backwards[3]] <- df$tmin_c[df$date == dates_backwards[3] & df$Priority == 11]
df_full[i_backwards[3], c("station_tmax", "station_tmin")] <- 
  df$Station[df$date == dates_backwards[3] & df$Priority == 11]
df_full[i_backwards[3],]

# 1930-11-22: replace with next highest priority station
df[df$date == dates_backwards[4], ]
df_full$tmax_c[i_backwards[4]] <- df$tmax_c[df$date == dates_backwards[4] & df$Priority == 11]
df_full$tmin_c[i_backwards[4]] <- df$tmin_c[df$date == dates_backwards[4] & df$Priority == 11]
df_full[i_backwards[4], c("station_tmax", "station_tmin")] <- 
  df$Station[df$date == dates_backwards[4] & df$Priority == 11]
df_full[i_backwards[4],]

# 1935-01-20: replace with next highest priority station
df[df$date == dates_backwards[5], ]
df_full$tmax_c[i_backwards[5]] <- df$tmax_c[df$date == dates_backwards[5] & df$Priority == 11]
df_full$tmin_c[i_backwards[5]] <- df$tmin_c[df$date == dates_backwards[5] & df$Priority == 11]
df_full[i_backwards[5], c("station_tmax", "station_tmin")] <- 
  df$Station[df$date == dates_backwards[5] & df$Priority == 11]
df_full[i_backwards[5],]

# double check nothing more backwards
if (!(length(which(df_full$tmin_c > df_full$tmax_c))==0)) stop("still some tmin > tmax values")

# check station
table(subset(df_full, Year >= 1988)$station_prcp)

## more manual QA/QC checks
# inspect daily and monthly values
ggplot(df_full, aes(x = date, y = prcp_mm)) + geom_line()
ggplot(df_full, aes(x = date, y = tmax_c)) + geom_line()
ggplot(df_full, aes(x = date, y = tmin_c)) + geom_line()

# figure out which values are more than Xx the IQR for each month
iqr_k <- 3  # factor by which to multiply IQR
df_mo_iqr <-
  df_full %>% 
  dplyr::group_by(Month) %>% 
  dplyr::summarize(tmax_q1 = quantile(tmax_c, 0.25, na.rm = T),
                   tmax_q3 = quantile(tmax_c, 0.75, na.rm = T),
                   tmax_iqr = tmax_q3-tmax_q1,
                   tmax_lower = tmax_q1 - iqr_k*tmax_iqr,
                   tmax_upper = tmax_q3 + iqr_k*tmax_iqr,
                   tmin_q1 = quantile(tmin_c, 0.25, na.rm = T),
                   tmin_q3 = quantile(tmin_c, 0.75, na.rm = T),
                   tmin_iqr = tmin_q3-tmin_q1,
                   tmin_lower = tmin_q1 - iqr_k*tmin_iqr,
                   tmin_upper = tmin_q3 + iqr_k*tmin_iqr,
                   tdiff_q1 = quantile((tmax_c - tmin_c), 0.25, na.rm = T),
                   tdiff_q3 = quantile((tmax_c - tmin_c), 0.75, na.rm = T),
                   tdiff_iqr = tdiff_q3-tdiff_q1,
                   tdiff_lower = tdiff_q1 - iqr_k*tdiff_iqr,
                   tdiff_upper = tdiff_q3 + iqr_k*tdiff_iqr)


df_full_withIQRs <-
  df_mo_iqr %>% 
  dplyr::select(Month, tmax_lower, tmax_upper, tmin_lower, tmin_upper, tdiff_lower, tdiff_upper) %>% 
  dplyr::left_join(df_full, ., by = "Month") %>% 
  dplyr::mutate(tdiff_c = tmax_c - tmin_c)

i_tmax_outliers <- which(df_full_withIQRs$tmax_c < df_full_withIQRs$tmax_lower | 
                         df_full_withIQRs$tmax_c > df_full_withIQRs$tmax_upper)

i_tmin_outliers <- which(df_full_withIQRs$tmin_c < df_full_withIQRs$tmin_lower | 
                         df_full_withIQRs$tmin_c > df_full_withIQRs$tmin_upper)

i_tdiff_outliers <- which(df_full_withIQRs$tdiff_c < df_full_withIQRs$tdiff_lower | 
                          df_full_withIQRs$tdiff_c > df_full_withIQRs$tdiff_upper)
i_outliers <- c(i_tmax_outliers, i_tmin_outliers, i_tdiff_outliers)

dates_outliers <- df_full_withIQRs$date[i_outliers]

# manually check each one
df[df$date == dates_outliers[1], ] # looks OK
df[df$date == dates_outliers[2], ] # looks OK
df[df$date == dates_outliers[3], ] # looks OK
df[df$date == dates_outliers[4], ] # looks OK

df[df$date == dates_outliers[5], ] # problem with tmin - replace with station 11
df_full$tmin_c[i_outliers[5]] <- df$tmin_c[df$date == dates_outliers[5] & df$Priority == 11]
df_full[i_outliers[5], "station_tmin"] <- 
  df$Station[df$date == dates_outliers[5] & df$Priority == 11]
df_full[i_outliers[5],]

df[df$date == dates_outliers[6], ] # problem with tmin - replace with station 11
df_full$tmin_c[i_outliers[6]] <- df$tmin_c[df$date == dates_outliers[6] & df$Priority == 11]
df_full[i_outliers[6], "station_tmin"] <- 
  df$Station[df$date == dates_outliers[6] & df$Priority == 11]
df_full[i_outliers[6],]

df[df$date == dates_outliers[7], ] # problem with tmin - replace with station 11
df_full$tmin_c[i_outliers[7]] <- df$tmin_c[df$date == dates_outliers[7] & df$Priority == 11]
df_full[i_outliers[7], "station_tmin"] <- 
  df$Station[df$date == dates_outliers[7] & df$Priority == 11]
df_full[i_outliers[7],]

df[df$date == dates_outliers[8], ] # looks OK
df[df$date == dates_outliers[9], ] # looks OK

df[df$date == dates_outliers[10], ] # problem with tmin - replace with station 11
df_full$tmin_c[i_outliers[10]] <- df$tmin_c[df$date == dates_outliers[10] & df$Priority == 11]
df_full[i_outliers[10], "station_tmin"] <- 
  df$Station[df$date == dates_outliers[10] & df$Priority == 11]
df_full[i_outliers[10],]

df[df$date == dates_outliers[11], ] # problem with tmin - replace with station 11
df_full$tmin_c[i_outliers[11]] <- df$tmin_c[df$date == dates_outliers[10] & df$Priority == 11]
df_full[i_outliers[11], "station_tmin"] <- 
  df$Station[df$date == dates_outliers[11] & df$Priority == 11]
df_full[i_outliers[11],]

# check missing dates
df_full$date[is.na(df_full$prcp_mm)]
df_full$date[is.na(df_full$tmax_c)]
df_full$date[is.na(df_full$tmin_c)]

#### save data
df_full %>% 
  dplyr::select(-Year, -Month) %>%
  write_csv(file.path("data", "Meteorology-Pawnee_Daily_Clean.csv"))


# break down by source
table(df_full$station_prcp)
table(df_full$station_tmax)
table(df_full$station_tmin)