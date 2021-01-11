## Data_Meteorology_02-Clean.R

source(file.path("code", "paths+packages.R"))

# load output from Data_Meteorology_01-Download.R
df <- readr::read_csv(file.path("data", "Meteorology_Daily_Raw.csv"))

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
  dplyr::arrange(date)

# for temperature, fill in gaps <= 3 days by linear interpolation
df_all$tmax_c <- na.approx(df_all$tmax_c, maxgap = 3)
df_all$tmin_c <- na.approx(df_all$tmin_c, maxgap = 3)
df_all$station_tmax[is.na(df_all$station_tmax) & is.finite(df_all$tmax_c)] <- "Gapfill"
df_all$station_tmin[is.na(df_all$station_tmin) & is.finite(df_all$tmin_c)] <- "Gapfill"

# because you are mixing data sources, check to make sure never tmin > tmax
i_backwards <- which(df_all$tmin_c > df_all$tmax_c)
df_all$tmin_c[i_backwards] <- df_all$tmax_c[i_backwards]
df_all$station_tmin[i_backwards] <- "Gapfill"

# inspect for completeness
rnoaa::vis_miss(df_all)
df_all$date[is.na(df_all$prcp_mm)]
df_all$date[is.na(df_all$tmax_c)]
df_all$date[is.na(df_all$tmin_c)]
# as of 2020/1/11: all three variables are complete from at least 1903/10/28 to present

# save data
write_csv(df_all, file.path("data", "Meteorology_Daily_Clean.csv"))

# break down by source
table(df_all$station_prcp)
table(df_all$station_tmax)
table(df_all$station_prcp)
