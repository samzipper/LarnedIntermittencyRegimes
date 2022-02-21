## Streamflow-Upstream_02_Clean.R
# Clean - add Larned data and calculate discharge from stage for Dodge City

source(file.path("code", "paths+packages.R"))

# load data
df_in <- read_csv(file.path("data", "Streamflow-Upstream_Daily_Raw.csv"))

# make a data frame that has all of the necessary dates for all gages
dates <- sort(unique(df_in$Date))
gages <- unique(df_in$gagename)

# full data frame
df_all <- tibble(Date = rep(dates, length(gages)),
                 gagename = rep(gages, each = length(dates)))
df_raw <- left_join(df_all, df_in, by = c("Date", "gagename"))

# for dodge city, gap-fill discharge in recent years. all 0s
df_dodge <- subset(df_raw, gagename == "Dodge City")

ggplot(df_dodge, aes(x = Date, y = stage_m)) +
  geom_line()

ggplot(df_dodge, aes(x = discharge_cms, y = stage_m)) +
  geom_point()

sum(is.na(df_dodge$discharge_cms))
sum(is.na(df_raw$discharge_cms))

df_dodge$discharge_cms[is.na(df_dodge$discharge_cms)] <- 0

ggplot(df_dodge, aes(x = Date, y = discharge_cms)) +
  geom_line()

df_raw$discharge_cms[is.na(df_raw$discharge_cms) & df_raw$gagename == "Dodge City"] <- 0

# pawnee river
df_burdett <- subset(df_raw, gagename == "Pawnee near Burdett")
which(is.na(df_burdett$discharge_cms))

df_rozel <- subset(df_raw, gagename == "Pawnee at Rozel")
which(is.na(df_rozel$discharge_cms))

## identify dry periods
for (g in unique(df_raw$gagename)){
  df_g <- subset(df_raw, gagename == g)
  
  # identify start and end of no-flow periods
  dates_noflow <- df_g$Date[df_g$discharge_cms==0]
  dates_diffs <- c(999, diff(dates_noflow))  # subtract dates from previous date; put first no-flow as 999
  dates_starts <- dates_noflow[which(dates_diffs != 1)]
  dates_ends <- c(dates_noflow[which(dates_diffs != 1)-1], max(dates_noflow))  # for the last dry period, it will end on the max date where flow=0
  df_dry_periods <- tibble::tibble(gagename = g,
                                   first_noflow_date = dates_starts,
                                   last_noflow_date = dates_ends,
                                   total_noflow_days = as.numeric(dates_ends - dates_starts) + 1)
  
  if (g == df_raw$gagename[1]){
    df_dry_all <- df_dry_periods
  } else {
    df_dry_all <- bind_rows(df_dry_all, df_dry_periods)
  }
}

# load Larned data already collected and cleaned and add to daily cleaned data
df_larned <- 
  read_csv(file.path("data", "Streamflow+Stage_Daily_Clean.csv")) %>% 
  rename(stage_m = stage_masl) %>% 
  mutate(gageid = "07141220",
         gagename = "Larned") %>% 
  dplyr::select(gageid, gagename, Date, discharge_cms, discharge_cd, stage_m, stage_cd)

# save output
df_out <- 
  bind_rows(df_raw, df_larned)

write_csv(df_out, file.path("data", "Streamflow-Upstream_Daily_Clean.csv"))
write_csv(df_dry_all, file.path("data", "Streamflow-Upstream_DryPeriods.csv"))
