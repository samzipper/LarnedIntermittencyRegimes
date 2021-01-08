# Data_Streamflow+Stage_02-Clean.R

## prep workspace
source(file.path("code", "paths+packages.R"))

## load raw daily data
daily_raw <- read_csv(file.path("data", "Streamflow+Stage_Daily_Raw.csv")) %>% 
  rename(discharge_cfs = X_00060_00003, 
         discharge_cd = X_00060_00003_cd, 
         stage_ft = X_00065_00003,
         stage_cd = X_00065_00003_cd)

# plot/investigate
ggplot(daily_raw, aes(x = Date, y = stage_ft, color = stage_cd)) +
  geom_point()

ggplot(daily_raw, aes(x = Date, y = discharge_cfs, color = discharge_cd)) +
  geom_point()

# for daily data, need to fill in discharge for 1998-08-01 to 1998-09-30; do this based on the first year of data
daily_missing_1998 <- subset(daily_raw, Date <= ymd("1998-09-30")) %>% 
  dplyr::select(Date, discharge_cfs, stage_ft)
daily_exists_year1 <- subset(daily_raw, Date <= ymd("1999-09-30") & is.finite(discharge_cfs) & discharge_cd == "A")

daily_missing_1998$i_match <- match(daily_missing_1998$stage_ft, daily_exists_year1$stage_ft)
daily_missing_1998$discharge_cfs <- daily_exists_year1$discharge_cfs[daily_missing_1998$i_match]

daily_combo <- dplyr::bind_rows(daily_missing_1998, daily_exists_year1) %>% 
  dplyr::select(Date, discharge_cfs, stage_ft) %>% 
  arrange(stage_ft)

# linearly interpolate within the range
i_missing <- which(is.na(daily_combo$discharge_cfs))
daily_combo$discharge_cfs[i_missing] <- 
  approx(x = daily_combo$stage_ft[-i_missing], 
       y = daily_combo$discharge_cfs[-i_missing], 
       xout = daily_combo$stage_ft[i_missing],
       rule = 1)$y

# for remainder, extrapolate using logarithmic fit for points with stage < 4.5 ft
lm_low <- lm(log10(discharge_cfs) ~ stage_ft, data = subset(daily_combo, stage_ft < 4.5))  # R2 = 0.99
i_missing_lm <- which(is.na(daily_combo$discharge_cfs))
daily_combo$discharge_cfs[i_missing_lm] <- 10^predict(lm_low, daily_combo[i_missing_lm, ])
daily_combo_filled <- subset(daily_combo, Date <= ymd("1998-09-30")) %>% arrange(Date)

# inspect fit
ggplot(daily_combo, aes(x = stage_ft, y = discharge_cfs)) +
  geom_point()

## make clean data frame
daily_clean <- 
  daily_raw
daily_clean$discharge_cd[is.na(daily_clean$discharge_cfs)] <- "Gapfill"
daily_clean$stage_cd[is.na(daily_clean$stage_ft)] <- "Gapfill"

# add first two months of discharge data
daily_clean$discharge_cfs[match(daily_combo_filled$Date, daily_clean$Date)] <- daily_combo_filled$discharge_cfs

# anything missing less than 3 days, gapfill via linear interpolation
daily_clean$discharge_cfs <- na.approx(daily_clean$discharge_cfs, maxgap = 3)
daily_clean$stage_ft <- na.approx(daily_clean$stage_ft, maxgap = 3)

## two remaining chunks of stage data missing: 2699:2706 and 3093:3101
which(is.na(daily_clean$stage_ft))
# for 2699:2706, discharge is 0 the whole time and stage is basically the same before (2.50)/after (2.51), so just set to 2.51
daily_clean$stage_ft[2699:2706] <- 2.51

# for 3093:3101, it is in the middle of a recession; plot 10 days before/after
ggplot(daily_clean[3083:3111,], aes(x = Date, y = stage_ft)) + geom_point()

# look like linear interpolation will do fine, use that
daily_clean$stage_ft[3083:3111] <- na.approx(daily_clean$stage_ft[3083:3111])

# plot/investigate
ggplot(daily_clean, aes(x = Date, y = stage_ft, color = stage_cd)) +
  geom_point()

ggplot(daily_clean, aes(x = Date, y = discharge_cfs, color = discharge_cd)) +
  geom_point()
