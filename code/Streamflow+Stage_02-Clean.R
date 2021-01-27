# Data_Streamflow+Stage_02-Clean.R

## prep workspace
source(file.path("code", "paths+packages.R"))

## load data
daily_raw <- 
  read_csv(file.path("data", "Streamflow+Stage_Daily_Raw.csv")) %>% 
  rename(discharge_cfs = X_00060_00003, 
         discharge_cd = X_00060_00003_cd, 
         stage_ft = X_00065_00003,
         stage_cd = X_00065_00003_cd)

inst_raw <- 
  read_csv(file.path(dir_data, "streamflow_stage", "raw", "Streamflow+Stage_Inst_Raw.csv"),
           col_types = "cTdcdc") %>% 
  rename(discharge_cfs = X_00060_00000, 
         discharge_cd = X_00060_00000_cd, 
         stage_ft = X_00065_00000,
         stage_cd = X_00065_00000_cd) %>% 
  dplyr::mutate(datetime_local = with_tz(dateTime, tz = "America/Chicago"),
                Date = date(datetime_local))

#### daily
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

# for remaining missing daily data, first take the average of the instantaneous data if it exists
inst_to_daily <- 
  inst_raw %>% 
  dplyr::group_by(Date) %>% 
  dplyr::summarize(discharge_cfs_mean = mean(discharge_cfs, na.rm = T),
                   discharge_n = sum(is.finite(discharge_cfs)),
                   stage_ft_mean = mean(stage_ft, na.rm = T),
                   stage_n = sum(is.finite(stage_ft)))

dates_missing_q <- daily_clean$Date[is.na(daily_clean$discharge_cfs)]
i_missing_q_inst <- match(dates_missing_q, inst_to_daily$Date)
daily_clean$discharge_cfs[is.na(daily_clean$discharge_cfs)] <- inst_to_daily$discharge_cfs_mean[i_missing_q_inst]

dates_missing_s <- daily_clean$Date[is.na(daily_clean$stage_ft)]
i_missing_s_inst <- match(dates_missing_s, inst_to_daily$Date)
daily_clean$stage_ft[is.na(daily_clean$stage_ft)] <- inst_to_daily$stage_ft_mean[i_missing_s_inst]

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

## convert to metric
daily_clean$stage_m <- daily_clean$stage_ft*0.3048
daily_clean$stage_masl <- daily_clean$stage_ft + 592.327  # datum is in m above NGVD29, from USGS page
daily_clean$discharge_cms <- daily_clean$discharge_cfs*(0.3048^3)

## identify readings where it is just air, set to -9999
# this is based on manual inspection of graph
ggplot(daily_clean, aes(x = Date, y = stage_m, color = (discharge_cms==0))) +
  geom_point() +
  geom_hline(yintercept = 1.15)

# set to -9999 for no data
daily_clean$stage_masl[daily_clean$stage_m < 0.777 & year(daily_clean$Date) < 2010] <- -9999
daily_clean$stage_masl[daily_clean$stage_m < 0.899 & year(daily_clean$Date) > 2010] <- -9999
daily_clean$stage_m[daily_clean$stage_m < 0.777 & year(daily_clean$Date) < 2010] <- -9999
daily_clean$stage_m[daily_clean$stage_m < 0.899 & year(daily_clean$Date) > 2010] <- -9999

# plot/investigate
ggplot(subset(daily_clean, stage_m != -9999), aes(x = Date, y = stage_masl, color = (discharge_cfs==0))) +
  geom_point()

ggplot(daily_clean, aes(x = Date, y = discharge_cfs, color = (discharge_cfs==0))) +
  geom_point()

## save output
daily_clean %>% 
  dplyr::select(Date, stage_masl, stage_cd, discharge_cms, discharge_cd) %>% 
  readr::write_csv(file.path("data", "Streamflow+Stage_Daily_Clean.csv"))

#### instantaneous
# start clean data frame
inst_clean <-
  tibble::tibble(datetime_local = seq(min(inst_raw$datetime_local), max(inst_raw$datetime_local), by = "15 min"),
                 Date = date(datetime_local),
                 Year = year(datetime_local)) %>% 
  dplyr::left_join(inst_raw[,c("datetime_local", "discharge_cfs", "discharge_cd", "stage_ft", "stage_cd")], by = "datetime_local") %>%
  tidyr::replace_na(list(stage_cd = "Gapfill", discharge_cd = "Gapfill"))

# stage data unavailable until 2007
min(inst_clean$Date[is.finite(inst_clean$stage_ft)])

# data are irregularly spaced (but at least hourly) earlier on, so gapfill with simple linear interpolation
max_gap_hrs <- 24
max_gap_pts <- max_gap_hrs*4

inst_clean$discharge_cfs <- na.approx(inst_clean$discharge_cfs, maxgap = max_gap_pts)
inst_clean$stage_ft <- na.approx(inst_clean$stage_ft, maxgap = max_gap_pts)

# dates with no stage data based on daily readings - set to 2.0 which is well below the 0 discharge level
#  will be later switched to -9999
dates_nostage <- daily_clean$Date[daily_clean$stage_m==-9999]
inst_clean$stage_ft[inst_clean$Date %in% dates_nostage] <- 2.0

# how much are we missing?
j_missing_q <- which(is.na(inst_clean$discharge_cfs))
j_missing_s <- which(is.na(inst_clean$stage_ft))
j_missing_both <- intersect(j_missing_q, j_missing_s)

# pre-2007: gap-fill stage from discharge based on daily data (because no instantaneous discharge exists)
ggplot(subset(daily_raw, Date <= ymd("2007-10-02")), 
       aes(x = stage_ft, y = discharge_cfs, color = factor(year(Date)))) + 
  geom_point()

lut_sfromq_pre2007 <- 
  daily_raw %>% 
  subset(Date <= ymd("2007-10-01")) %>% 
  dplyr::select(stage_ft, discharge_cfs) %>% 
  subset(is.finite(stage_ft) & is.finite(discharge_cfs)) %>% 
  unique() %>% 
  dplyr::group_by(discharge_cfs) %>% 
  dplyr::summarize(stage_mean = mean(stage_ft)) %>% 
  arrange(discharge_cfs)

ggplot(lut_sfromq_pre2007, aes(x = stage_mean, y = discharge_cfs)) + 
  geom_point()

af_sfromq_pre2007 <- approxfun(x = lut_sfromq_pre2007$discharge_cfs,
                               y = lut_sfromq_pre2007$stage_mean,
                               yleft = 2.0, na.rm = T)

j_missing_s_pre2007 <- which(is.na(inst_clean$stage_ft) & inst_clean$Date <= ymd("2007-10-01"))
inst_clean$stage_ft[j_missing_s_pre2007] <- af_sfromq_pre2007(inst_clean$discharge_cfs[j_missing_s_pre2007])

# 2007-2015: gap-fill discharge from stage
lut_qfroms_pre2015 <- 
  inst_clean %>% 
  subset(Date > ymd("2007-10-01") & Year < 2015) %>% 
  dplyr::select(stage_ft, discharge_cfs) %>% 
  subset(is.finite(stage_ft) & is.finite(discharge_cfs)) %>% 
  unique() %>% 
  dplyr::group_by(stage_ft) %>% 
  dplyr::summarize(discharge_mean = mean(discharge_cfs)) %>% 
  arrange(stage_ft)

ggplot(lut_qfroms_pre2015, aes(x = stage_ft, y = discharge_mean)) + 
  geom_point()

# use look up table and interpolate between look-up table values
af_qfroms <- approxfun(x = lut_qfroms_pre2015$stage_ft,
                       y = lut_qfroms_pre2015$discharge_mean,
                       yleft = 0, na.rm = T)

j_missing_q_pre2015 <- which(is.na(inst_clean$discharge_cfs) & inst_clean$Date > ymd("2007-10-01") & inst_clean$Year < 2015)
inst_clean$discharge_cfs[j_missing_q_pre2015] <- af_qfroms(inst_clean$stage_ft[j_missing_q_pre2015])

# pre-2015: gap-fill stage from discharge
lut_sfromq_pre2015 <- 
  inst_clean %>% 
  subset(Date > ymd("2007-10-01") & Year < 2015) %>% 
  dplyr::select(stage_ft, discharge_cfs) %>% 
  subset(is.finite(stage_ft) & is.finite(discharge_cfs)) %>% 
  unique() %>% 
  dplyr::group_by(discharge_cfs) %>% 
  dplyr::summarize(stage_mean = mean(stage_ft, na.rm = T)) %>% 
  arrange(discharge_cfs)

af_sfromq <- approxfun(x = lut_sfromq_pre2015$discharge_cfs,
                       y = lut_sfromq_pre2015$stage_mean,
                       yleft = 2.0, na.rm = T)

j_missing_s_pre2015 <- which(is.na(inst_clean$stage_ft) & inst_clean$Date > ymd("2007-10-01") & inst_clean$Year < 2015)
inst_clean$stage_ft[j_missing_s_pre2015] <- af_sfromq(inst_clean$discharge_cfs[j_missing_s_pre2015])
inst_clean$stage_cd[j_missing_s_pre2015] <- "Gapfill"

## see what's still missing
j_missing_q <- which(is.na(inst_clean$discharge_cfs))
j_missing_s <- which(is.na(inst_clean$stage_ft))
j_missing_both <- intersect(j_missing_q, j_missing_s)
inst_clean %>% 
  tidyr::pivot_longer(cols = c("stage_ft", "discharge_cfs")) %>% 
  ggplot(aes(x = Date, y = value)) +
  geom_point() +
  facet_wrap(~name, scales = "free_y", ncol = 1)
qplot(inst_clean$Year[j_missing_q])
qplot(inst_clean$Year[j_missing_s])

## post-2015 gap fill
# stage from discharge
j_missing_s_post2015 <- which(is.na(inst_clean$stage_ft) & inst_clean$Year > 2015)
unique(inst_clean$Date[j_missing_s_post2015]) #2019-03-20 and 2019-03-21
min(inst_clean$discharge_cfs[j_missing_s_post2015])
max(inst_clean$discharge_cfs[j_missing_s_post2015])

ggplot(subset(inst_clean, Date > ymd("2019-03-13") & Date < ymd("2019-03-28") & 
                stage_cd != "Gapfill" & discharge_cd != "Gapfill"), 
       aes(x = stage_ft, y = discharge_cfs, color = factor(Year))) + 
  geom_point()

lut_sfromq_post2015 <- 
  inst_clean %>% 
  subset(Date > ymd("2019-03-13") & Date < ymd("2019-03-28") & 
           stage_cd != "Gapfill" & discharge_cd != "Gapfill") %>% 
  dplyr::select(stage_ft, discharge_cfs) %>% 
  subset(is.finite(stage_ft) & is.finite(discharge_cfs)) %>% 
  unique() %>% 
  dplyr::group_by(discharge_cfs) %>% 
  dplyr::summarize(stage_mean = mean(stage_ft, na.rm = T)) %>% 
  arrange(discharge_cfs)

af_sfromq_post2015 <- approxfun(x = lut_sfromq_post2015$discharge_cfs,
                       y = lut_sfromq_post2015$stage_mean,
                       na.rm = T)
inst_clean$stage_ft[j_missing_s_post2015] <- af_sfromq_post2015(inst_clean$discharge_cfs[j_missing_s_post2015])

ggplot(subset(inst_clean, Date > ymd("2019-03-13") & Date < ymd("2019-03-28")), 
       aes(x = datetime_local, y = stage_ft, color = stage_cd, group = 1)) + 
  geom_line()

# discharge from stage
j_missing_q_post2015 <- which(is.na(inst_clean$discharge_cfs) & inst_clean$Year > 2015)
unique(inst_clean$Date[j_missing_q_post2015]) # 2016-08-17 to 2016-09-06, 2017-12-29 to 2018-01-25

ggplot(subset(daily_clean, Date >= ymd("2016-08-10") & Date <= ymd("2016-09-13")),
       aes(x = Date, y = discharge_cfs)) +
  geom_line()
ggplot(subset(daily_clean, Date >= ymd("2016-08-10") & Date <= ymd("2016-09-13")),
       aes(x = Date, y = stage_ft)) +
  geom_line()
ggplot(subset(inst_clean, Date >= ymd("2016-08-10") & Date <= ymd("2016-09-13") & 
                stage_cd != "Gapfill" & discharge_cd != "Gapfill"), 
       aes(x = stage_ft, y = discharge_cfs, color = factor(Year))) + 
  geom_point()

lut_qfroms_2016 <- 
  inst_clean %>% 
  subset(Date >= ymd("2016-08-10") & Date <= ymd("2016-09-13")) %>% 
  dplyr::select(stage_ft, discharge_cfs) %>% 
  subset(is.finite(stage_ft) & is.finite(discharge_cfs)) %>% 
  unique() %>% 
  dplyr::group_by(stage_ft) %>% 
  dplyr::summarize(discharge_mean = mean(discharge_cfs)) %>% 
  arrange(stage_ft)

ggplot(lut_qfroms_2016, aes(x = stage_ft, y = discharge_mean)) + 
  geom_point()

af_qfroms_2016 <- approxfun(x = lut_qfroms_2016$stage_ft,
                            y = lut_qfroms_2016$discharge_mean,
                            na.rm = T)

j_missing_q_2016 <- which(is.na(inst_clean$discharge_cfs) & inst_clean$Year == 2016)
inst_clean$discharge_cfs[j_missing_q_2016] <- af_qfroms_2016(inst_clean$stage_ft[j_missing_q_2016])

ggplot(subset(inst_clean, Date >= ymd("2016-08-10") & Date <= ymd("2016-09-13")), 
       aes(x = datetime_local, y = stage_ft, color = stage_cd)) + 
  geom_point()
ggplot(subset(inst_clean, Date >= ymd("2016-08-10") & Date <= ymd("2016-09-13")), 
       aes(x = datetime_local, y = discharge_cfs, color = discharge_cd, group = 1)) + 
  geom_line()

ggplot(subset(daily_clean, Date >= ymd("2017-12-22") & Date <= ymd("2018-02-09")),
       aes(x = Date, y = discharge_cfs)) +
  geom_line()
ggplot(subset(daily_clean, Date >= ymd("2017-12-22") & Date <= ymd("2018-02-09")),
       aes(x = Date, y = stage_ft)) +
  geom_line()
ggplot(subset(inst_clean, Date >= ymd("2018-01-01") & Date <= ymd("2018-12-31") &  # only 2018 data look reasonable
                stage_cd != "Gapfill" & discharge_cd != "Gapfill"), 
       aes(x = stage_ft, y = discharge_cfs, color = factor(Year))) + 
  geom_point()

lut_qfroms_2018 <-  # use 2018
  inst_clean %>% 
  subset(Year == 2018 & stage_cd != "Gapfill" & discharge_cd != "Gapfill") %>% 
  dplyr::select(stage_ft, discharge_cfs) %>% 
  subset(is.finite(stage_ft) & is.finite(discharge_cfs)) %>% 
  unique() %>% 
  dplyr::group_by(stage_ft) %>% 
  dplyr::summarize(discharge_mean = mean(discharge_cfs)) %>% 
  arrange(stage_ft)

af_qfroms_2018 <- approxfun(x = lut_qfroms_2018$stage_ft,
                            y = lut_qfroms_2018$discharge_mean,
                            na.rm = T)

j_missing_q_2018 <- which(is.na(inst_clean$discharge_cfs) & inst_clean$Date >= ymd("2017-12-22") & inst_clean$Date <= ymd("2018-02-02"))
inst_clean$discharge_cfs[j_missing_q_2018] <- af_qfroms_2018(inst_clean$stage_ft[j_missing_q_2018])

ggplot(subset(inst_clean, Date >= ymd("2017-12-22") & Date <= ymd("2018-02-02")), 
       aes(x = datetime_local, y = stage_ft, color = stage_cd)) + 
  geom_point()
ggplot(subset(inst_clean, Date >= ymd("2017-12-22") & Date <= ymd("2018-02-02")), 
       aes(x = datetime_local, y = discharge_cfs, color = discharge_cd, group = 1)) + 
  geom_line()

## missing 2014 Q data - it should just be = 0
j_missing_q_2015 <- which(is.na(inst_clean$discharge_cfs) & inst_clean$Year == 2015)
unique(inst_clean$Date[j_missing_q_2015])
min(inst_clean$stage_ft[j_missing_q_2015]) # 2 corresponds to no stage data
max(inst_clean$stage_ft[j_missing_q_2015]) # 2 corresponds to no stage data
inst_clean$discharge_cfs[j_missing_q_2015] <- 0

## see what's still missing
j_missing_q <- which(is.na(inst_clean$discharge_cfs))
j_missing_s <- which(is.na(inst_clean$stage_ft))
j_missing_both <- intersect(j_missing_q, j_missing_s)
j_missing_justq <- j_missing_q[!(j_missing_q %in% j_missing_s)]
j_missing_justs <- j_missing_s[!(j_missing_s %in% j_missing_q)]

## missing just stage: high flows in 2007 (2007-07-16)
# extrapolate based on before/after (tried using high flows in 2019, but rating curve had shifted)
inst_clean$datetime_local[j_missing_justs]

lut_sfromq_highflow <- 
  inst_clean %>% 
  subset(Date > ymd("2007-07-15") & Date < ymd("2007-07-17") & 
           discharge_cd != "Gapfill") %>% 
  dplyr::select(stage_ft, discharge_cfs) %>% 
  subset(is.finite(stage_ft) & is.finite(discharge_cfs)) %>% 
  unique() %>% 
  dplyr::group_by(discharge_cfs) %>% 
  dplyr::summarize(stage_mean = mean(stage_ft, na.rm = T)) %>% 
  arrange(discharge_cfs)

lm_sfromq_highflow <- lm(stage_mean ~ discharge_cfs, data = lut_sfromq_highflow)
inst_clean$stage_ft[j_missing_justs] <- predict.lm(lm_sfromq_highflow, inst_clean[j_missing_justs, ])

ggplot(subset(inst_clean, Date >= ymd("2007-07-15") & Date <= ymd("2007-07-17")),
       aes(x = datetime_local, y = stage_ft, color = stage_cd)) + geom_point()

## missing just discharge: 2005-02-15 to 2005-02-16, 2005-12-20 to 2005-12-27, 2006-11-29 to 2006-22-30
# all of these have stage = 2, meaning no stage data (below bottom of stilling well) and discharge = 0
unique(inst_clean$Date[j_missing_justq])
unique(inst_clean$stage_ft[j_missing_justq])
inst_clean$discharge_cfs[j_missing_justq] <- 0

## see what's still missing:
j_missing_q <- which(is.na(inst_clean$discharge_cfs))
j_missing_s <- which(is.na(inst_clean$stage_ft))
j_missing_both <- intersect(j_missing_q, j_missing_s)
unique(inst_clean$Date[j_missing_both])

## for everything remaining, both stage and discharge are missing (though daily mean data exist)
# assign the daily mean value for noon on that date, and linearly interpolate in between
daily_withNoons <-
  subset(daily_clean, Date %in% unique(inst_clean$Date[j_missing_both])) %>% 
  dplyr::mutate(datetime_local = Date + hours(18)) %>% # add 18 since this will default to UTC
  dplyr::select(datetime_local, discharge_cfs, stage_ft)
match_daily_to_inst <- match(daily_withNoons$datetime_local, inst_clean$datetime_local)
inst_clean$stage_ft[match_daily_to_inst] <- daily_withNoons$stage_ft
inst_clean$discharge_cfs[match_daily_to_inst] <- daily_withNoons$discharge_cfs

## linearly interpolate to fill holes
inst_clean$discharge_cfs <- na.approx(inst_clean$discharge_cfs, maxgap = max_gap_pts)
inst_clean$stage_ft <- na.approx(inst_clean$stage_ft, maxgap = max_gap_pts)

ggplot(subset(inst_clean, Date > ymd("2007-01-01") & Date <= ymd("2007-02-05")), 
       aes(x = datetime_local, y = discharge_cfs)) +
  geom_point()
ggplot(subset(inst_clean, Date > ymd("2007-01-01") & Date <= ymd("2007-02-05")), 
       aes(x = datetime_local, y = stage_ft)) +
  geom_point()

## convert to metric
inst_clean$stage_m <- inst_clean$stage_ft*0.3048
inst_clean$stage_masl <- inst_clean$stage_ft + 592.327  # datum is in m above NGVD29, from USGS page
inst_clean$discharge_cms <- inst_clean$discharge_cfs*(0.3048^3)

# set to -9999 for no data (use same levels as daily data)
inst_clean$stage_masl[inst_clean$stage_m < 0.777 & year(inst_clean$Date) < 2010] <- -9999
inst_clean$stage_masl[inst_clean$stage_m < 0.899 & year(inst_clean$Date) > 2010] <- -9999
inst_clean$stage_m[inst_clean$stage_m < 0.777 & year(inst_clean$Date) < 2010] <- -9999
inst_clean$stage_m[inst_clean$stage_m < 0.899 & year(inst_clean$Date) > 2010] <- -9999

# plot/investigate
ggplot(subset(inst_clean, stage_m != -9999), aes(x = datetime_local, y = stage_m, color = (discharge_cms==0))) +
  geom_point()

ggplot(subset(inst_clean, stage_m != -9999 & year(datetime_local) < 2004), 
       aes(x = datetime_local, y = stage_m, color = (discharge_cms==0))) +
  geom_point()

ggplot(inst_clean, aes(x = datetime_local, y = discharge_cfs, color = (discharge_cfs==0))) +
  geom_point()

## inspect all data
inst_clean %>% 
  tidyr::pivot_longer(cols = c("stage_ft", "discharge_cfs")) %>% 
  ggplot(aes(x = Date, y = value)) +
  geom_point() +
  facet_wrap(~name, scales = "free_y", ncol = 1)

# compare USGS daily means to means estimated from instantaneous data
inst_daily_compare <-
  inst_clean %>% 
  #subset(stage_cd != "Gapfill" & discharge_cd != "Gapfill") %>% 
  dplyr::group_by(Date) %>% 
  dplyr::summarize(stage_m_mean = mean(stage_m, na.rm = T),
                   discharge_cms_mean = mean(discharge_cms, na.rm = T)) %>% 
  dplyr::left_join(daily_clean, by = "Date") %>% 
  dplyr::mutate(Year = year(Date))

ggplot(inst_daily_compare, aes(x = discharge_cms, y = discharge_cms_mean, color = Year < 2015)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

# most of the poor fits are from pre-2015 where we don't have good stage data
ggplot(subset(inst_daily_compare, stage_m_mean > 0), 
       aes(x = stage_m, y = stage_m_mean, color = Year < 2015)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

## save instantaneous data
inst_clean %>% 
  dplyr::select(datetime_local, stage_masl, stage_cd, discharge_cms, discharge_cd) %>% 
  readr::write_csv(file.path(dir_data, "streamflow_stage", "processed", "Streamflow+Stage_Inst_Clean.csv"))
