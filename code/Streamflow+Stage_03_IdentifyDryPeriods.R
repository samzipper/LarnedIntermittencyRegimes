## Streamflow+Stage_03_IdentifyDryPeriods.R
# This script loads daily streamflow data and identifies dry periods.

source(file.path("code", "paths+packages.R"))

## load data
df_day <- readr::read_csv(file.path("data", "Streamflow+Stage_Daily_Clean.csv"))

## identify start and end of no-flow periods
dates_noflow <- df_day$Date[df_day$discharge_cms==0]
dates_diffs <- c(999, diff(dates_noflow))  # subtract dates from previous date; put first no-flow as 999
dates_starts <- dates_noflow[which(dates_diffs != 1)]
dates_ends <- c(dates_noflow[which(dates_diffs != 1)-1], max(dates_noflow))  # for the last dry period, it will end on the max date where flow=0
df_dry_periods <- tibble::tibble(first_noflow_date = dates_starts,
                                 last_noflow_date = dates_ends,
                                 total_noflow_days = as.numeric(dates_ends - dates_starts) + 1)


## save data
write_csv(df_dry_periods, file.path("data", "Streamflow_DryPeriods.csv"))
