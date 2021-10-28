# Data_Streamflow+Stage_01a-Update.R
# This script updates and overwrites existing data, for example to replace provisional data with new data.

## prep workspace
source(file.path("code", "paths+packages.R"))

## load existing data
daily_raw_old <- read_csv(file.path("data", "Streamflow+Stage_Daily_Raw.csv"))

inst_raw_old <- read_csv(file.path(dir_data, "streamflow_stage", "raw", "Streamflow+Stage_Inst_Raw.csv"),
                         col_types = "cTdcdc")

## download data
update_start_date <- "2020-12-07" # daily_raw_old$Date[min(which(daily_raw_old$X_00060_00003_cd == "P"))]
  
# daily
pCodes = c("00060", "00065") # discharge = 00060, stage = 00065
daily_raw <- 
  dataRetrieval::readNWISdv(siteNumbers = USGS_gage, 
                            parameterCd = c("00060", "00065"),
                            startDate = update_start_date,
                            endDate = last_date,
                            statCd = "00003") # daily mean

# instantaneous - this takes a long time!
inst_raw <-
  dataRetrieval::readNWISuv(siteNumbers = USGS_gage, 
                            parameterCd = c("00060", "00065"),
                            startDate = update_start_date,
                            endDate = last_date)

# replace old data with new data
d_match <- match(daily_raw$Date, daily_raw_old$Date)
daily_raw_old$X_00060_00003[d_match] <- daily_raw$X_00060_00003
daily_raw_old$X_00060_00003_cd[d_match] <- daily_raw$X_00060_00003_cd
daily_raw_old$X_00065_00003[d_match] <- daily_raw$X_00065_00003
daily_raw_old$X_00065_00003_cd[d_match] <- daily_raw$X_00065_00003_cd

i_match <- match(inst_raw$dateTime, inst_raw_old$dateTime)
inst_raw_old$X_00060_00000[i_match] <- inst_raw$X_00060_00000
inst_raw_old$X_00060_00000_cd[i_match] <- inst_raw$X_00060_00000_cd
inst_raw_old$X_00065_00000[i_match] <- inst_raw$X_00065_00000
inst_raw_old$X_00065_00000_cd[i_match] <- inst_raw$X_00065_00000_cd


## save data
write_csv(daily_raw_old, file.path("data", "Streamflow+Stage_Daily_Raw.csv"))

write_csv(inst_raw_old, file.path(dir_data, "streamflow_stage", "raw", "Streamflow+Stage_Inst_Raw.csv"))  # too big for git repository
