# Data_Streamflow+Stage_01-Download.R

## prep workspace
source(file.path("code", "paths+packages.R"))

## download data
# daily
pCodes = c("00060", "00065") # discharge = 00060, stage = 00065
daily_raw <- 
  dataRetrieval::readNWISdv(siteNumbers = USGS_gage, 
                            parameterCd = c("00060", "00065"),
                            startDate = "",
                            endDate = last_date,
                            statCd = "00003") # daily mean

# instantaneous - this takes a long time!
inst_raw <-
  dataRetrieval::readNWISuv(siteNumbers = USGS_gage, 
                            parameterCd = c("00060", "00065"),
                            startDate = "",
                            endDate = last_date)

## save data
write_csv(daily_raw, file.path("data", "Streamflow+Stage_Daily_Raw_new.csv"))

inst_raw %>% 
  dplyr::select(-agency_cd, -tz_cd) %>% 
  write_csv(file.path(dir_data, "streamflow_stage", "raw", "Streamflow+Stage_Inst_Raw.csv"))  # too big for git repository
