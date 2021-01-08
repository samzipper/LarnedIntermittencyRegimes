# Data_Streamflow+Stage_01-Download.R

## prep workspace
source(file.path("code", "paths+packages.R"))

## download data
# daily
pCodes = c("00060", "00065") # discharge = 00060, stage = 00065
df_daily <- 
  dataRetrieval::readNWISdv(siteNumbers = USGS_gage, 
                            parameterCd = c("00060", "00065"),
                            startDate = "",
                            endDate = last_date,
                            statCd = "00003") # daily mean

# instantaneous - this takes a long time!
df_inst <-
  dataRetrieval::readNWISuv(siteNumbers = USGS_gage, 
                            parameterCd = c("00060", "00065"),
                            startDate = "",
                            endDate = last_date)

## save data
write_csv(df_daily, file.path("data", "Streamflow+Stage_Daily_Raw.csv"))
write_csv(df_inst, file.path(dir_data, "streamflow_stage", "raw", "Streamflow+Stage-Inst_Raw.csv"))  # too big for git repository
