## Streamflow-Upstream_01_Download.R
# Collect streamflow for upstream gages on the Arkansas and Pawnee Rivers.

source(file.path("code", "paths+packages.R"))

# set gage IDs
# list of stations, upstream to downstream
stations <- tibble::tibble(gageid = c("07134180", "07137500", "07138000", "07138020",
                                      "07138070", "07139000", "07139500", 
                                      "07141200", "07140850", "07141000",
                                      "07140900", "07140890", "07140880"),
                           gagename = c("Granada (CO)", "Coolidge", "Syracuse", "Kendall",
                                        "Deerfield", "Garden City", "Dodge City", 
                                        "Pawnee at Rozel", "Pawnee near Burdett", "Buckner at Hanston",
                                        "Buckner near Jetmore", "Buckner below Horsethief", "Buckner above Horsethief"))

# download data for each site
for (s in 1:dim(stations)[1]){
  df_site <- dataRetrieval::readNWISdv(stations$gageid[s], c("00060", "00065"),
                                       "1988-10-01","2021-09-30")
  
  if (s == 1){
    df_all <- df_site
  } else {
    df_all <- dplyr::bind_rows(df_all, df_site)
  }
  
  print(paste0(s, " complete"))
}

# rename columns

# add site name
df_out <- 
  df_all %>% 
  dplyr::rename(gageid = site_no, discharge_cfs = X_00060_00003, discharge_cd = X_00060_00003_cd, 
                stage_ft = X_00065_00003, stage_cd = X_00065_00003_cd) %>% 
  dplyr::left_join(stations, by = "gageid") %>% 
  dplyr::mutate(discharge_cms = round(discharge_cfs*(0.3048^3), 3),
                stage_m = round(stage_ft*0.3048, 2))

# save output
df_out %>% 
  dplyr::select(gageid, gagename, Date, discharge_cms, discharge_cd, stage_m, stage_cd) %>% 
  write_csv(file.path("data", "Streamflow-Upstream_Daily_Raw.csv"))
