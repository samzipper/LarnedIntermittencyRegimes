## Table_MetDataSource.R
# Compile a table of meteorological data source used in daily record.
# Requires output from Meteorology_02-Clean.R

source(file.path("code", "paths+packages.R"))

## load data
df_met <- readr::read_csv(file.path("data", "Meteorology_Daily_Clean.csv"))

# same as in script Meteorology_01-Download.R
station_priority <-
  tibble::tibble(StationName = c("LARNED NUMBER 2", "LARNED", 
                                 "MACKSVILLE 8 NNE", "MACKSVILLE 8 NNE", "GARFIELD",
                                 "SANFORD 1 NW", "ROZEL",
                                 "GREAT BEND 3 W", "HUDSON", 
                                 "ALBERT 5 SE", "ALBERT", "HOISINGTON",
                                 "TROUSDALE 1 NE", "BISON 3 NW"),
                 Station = c("USC00144531", "USC00144530", 
                             "USC00144932", "USC00149121", "USC00142993",
                             "USC00147192", "USC00147021",
                             "USC00143218", "USC00143847", 
                             "USC00140119", "USC00140117", "USC00143679",
                             "USC00148245", "USC00140865"),
                 Priority = seq(1, length(StationName)))

## summarize: number of records by station
df_met %>% 
  dplyr::select(station_prcp, station_tmax, station_tmin) %>% 
  tidyr::pivot_longer(everything()) %>% 
  dplyr::group_by(name, value) %>% 
  dplyr::summarize(count = n()) %>% 
  tidyr::pivot_wider(id_cols = value, values_from = count) %>% 
  dplyr::left_join(station_priority, by = c("value" = "Station")) %>% 
  tidyr::replace_na(list(station_prcp = 0, station_tmax = 0, station_tmin = 0,
                         StationName = "Gapfilled")) %>% 
  dplyr::arrange(-station_prcp) %>% 
  dplyr::select(value, StationName, station_prcp, station_tmax, station_tmin) %>% 
  readr::write_csv(file.path("figures+tables", "Table_MetDataSource.csv"))
