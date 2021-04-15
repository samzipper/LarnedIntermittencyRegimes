## WaterUse_01-Collect+Clean.R
# This script collects data from the WIMAS water use database for
# wells near the Arkansas River field site.

## prep workspace
source(file.path("code", "paths+packages.R"))

## load monitoring well locations
sf_wells <- sf::st_read(file.path("data", "Spatial_Well+GageLocations.gpkg"))

## load water use data
sf_wuse <- 
  file.path(dir_data, "water_use", "processed", "WIMAS_WaterUse_1990to2019_Within10km.gpkg") %>% 
  sf::st_read()

## calculate distance from wells to site. just use the USGS gage to represent the site.
sf_gage <- 
  subset(sf_wells, Well == "USGS_07141220") %>% 
  sf::st_transform(sf::st_crs(sf_wuse))

sf_wuse$dist_to_gage_m <- as.numeric(sf::st_distance(sf_wuse, sf_gage)[,1])

ggplot(sf_wuse, aes(color = dist_to_gage_m)) + geom_sf()

## separate pumping well info into:
# - gpkg file with PDIV locations and other well data that does not change by year
# - csv file with annual water use, acres irrigated, etc.
sf_wuse %>% 
  dplyr::select(PDIV_ID, long_nad83, long_nad83, source, dist_to_gage_m, hpa_aquifer) %>% 
  sf::st_write(file.path("data", "WaterUse_DiversionLocations.gpkg"), append = F)

df_wuse <-
  sf_wuse %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(PDIV_ID, starts_with("AF_USED_"), -contains("REC"), -contains("IRR"), 
                -contains("STK"), -contains("MUN"), -contains("IND")) %>% 
  tidyr::pivot_longer(-PDIV_ID, values_to = "WaterUse_af") %>% 
  dplyr::mutate(Year = as.numeric(stringr::str_sub(name, start = -4)),
                WaterUse_m3 = WaterUse_af*43560*(0.3048^3)) %>% 
  dplyr::select(PDIV_ID, Year, WaterUse_m3)

df_wuse %>% 
  readr::write_csv(file.path("data", "WaterUse_AnnualByPDIV.csv"))

# plot to investigate over time
df_wuse %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(WaterUse_m3_total = sum(WaterUse_m3)) %>% 
  ggplot(aes(x = Year, y = WaterUse_m3_total)) +
  geom_point() + geom_line()
