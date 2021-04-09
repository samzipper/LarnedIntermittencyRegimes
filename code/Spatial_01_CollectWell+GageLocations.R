## Spatial_01_CollectWell+GageLocations.R
# This script collects spatial data about the monitoring wells at the site.

## prep workspace
source(file.path("code", "paths+packages.R"))

# load survey
df_wells <- 
  file.path(dir_data, "survey", "LarnedWellInfo.csv") %>% 
  readr::read_csv()

# convert to sf
sf_wells <- 
  df_wells %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4269)

ggplot(sf_wells, aes(color = (TOC_elevation_ft - TOC_above_ls_ft))) + 
  geom_sf()

# save as output
sf::st_write(sf_wells, file.path("data", "Spatial_Well+GageLocations.gpkg"))
