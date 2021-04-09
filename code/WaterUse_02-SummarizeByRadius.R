## WaterUse_02-SummarizeByRadius.R
# This script summarizes annual water use within a given distance of the Larned Research Site.
# The raw data includes all points of diversion within 10 km.

## prep workspace
source(file.path("code", "paths+packages.R"))

# read points of diversion and annual water use
sf_pdiv <- sf::st_read(file.path("data", "WaterUse_DiversionLocations.gpkg"))
df_wuse <- readr::read_csv(file.path("data", "WaterUse_AnnualByPDIV.csv"))

# set radius of interest
rad_m_all <- c(100, 500, 1000, seq(2000, 10000, 1000))
for (rad_m in rad_m_all){
  # subset pdiv within that distance
  pdiv_r <- 
    sf_pdiv %>% 
    subset(dist_to_gage_m <= rad_m) %>% 
    dplyr::pull(PDIV_ID)
  
  # summarize water use for those pdiv
  df_wuse_yr_r <- 
    df_wuse %>% 
    subset(PDIV_ID %in% pdiv_r) %>% 
    dplyr::group_by(Year) %>% 
    dplyr::summarize(WaterUse_m3 = sum(WaterUse_m3)) %>%
    dplyr::mutate(WaterUse_z = scale(WaterUse_m3),
                  radius_m = rad_m)
  
  if (rad_m == rad_m_all[1]){
    df_wuse_yr <- df_wuse_yr_r
  } else {
    df_wuse_yr <- dplyr::bind_rows(df_wuse_yr, df_wuse_yr_r)
  }
}

# save output
readr::write_csv(df_wuse_yr, file.path("data", "WaterUse_AnnualByRadius.csv"))

# look at water use by volume for a given radius
ggplot(subset(df_wuse_yr, radius_m = 5000), aes(x = Year, y = WaterUse_m3)) + geom_point() + geom_line()

# look at water use as z-score for all radius
ggplot(df_wuse_yr, aes(x = Year, y = WaterUse_z, color = factor(radius_m))) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_point() + geom_line()
