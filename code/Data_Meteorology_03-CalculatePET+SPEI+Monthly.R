## Data_Meteorology_03-CalculatePET+SPEI+Monthly.R

source(file.path("code", "paths+packages.R"))

# load data
df_daily <- 
  readr::read_csv(file.path("data", "Meteorology_Daily_Clean.csv"))

# prep for ET calculations
# using evapotranspiration package: https://cran.r-project.org/package=Evapotranspiration
df_forET <-
  df_daily %>% 
  dplyr::mutate(Year = year(date),
         Month = month(date),
         Day = day(date)) %>% 
  subset(Year >= 1904) %>% 
  dplyr::rename(Tmax = tmax_c, Tmin = tmin_c, Precip = prcp_mm)

df_preppedforET <- 
  ReadInputs(varnames = c("Tmax", "Tmin", "Precip"),
             climatedata = df_forET,
             stopmissing = c(1, 1, 1))

# sum to monthly
df_mo <- 
  df_daily %>% 
  dplyr::group_by(year, month) %>% 
  dplyr::summarize(prcpSum_mm = sum(prcp_mm),
                   tmaxMean_mm = mean(tmax_c),
                   tminMean_mm = mean(tmin_c))

# calculate PET for SPEI
