## Streamflow-Upstream_02_Clean.R
# Clean - add Larned data and calculate discharge from stage for Dodge City

source(file.path("code", "paths+packages.R"))

# load data
df_raw <- read_csv(file.path("data", "Streamflow-Upstream_Daily_Raw.csv"))

# for dodge city, gap-fill discharge in recent years. all 0s
df_dodge <- 
  df_raw %>% 
  subset(gagename == "Dodge City")

ggplot(df_dodge, aes(x = Date, y = stage_m)) +
  geom_line()

ggplot(df_dodge, aes(x = discharge_cms, y = stage_m)) +
  geom_point()

sum(is.na(df_dodge$discharge_cms))
sum(is.na(df_raw$discharge_cms))

df_dodge$discharge_cms[is.na(df_dodge$discharge_cms)] <- 0

ggplot(df_dodge, aes(x = Date, y = discharge_cms)) +
  geom_line()

df_raw$discharge_cms[is.na(df_raw$discharge_cms) & df_raw$gagename == "Dodge City"] <- 0

# deerfield - beginning of record is NAs, just delete
df_raw <- subset(df_raw, is.finite(discharge_cms))

# load Larned data already collected and cleaned
df_larned <- 
  read_csv(file.path("data", "Streamflow+Stage_Daily_Clean.csv")) %>% 
  rename(stage_m = stage_masl) %>% 
  mutate(gageid = "07141220",
         gagename = "Larned") %>% 
  dplyr::select(gageid, gagename, Date, discharge_cms, discharge_cd, stage_m, stage_cd)

# save output
df_out <- 
  bind_rows(df_raw, df_larned)

write_csv(df_out, file.path("data", "Streamflow-Upstream_Daily_Clean.csv"))
