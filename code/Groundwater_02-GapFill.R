## Groundwater_02-GapFill-LWPH4.R
# This script is intended to gap fill groundwater levels from the LWPH4 wells.

## prep workspace
source(file.path("code", "paths+packages.R"))

# load daily data
df_day <- readr::read_csv(file.path("data", "Groundwater_WaterLevels-DailyGHCN.csv"),
                          col_types = "Ddddddddddddddddddd")

# prep data frame for LWPH4
LWPH4_first <- min(df_day$date_ghcn[is.finite(df_day$LWPH4c) | is.finite(df_day$LWPH4a) | is.finite(df_day$LWPH4b)])
LWPH4_last <- max(df_day$date_ghcn[is.finite(df_day$LWPH4c) | is.finite(df_day$LWPH4a) | is.finite(df_day$LWPH4b)])

df_LWPH4 <- df_day[df_day$date_ghcn >= LWPH4_first & df_day$date_ghcn <= LWPH4_last, ]

# inspect data
ggplot(df_LWPH4, aes(x = date_ghcn, y = LWPH4a)) + geom_point()

# there is some bad data in LWPH4a... set to NA
df_LWPH4$LWPH4a[df_LWPH4$date_ghcn >= ymd("2016-06-13") & df_LWPH4$date_ghcn <= ymd("2016-10-24")] <- NA

## gap-fill LWPH4c (High Plains Aquifer well)
df_LWPH4$LWPH4c_source <- "Observed"

# regression for group 1
LWPH4c_missing <- which(is.na(df_LWPH4$LWPH4c)) # two groups: 1343-1539, 1764-1945
df_LWPH4[1243:1639, ] %>% 
  subset(LWPH4c_source == "Observed") %>% 
  ggplot(aes(x = LEA5, y = LWPH4c, color = date_ghcn)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
LWPH4c_group1 <- LWPH4c_missing[LWPH4c_missing %in% 1243:1639]
lm_LWPH4c <- lm(LWPH4c ~ LEA5, data = df_LWPH4[1243:1639, ])
df_LWPH4$LWPH4c[LWPH4c_group1] <- predict(lm_LWPH4c, newdata = df_LWPH4[LWPH4c_group1, ])
df_LWPH4$LWPH4c_source[LWPH4c_group1] <- "Gapfill"

# 1 gap remaining within group 1- linearly interpolate because it is a pretty linear decrease
LWPH4c_missing <- which(is.na(df_LWPH4$LWPH4c)) # two groups: 1343-1539, 1764-1945
LWPH4c_group1a <- LWPH4c_missing[LWPH4c_missing %in% 1243:1460]
df_LWPH4[1375:1467, ] %>% 
  ggplot(aes(x = date_ghcn, y = LWPH4c, color = LWPH4c_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4c_1a <- lm(LWPH4c ~ date_ghcn, data = df_LWPH4[1375:1467, ])
df_LWPH4$LWPH4c[LWPH4c_group1a] <- predict(lm_LWPH4c_1a, newdata = df_LWPH4[LWPH4c_group1a, ])
df_LWPH4$LWPH4c_source[LWPH4c_group1a] <- "Gapfill"

LWPH4c_missing <- which(is.na(df_LWPH4$LWPH4c)) # two groups: 1532-1539, 1764-1945
LWPH4c_group1b <- LWPH4c_missing[LWPH4c_missing %in% 1512:1559]
df_LWPH4[1512:1559, ] %>% 
  ggplot(aes(x = date_ghcn, y = LWPH4c, color = LWPH4c_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4c_1b <- lm(LWPH4c ~ date_ghcn, data = df_LWPH4[1531:1540, ])
df_LWPH4$LWPH4c[LWPH4c_group1b] <- predict(lm_LWPH4c_1b, newdata = df_LWPH4[LWPH4c_group1b, ])
df_LWPH4$LWPH4c_source[LWPH4c_group1b] <- "Gapfill"

LWPH4c_missing <- which(is.na(df_LWPH4$LWPH4c)) # one group: 1764-1945. regression should include following pumping season since it is during drawdown period
LWPH4c_group1c <- LWPH4c_missing[LWPH4c_missing %in% 1764:2410]
df_LWPH4[1764:2410, ] %>% 
  subset(LWPH4c_source == "Observed") %>% 
  ggplot(aes(x = LEA5, y = LWPH4c, color = date_ghcn)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4c_1c <- lm(LWPH4c ~ LEA5, data = df_LWPH4[1764:2410, ])
df_LWPH4$LWPH4c[LWPH4c_group1c] <- predict(lm_LWPH4c_1c, newdata = df_LWPH4[LWPH4c_group1c, ])
df_LWPH4$LWPH4c_source[LWPH4c_group1c] <- "Gapfill"

ggplot(df_LWPH4, aes(x = date_ghcn, y = LWPH4c, color = LWPH4c_source)) + 
  geom_point()

## gap-fill LWPH4b (deep alluvial well)
df_LWPH4$LWPH4b_source <- "Observed"

LWPH4b_missing <- which(is.na(df_LWPH4$LWPH4b)) # 3177:3302, 4184:4220

LWPH4b_group1a <- LWPH4b_missing[LWPH4b_missing %in% 3177:3302]
df_LWPH4[3077:3402, ] %>% 
  ggplot(aes(x = LEA4, y = LWPH4b, color = LWPH4b_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4b_1a <- lm(LWPH4b ~ LEA4, data = df_LWPH4[3077:3402, ])
df_LWPH4$LWPH4b[LWPH4b_group1a] <- predict(lm_LWPH4b_1a, newdata = df_LWPH4[LWPH4b_group1a, ])
df_LWPH4$LWPH4b_source[LWPH4b_group1a] <- "Gapfill"

LWPH4b_missing <- which(is.na(df_LWPH4$LWPH4b)) # 3177:3302, 4184:4220
LWPH4b_group1b <- LWPH4b_missing[LWPH4b_missing %in% 4184:4220]
df_LWPH4[4084:4320, ] %>% 
  ggplot(aes(x = LEA4, y = LWPH4b, color = LWPH4b_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4b_1b <- lm(LWPH4b ~ LEA4, data = df_LWPH4[4084:4320, ])
df_LWPH4$LWPH4b[LWPH4b_group1b] <- predict(lm_LWPH4b_1b, newdata = df_LWPH4[LWPH4b_group1b, ])
df_LWPH4$LWPH4b_source[LWPH4b_group1b] <- "Gapfill"

ggplot(df_LWPH4, aes(x = date_ghcn, y = LWPH4b, color = LWPH4b_source)) + 
  geom_point()

## gap-fill LWPH4a (shallow alluvial well)
df_LWPH4$LWPH4a_source <- "Observed"

LWPH4a_missing <- which(is.na(df_LWPH4$LWPH4a)) # 1662-1672, 2040-2058, 2402-2438, 2582-2596, 2745-2793, 3095-3122, 3481-3513, 3681-3711
df_LWPH4 %>% 
  ggplot(aes(x = LWPH4b, y = LWPH4a, color = LWPH4a_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")

lm_LWPH4a <- lm(LWPH4a ~ LWPH4b, data = df_LWPH4)
df_LWPH4$LWPH4a[LWPH4a_missing] <- predict(lm_LWPH4a, newdata = df_LWPH4[LWPH4a_missing, ])
df_LWPH4$LWPH4a_source[LWPH4a_missing] <- "Gapfill"

ggplot(df_LWPH4, aes(x = date_ghcn, y = LWPH4a, color = LWPH4a_source)) + 
  geom_point()

## check to make sure everything full
sum(is.na(df_LWPH4$LWPH4a))
sum(is.na(df_LWPH4$LWPH4b))
sum(is.na(df_LWPH4$LWPH4c))

# save
df_LWPH4 %>% 
  dplyr::select(date_ghcn, starts_with("LWPH4")) %>% 
  readr::write_csv(file.path("data", "Groundwater_WaterLevels-DailyGHCN-LWPH4-GapFill.csv"))

# plot
df_long <- tibble::tibble(date_ghcn = rep(df_LWPH4$date_ghcn, 3),
                          waterlevel_m = c(df_LWPH4$LWPH4a, df_LWPH4$LWPH4b, df_LWPH4$LWPH4c),
                          source = c(df_LWPH4$LWPH4a_source, df_LWPH4$LWPH4b_source, df_LWPH4$LWPH4c_source),
                          well = c(rep("LWPH4a", dim(df_LWPH4)[1]), rep("LWPH4b", dim(df_LWPH4)[1]), rep("LWPH4c", dim(df_LWPH4)[1])))

ggplot(subset(df_long, lubridate::year(date_ghcn) < 2015), 
       aes(x = date_ghcn, y = waterlevel_m, color = well)) +
  geom_line()
