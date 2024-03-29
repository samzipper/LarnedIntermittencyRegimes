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

## inspect/fix weird data in LWPH4a
df_stream <- readr::read_csv(file.path("data", "Streamflow+Stage_DailyGHCN.csv"))

df_day %>% 
  left_join(df_stream, by = "date_ghcn") %>% 
  dplyr::select(date_ghcn, LWPH4a, LWPH4b, LWPH4c, stage_masl) %>% 
  pivot_longer(-date_ghcn, values_to = "level_masl") %>% 
  subset(date_ghcn >= ymd("2015-01-01") & date_ghcn <= ymd("2018-01-01")) %>% 
  ggplot(aes(x = date_ghcn, y = level_masl, color = name)) +
  geom_line() +
  scale_color_manual(values = c(col.cat.red, col.cat.blu, col.cat.org, "black"))

# there is some bad data in LWPH4a... set to NA
df_LWPH4$LWPH4a[df_LWPH4$date_ghcn >= ymd("2016-05-16") & df_LWPH4$date_ghcn <= ymd("2016-10-23")] <- NA

## gap-fill LWPH4c (High Plains Aquifer well)
df_LWPH4$LWPH4c_source <- "Observed"

# regression for group 1
LWPH4c_missing <- which(is.na(df_LWPH4$LWPH4c)) # three groups: 1343-1539, 1764-1945, 5223-end
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
LWPH4c_missing <- which(is.na(df_LWPH4$LWPH4c))
LWPH4c_group1a <- LWPH4c_missing[LWPH4c_missing %in% 1243:1460]
df_LWPH4[1375:1467, ] %>% 
  ggplot(aes(x = date_ghcn, y = LWPH4c, color = LWPH4c_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4c_1a <- lm(LWPH4c ~ date_ghcn, data = df_LWPH4[1375:1467, ])
df_LWPH4$LWPH4c[LWPH4c_group1a] <- predict(lm_LWPH4c_1a, newdata = df_LWPH4[LWPH4c_group1a, ])
df_LWPH4$LWPH4c_source[LWPH4c_group1a] <- "Gapfill"

LWPH4c_missing <- which(is.na(df_LWPH4$LWPH4c))
LWPH4c_group1b <- LWPH4c_missing[LWPH4c_missing %in% 1512:1559]
df_LWPH4[1512:1559, ] %>% 
  ggplot(aes(x = date_ghcn, y = LWPH4c, color = LWPH4c_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4c_1b <- lm(LWPH4c ~ date_ghcn, data = df_LWPH4[1531:1540, ])
df_LWPH4$LWPH4c[LWPH4c_group1b] <- predict(lm_LWPH4c_1b, newdata = df_LWPH4[LWPH4c_group1b, ])
df_LWPH4$LWPH4c_source[LWPH4c_group1b] <- "Gapfill"

LWPH4c_missing <- which(is.na(df_LWPH4$LWPH4c)) # two groups: 1764-1945, 5223-end
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

LWPH4c_missing <- which(is.na(df_LWPH4$LWPH4c)) # one group: 5223-end
LWPH4c_group1d <- LWPH4c_missing[LWPH4c_missing %in% 5133:5352]
df_LWPH4[5133:5352, ] %>% 
  subset(LWPH4c_source == "Observed") %>% 
  ggplot(aes(x = LEA5, y = LWPH4c, color = date_ghcn)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4c_1d <- lm(LWPH4c ~ LEA5, data = df_LWPH4[5133:5352, ])
df_LWPH4$LWPH4c[LWPH4c_group1d] <- predict(lm_LWPH4c_1d, newdata = df_LWPH4[LWPH4c_group1d, ])
df_LWPH4$LWPH4c_source[LWPH4c_group1d] <- "Gapfill"

ggplot(df_LWPH4, aes(x = date_ghcn, y = LWPH4c, color = LWPH4c_source)) + 
  geom_point()

## gap-fill LWPH4b (deep alluvial well)
df_LWPH4$LWPH4b_source <- "Observed"

LWPH4b_missing <- which(is.na(df_LWPH4$LWPH4b)) # 3177:3302, 4184:4220, 4670:4758, 5213:5352

LWPH4b_group1a <- LWPH4b_missing[LWPH4b_missing %in% 3177:3302]
df_LWPH4[3077:3402, ] %>% 
  ggplot(aes(x = LEA4, y = LWPH4b, color = LWPH4b_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4b_1a <- lm(LWPH4b ~ LEA4, data = df_LWPH4[3077:3402, ])
df_LWPH4$LWPH4b[LWPH4b_group1a] <- predict(lm_LWPH4b_1a, newdata = df_LWPH4[LWPH4b_group1a, ])
df_LWPH4$LWPH4b_source[LWPH4b_group1a] <- "Gapfill"

LWPH4b_missing <- which(is.na(df_LWPH4$LWPH4b)) # 4184:4220, 4670:4758, 5213:5352
LWPH4b_group1b <- LWPH4b_missing[LWPH4b_missing %in% 4184:4220]
df_LWPH4[4084:4320, ] %>% 
  ggplot(aes(x = LEA4, y = LWPH4b, color = LWPH4b_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4b_1b <- lm(LWPH4b ~ LEA4, data = df_LWPH4[4084:4320, ])
df_LWPH4$LWPH4b[LWPH4b_group1b] <- predict(lm_LWPH4b_1b, newdata = df_LWPH4[LWPH4b_group1b, ])
df_LWPH4$LWPH4b_source[LWPH4b_group1b] <- "Gapfill"

LWPH4b_missing <- which(is.na(df_LWPH4$LWPH4b)) # 4670:4758, 5213:5352
LWPH4b_group1c <- LWPH4b_missing[LWPH4b_missing %in% 4670:4758]
df_LWPH4[4305:5123, ] %>% 
  ggplot(aes(x = LWPH6, y = LWPH4b, color = LWPH4b_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4b_1c <- lm(LWPH4b ~ LWPH6, data = df_LWPH4[4305:5123, ])
df_LWPH4$LWPH4b[LWPH4b_group1c] <- predict(lm_LWPH4b_1c, newdata = df_LWPH4[LWPH4b_group1c, ])
df_LWPH4$LWPH4b_source[LWPH4b_group1c] <- "Gapfill"

LWPH4b_missing <- which(is.na(df_LWPH4$LWPH4b)) # 5213:5352
LWPH4b_group1d <- LWPH4b_missing[LWPH4b_missing %in% 5213:5352]
df_LWPH4[4329:4694, ] %>% 
  subset(LWPH4b_source == "Observed") %>% 
  ggplot(aes(x = LWPH4a, y = LWPH4b, color = LWPH4b_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4b_1d <- lm(LWPH4b ~ LWPH4a, data = subset(df_LWPH4[4329:4694, ], LWPH4b_source == "Observed"))
df_LWPH4$LWPH4b[LWPH4b_group1d] <- predict(lm_LWPH4b_1d, newdata = df_LWPH4[LWPH4b_group1d, ])
df_LWPH4$LWPH4b_source[LWPH4b_group1d] <- "Gapfill"

LWPH4b_missing <- which(is.na(df_LWPH4$LWPH4b)) # 5213:5221 - linearly interpolate
LWPH4b_group1e <- LWPH4b_missing[LWPH4b_missing %in% 5213:5221]
df_LWPH4[5203:5231, ] %>% 
  ggplot(aes(x = date_ghcn, y = LWPH4b, color = LWPH4b_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")
lm_LWPH4b_1e <- lm(LWPH4b ~ date_ghcn, data = df_LWPH4[5203:5231, ])
df_LWPH4$LWPH4b[LWPH4b_group1e] <- predict(lm_LWPH4b_1e, newdata = df_LWPH4[LWPH4b_group1e, ])
df_LWPH4$LWPH4b_source[LWPH4b_group1e] <- "Gapfill"

ggplot(df_LWPH4, aes(x = date_ghcn, y = LWPH4b, color = LWPH4b_source)) + 
  geom_point()

## gap-fill LWPH4a (shallow alluvial well)
df_LWPH4$LWPH4a_source <- "Observed"

LWPH4a_missing <- which(is.na(df_LWPH4$LWPH4a)) # 1662-1672, 2040-2058, 2402-2438, 2582-2596, 2745-2793, 3095-3122, 3481-3513, 3681-3711
df_LWPH4 %>% 
  subset(LWPH4b_source == "Observed") %>% 
  ggplot(aes(x = LWPH4b, y = LWPH4a, color = LWPH4a_source)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm")

lm_LWPH4a <- lm(LWPH4a ~ LWPH4b, data = subset(df_LWPH4, LWPH4b_source == "Observed"))
df_LWPH4$LWPH4a[LWPH4a_missing] <- predict(lm_LWPH4a, newdata = df_LWPH4[LWPH4a_missing, ])
df_LWPH4$LWPH4a_source[LWPH4a_missing] <- "Gapfill"

ggplot(df_LWPH4, aes(x = date_ghcn, y = LWPH4a, color = LWPH4a_source)) + 
  geom_point()

## check to make sure everything full
sum(is.na(df_LWPH4$LWPH4a))
sum(is.na(df_LWPH4$LWPH4b))
sum(is.na(df_LWPH4$LWPH4c))

sum(df_LWPH4$LWPH4a_source == "Observed")/sum(is.finite(df_LWPH4$LWPH4a))
sum(df_LWPH4$LWPH4b_source == "Observed")/sum(is.finite(df_LWPH4$LWPH4b))
sum(df_LWPH4$LWPH4c_source == "Observed")/sum(is.finite(df_LWPH4$LWPH4c))

# save
df_LWPH4 %>% 
  dplyr::select(date_ghcn, starts_with("LWPH4")) %>% 
  readr::write_csv(file.path("data", "Groundwater_WaterLevels-DailyGHCN-LWPH4-GapFill.csv"))

# plot
df_long <- tibble::tibble(date_ghcn = rep(df_LWPH4$date_ghcn, 3),
                          waterlevel_m = c(df_LWPH4$LWPH4a, df_LWPH4$LWPH4b, df_LWPH4$LWPH4c),
                          source = c(df_LWPH4$LWPH4a_source, df_LWPH4$LWPH4b_source, df_LWPH4$LWPH4c_source),
                          well = c(rep("LWPH4a", dim(df_LWPH4)[1]), rep("LWPH4b", dim(df_LWPH4)[1]), rep("LWPH4c", dim(df_LWPH4)[1])))

ggplot(subset(df_long, lubridate::year(date_ghcn) < 2021), 
       aes(x = date_ghcn, y = waterlevel_m, color = well)) +
  geom_line()
