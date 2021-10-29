## StableStates_Bimodality.R

source(file.path("code", "paths+packages.R"))

## load data
df_day <- readr::read_csv(file.path("data", "Streamflow+Stage_Daily_Clean.csv"))

df_dry_periods <- readr::read_csv(file.path("data", "Streamflow_DryPeriods.csv"))

# set minimum for plotting
min_q <- 0.001
df_day$discharge_forlog <- df_day$discharge_cms
df_day$discharge_forlog[df_day$discharge_forlog < min_q] <- min_q

## summarize by year
df_yr <-
  df_day %>% 
  dplyr::group_by(WaterYear) %>% 
  dplyr::summarize(n_noflow = sum(discharge_cms == 0),
                   n_total = sum(is.finite(discharge_cms)),
                   prc_noflow = n_noflow/n_total) %>% 
  subset(n_total > 330)  # get rid of first year of data since it starts in August

## summarize by month
df_mo <-
  df_day %>% 
  mutate(Year = year(Date), 
         Month = month(Date)) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarize(date_mid = mean(Date),
                   n_noflow = sum(discharge_cms == 0),
                   n_total = sum(is.finite(discharge_cms)),
                   prc_noflow = n_noflow/n_total)


## plots - histogram
p_hist_mo <- 
  ggplot(df_mo, aes(x = prc_noflow)) +
  geom_histogram(breaks = seq(0, 1, 0.1), fill = col.cat.blu) +
  scale_y_continuous(name = "Number of Months", expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(name = "No-Flow Days [% of Month]", 
                     breaks = seq(0, 1, 0.2),
                     labels = scales::percent, 
                     expand = expansion(mult = 0)) +
  theme(plot.margin = margin(t = 1, r = 10, b = 1, l = 1, unit = "pt"))

p_hist_yr <- 
  ggplot(df_yr, aes(x = prc_noflow)) +
  geom_histogram(breaks = seq(0, 1, 0.1), fill = col.cat.blu) +
  scale_y_continuous(name = "Number of Years", expand = expansion(mult = c(0, 0.02)),
                     breaks = seq(0, 10, 2)) +
  scale_x_continuous(name = "No-Flow Days [% of Water Year]", 
                     breaks = seq(0, 1, 0.2),
                     minor_breaks = seq(0, 1, 0.1),
                     labels = scales::percent, 
                     expand = expansion(mult = 0)) +
  theme(plot.margin = margin(t = 1, r = 10, b = 1, l = 1, unit = "pt"))

p_hist <-
  (p_hist_yr + p_hist_mo) +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

ggsave(file.path("figures+tables", "StableStates_Bimodality-NoFlowDays.png"),
       p_hist, width = 190, height = 140, units = "mm")