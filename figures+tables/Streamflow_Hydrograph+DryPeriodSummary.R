## Streamflow_DryPeriodSummary.R
# Requires output from the script Streamflow+Stage_03-IdentifyDryPeriods.R

source(file.path("code", "paths+packages.R"))

## load data
df_dry_periods <- readr::read_csv(file.path("data", "Streamflow_DryPeriods.csv"))
df_day <- readr::read_csv(file.path("data", "Streamflow+Stage_Daily_Clean.csv"))

## prep/plot hydrograph
# set minimum for plotting
min_q <- 0.001
df_day$discharge_forlog <- df_day$discharge_cms
df_day$discharge_forlog[df_day$discharge_forlog < min_q] <- min_q

p_hydrographs <-
  ggplot() +
  geom_rect(data = df_dry_periods, aes(xmin = first_noflow_date, xmax = last_noflow_date,
                                       ymin = min_q, ymax = Inf), 
            color = "transparent", fill = col.cat.yel, alpha = 0.4) +
  geom_hline(yintercept = min_q, color = col.gray) +
  scale_x_date(name = "Date", expand = c(0,0), date_labels = "%Y") +
  geom_line(data = df_day, aes(x = Date, y = discharge_forlog), color = col.cat.blu) +
  scale_y_log10(name = "Mean Daily Discharge [m\u00b3/s]", 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                expand = c(0, 0)) +
  #annotation_logticks() +
  NULL

## prep/plot dry periods

# summarize by length
df_dry_periods$noflow_length <- 
  cut(df_dry_periods$total_noflow_days,
      breaks = c(0, 7, 30, 90, 365, 1000),
      labels = c("< 7", "7-30", "30-90", "90-365", "> 365"))

df_length_summary <- 
  df_dry_periods %>% 
  dplyr::group_by(noflow_length) %>% 
  dplyr::summarize(n_events = n(),
                   total_days = sum(total_noflow_days),
                   prc_days = total_days/sum(df_dry_periods$total_noflow_days))

# summarize by season
df_season_summary <-
  df_dry_periods %>% 
  dplyr::mutate(Month = month(first_noflow_date),
                Season = quarter(first_noflow_date)) %>% 
  dplyr::group_by(Season) %>% 
  dplyr::summarize(n_events = n(),
                   mean_length = mean(total_noflow_days))

## plot
p_length <-
  ggplot(df_length_summary, aes(x = noflow_length, y = n_events, fill = prc_days)) +
  geom_col() +
  scale_x_discrete(name = "No-Flow Event Duration [days]") +
  scale_y_continuous(name = "Number of Events", 
                     breaks = seq(0, 8, 4),
                     expand = expansion(mult = c(0, 0.03))) +
  scale_fill_viridis_c(name = "Percent of all\nNo-Flow Days",
                       limits = c(0, max(df_length_summary$prc_days)),
                       breaks = c(0, 0.25, 0.50),
                       labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom",
        legend.title = element_text(vjust = 1, hjust = 0.5))

p_timing <-
  ggplot(df_season_summary, aes(x = factor(Season), y = n_events, fill = mean_length)) + 
  geom_col() +
  scale_x_discrete(name = "Season [start of event]",
                   labels = c("Winter", "Spring", "Summer", "Fall")) +
  scale_y_continuous(name = "Number of Events", 
                     breaks = seq(0, 12, 4),
                     expand = expansion(mult = c(0, 0.03))) +
  scale_fill_distiller(name = "Mean Event\nDuration [days]", 
                       type = "seq", palette = "YlOrRd", direction = 1,
                       limits = c(0, max(df_season_summary$mean_length))) +
  theme(legend.position = "bottom",
        legend.title = element_text(vjust = 1, hjust = 0.5))

p_combo <-
  (p_hydrographs / (p_length + p_timing)) +
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

ggsave(file.path("figures+tables", "Streamflow_Hydrograph+DryPeriodSummary.png"),
       p_combo, width = 190, height = 140, units = "mm")
