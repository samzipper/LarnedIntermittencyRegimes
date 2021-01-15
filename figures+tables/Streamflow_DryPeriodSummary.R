## Streamflow_DryPeriodSummary.R
# Requires output from the script Streamflow+Stage_03-IdentifyDryPeriods.R

source(file.path("code", "paths+packages.R"))

## load data
df_dry_periods <- readr::read_csv(file.path("data", "Streamflow_DryPeriods.csv"))

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
                   labels = c("Winter\n(JFM)", "Spring\n(AMJ)", "Summer\n(JAS)", "Fall\n(OND)")) +
  scale_y_continuous(name = "Number of Events", 
                     breaks = seq(0, 12, 4),
                     expand = expansion(mult = c(0, 0.03))) +
  scale_fill_distiller(name = "Mean Event\nDuration [days]", 
                       type = "seq", palette = "YlOrRd", direction = 1,
                       limits = c(0, max(df_season_summary$mean_length))) +
  theme(legend.position = "bottom",
        legend.title = element_text(vjust = 1, hjust = 0.5))

(p_length + p_timing) +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") +
  ggsave(file.path("figures+tables", "Streamflow_DryPeriodSummary.png"),
         width = 190, height = 100, units = "mm")
