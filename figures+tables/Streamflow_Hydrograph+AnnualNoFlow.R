## Streamflow_Hydrograph+AnnualNoFlow.R

source(file.path("code", "paths+packages.R"))

## load data
df_day <- readr::read_csv(file.path("data", "Streamflow+Stage_Daily_Clean.csv")) %>% 
  dplyr::mutate(Year = year(Date))

df_dry_periods <- readr::read_csv(file.path("data", "Streamflow_DryPeriods.csv"))

# set minimum for plotting
min_q <- 0.001
df_day$discharge_forlog <- df_day$discharge_cms
df_day$discharge_forlog[df_day$discharge_forlog < min_q] <- min_q

## summarize by year
df_yr <-
  df_day %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(n_noflow = sum(discharge_cms == 0),
                   n_total = sum(is.finite(discharge_cms)),
                   prc_noflow = n_noflow/n_total) %>% 
  subset(n_total > 330)  # get rid of first year of data since it starts in August

## plot
p_hydrographs <-
  ggplot() +
  geom_rect(data = df_dry_periods, aes(xmin = first_noflow_date, xmax = last_noflow_date,
                                       ymin = min_q, ymax = Inf), 
            color = "transparent", fill = col.cat.yel, alpha = 0.3) +
  geom_hline(yintercept = min_q, color = col.gray) +
  scale_x_date(name = "Date", expand = c(0,0)) +
  geom_line(data = df_day, aes(x = Date, y = discharge_forlog), color = col.cat.blu) +
  scale_y_log10(name = "Mean Daily Discharge [m\u00b3/s]", 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                expand = c(0, 0)) +
  #annotation_logticks() +
  NULL

p_dryprc <-
  ggplot(df_yr, aes(x = Year, y = prc_noflow)) +
  geom_point() + 
  geom_line() +
  scale_y_continuous(name = "No-Flow Days [% of Year]",
                     labels = scales::percent,
                     expand = expansion(mult = c(0.02,0.02))) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.04)))

(p_hydrographs + p_dryprc) +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") +
  ggsave(file.path("figures+tables", "Streamflow_Hydrograph+AnnualNoFlow.png"),
         width = 190, height = 140, units = "mm")
