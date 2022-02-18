## Streamflow-Upstream_PlotArkHydrographs.R
# This script plots a series of hydrographs for gages along the Arkansas River from upstream to downstream.

source(file.path("code", "paths+packages.R"))

# define gagenames to plot, in order you want plotted
#  based on gagenames in Streamflow-Upstream_01_Download.R
gages_plot <- c("Coolidge", "Kendall",
                "Deerfield", "Garden City", "Dodge City", "Larned")

gage_labels <- c("Coolidge", "Kendall",
                 "Deerfield", "Garden City", "Dodge City","Larned")

# read in data
df_day <-
  read_csv(file.path("data", "Streamflow-Upstream_Daily_Clean.csv")) %>% 
  filter(gagename %in% gages_plot) %>% 
  mutate(gage_factor = factor(gagename, levels = gages_plot, labels = gage_labels))

df_dry_periods <- readr::read_csv(file.path("data", "Streamflow_DryPeriods.csv"))

# set minimum for plotting
min_q <- 0.001
df_day$discharge_forlog <- df_day$discharge_cms
df_day$discharge_forlog[df_day$discharge_forlog < min_q] <- min_q

min_plot_date <- ymd(first_date)

# plot
ggplot() +
  geom_rect(data = df_dry_periods, aes(xmin = first_noflow_date, xmax = last_noflow_date,
                                       ymin = min_q, ymax = Inf), 
            color = "transparent", fill = col.cat.yel, alpha = 0.4) +
  geom_line() +
  geom_hline(yintercept = min_q, color = col.gray) +
  facet_wrap(~gage_factor, scales = "free_x", ncol = 1, strip.position = "right") +
  scale_x_date(name = "Date", limits = c(ymd(min_plot_date), ymd(last_date)), expand = c(0,0), date_labels = "%Y") +
  geom_line(data = subset(df_day, Date >= min_plot_date), aes(x = Date, y = discharge_forlog), color = col.cat.blu) +
  scale_y_log10(name = "Mean Daily Discharge [m\u00b3/s]", 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                expand = c(0, 0))
ggsave(file.path("figures+tables", "Streamflow-Upstream_PlotArkHydrographs.png"),
       width = 120, height = 190, units = "mm")
