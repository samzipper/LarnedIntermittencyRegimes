## Streamflow-Upstream_PlotArk+PawneeHydrographs.R
# This script plots a series of hydrographs for upstream gages in the Arkansas and Pawnee rivers.

source(file.path("code", "paths+packages.R"))

# define gagenames to plot, in order you want plotted
#  based on gagenames in Streamflow-Upstream_02_Clean.R
#  c("Granada (CO)", "Coolidge", "Syracuse", "Kendall", "Deerfield", 
#    "Garden City", "Dodge City", "Pawnee at Rozel", "Pawnee near Burdett", 
#    "Buckner at Hanston", "Buckner near Jetmore", "Buckner below Horsethief", 
#    "Buckner above Horsethief", "Larned")
gages_plot <- c("Dodge City", "Pawnee at Rozel", "Larned")

gage_labels <- c("Arkansas at Dodge City", "Pawnee at Rozel", "Arkansas near Larned")

# read in data
df_day <-
  read_csv(file.path("data", "Streamflow-Upstream_Daily_Clean.csv")) %>% 
  filter(gagename %in% gages_plot) %>% 
  mutate(gage_factor = factor(gagename, levels = gages_plot, labels = gage_labels))

df_dry_periods <- read_csv(file.path("data", "Streamflow_DryPeriods.csv")) # larned dry periods

df_dry_upstream <- read_csv(file.path("data", "Streamflow-Upstream_DryPeriods.csv")) %>%  #other site dry periods
  filter(gagename %in% gages_plot)

df_dry_all <- 
  df_dry_periods %>% 
  mutate(gagename = "Larned") %>% 
  bind_rows(df_dry_upstream) %>% 
  mutate(gage_factor = factor(gagename, levels = gages_plot, labels = gage_labels))

# set minimum for plotting
min_q <- 0.001
df_day$discharge_forlog <- df_day$discharge_cms
df_day$discharge_forlog[df_day$discharge_forlog < min_q] <- min_q

min_plot_date <- ymd(first_date)

# plot
ggplot() +
  geom_rect(data = df_dry_all, aes(xmin = first_noflow_date, xmax = last_noflow_date,
                                       ymin = min_q, ymax = Inf), 
            color = "transparent", fill = col.cat.yel, alpha = 0.4) +
  #geom_rect(data = df_dry_upstream, aes(xmin = first_noflow_date, xmax = last_noflow_date,
  #                                     ymin = min_q, ymax = min_q+min_q/2), 
  #          color = "transparent", fill = col.cat.red) +
  geom_line() +
  geom_hline(yintercept = min_q, color = col.gray) +
  facet_wrap(~gage_factor, scales = "free_x", ncol = 1, strip.position = "right") +
  scale_x_date(name = "Date", limits = c(ymd(min_plot_date), ymd(last_date)), expand = c(0,0), date_labels = "%Y") +
  geom_line(data = subset(df_day, Date >= min_plot_date), aes(x = Date, y = discharge_forlog), color = col.ark) +
  scale_y_log10(name = "Mean Daily Discharge [m\u00b3/s]", 
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                expand = c(0, 0))
ggsave(file.path("figures+tables", "Streamflow-Upstream_PlotArk+PawneeHydrographs.png"),
       width = 190, height = 140, units = "mm")

sum(subset(df_dry_all, gagename == "Pawnee at Rozel" & last_noflow_date >= ymd("1998-10-01") & last_noflow_date <= ymd("2018-05-28"))$total_noflow_days)
length(seq(ymd("1998-10-01"), ymd("2018-05-31"), by = "day"))
length(subset(df_dry_all, gagename == "Pawnee at Rozel" & last_noflow_date >= ymd("1998-10-01") & last_noflow_date <= ymd("2018-05-28"))$total_noflow_days)
mean(subset(df_dry_all, gagename == "Pawnee at Rozel" & last_noflow_date >= ymd("1998-10-01") & last_noflow_date <= ymd("2018-05-28"))$total_noflow_days)


sum(subset(df_dry_all, gagename == "Pawnee at Rozel" & last_noflow_date >= ymd("2018-06-01") & last_noflow_date <= ymd("2021-09-30"))$total_noflow_days)
length(seq(ymd("2018-06-01"), ymd("2021-09-30"), by = "day"))
length(subset(df_dry_all, gagename == "Pawnee at Rozel" & last_noflow_date >= ymd("2018-06-01") & last_noflow_date <= ymd("2021-09-30"))$total_noflow_days)
