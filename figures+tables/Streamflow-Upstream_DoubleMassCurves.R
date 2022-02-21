## Streamflow-Upstream_DoubleMassCurves.R

source(file.path("code", "paths+packages.R"))

# define gagenames to plot, in order you want plotted
#  based on gagenames in Streamflow-Upstream_02_Clean.R
#  c("Granada (CO)", "Coolidge", "Syracuse", "Kendall", "Deerfield", 
#    "Garden City", "Dodge City", "Pawnee at Rozel", "Pawnee near Burdett", 
#    "Buckner at Hanston", "Buckner near Jetmore", "Buckner below Horsethief", 
#    "Buckner above Horsethief", "Larned")
gages_plot <- c("Pawnee near Burdett", "Pawnee at Rozel", "Larned")

gage_labels <- c("Pawnee near Burdett", "Pawnee at Rozel", "Arkansas near Larned")

# read in data
df_day <-
  read_csv(file.path("data", "Streamflow-Upstream_Daily_Clean.csv")) %>% 
  filter(gagename %in% gages_plot) %>% 
  mutate(gage_factor = factor(gagename, levels = gages_plot, labels = gage_labels)) %>% 
  subset(Date >= ymd("1998-10-01")) # start all when ark @ Larned starts

# for each gage, calculate cumsum of discharge
for (g in gages_plot){
  df_g <- subset(df_day, gagename == g)
  df_g$discharge_cms_cumsum <- cumsum(df_g$discharge_cms*86400)
  
  if (g == gages_plot[1]){
    df_all <- df_g
  } else {
    df_all <- bind_rows(df_all, df_g)
  }
}

# pivot to have cumsum as the columns
df_wide <- 
  df_all %>% 
  dplyr::select(Date, gage_factor, discharge_cms_cumsum) %>% 
  pivot_wider(names_from = "gage_factor", values_from = "discharge_cms_cumsum")

# pawnee river double mass curve
ggplot(df_wide, aes(x = `Pawnee near Burdett`/1e6, y = `Pawnee at Rozel`/1e6)) +
  geom_vline(xintercept = subset(df_wide, Date == ymd("2009-09-01"))$`Pawnee near Burdett`/1e6, color = col.gray) +
  geom_point() +
  scale_x_continuous(name = "Pawnee near Burdett cumulative discharge [10\u2076 m\u00b3]", expand = c(0,0)) +
  scale_y_continuous(name = "Pawnee at Rozel cumulative discharge [10\u2076 m\u00b3]", expand = c(0,0))
ggsave(file.path("figures+tables", "Streamflow-Upstream_DoubleMassCurvePawnee.png"),
       width = 105, height = 105, units = "mm")
