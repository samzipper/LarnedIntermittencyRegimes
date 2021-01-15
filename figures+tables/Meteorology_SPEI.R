## Meteorology_SPEI.R
# Needs output from script Meteorology_03-CalculateETo+SumMonthly+SPEI.R

source(file.path("code", "paths+packages.R"))

## load data
df_mo <- readr::read_csv(file.path("data", "Meteorology_Monthly+SPEI.csv"))

## plot
ts_plot <- c(12)
df_plot <-
  df_mo %>% 
  dplyr::select(date_mid, paste0("SPEI_", ts_plot, "mo")) %>% 
  tidyr::pivot_longer(cols = starts_with("SPEI"), names_to = "Timescale", values_to = "SPEI")

ggplot(df_plot, aes(x = date_mid, y = SPEI, color = SPEI)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_line() +
  facet_wrap(~Timescale, ncol = 1) +
  scale_x_date(name = "Date", expand = c(0,0)) +
  scale_color_gradient2(guide = F,
                        low = col.cat.red, mid = col.cat.grn, high = col.cat.blu) +
  ggsave(file.path("figures+tables", "Meteorology_SPEI.png"),
         width = 190, height = 95, units = "mm")
