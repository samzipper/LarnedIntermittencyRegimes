## Meteorology_SPEI.R
# Needs output from script Meteorology_03-CalculateETo+SumMonthly+SPEI.R

source(file.path("code", "paths+packages.R"))

## load data
df_mo <- readr::read_csv(file.path("data", "Meteorology_Monthly+SPEI.csv"))

## plot
ts_plot <- c(3, 12)
df_plot <-
  df_mo %>% 
  dplyr::select(date_mid, paste0("SPEI_", ts_plot, "mo")) %>% 
  tidyr::pivot_longer(cols = starts_with("SPEI"), names_to = "Timescale", values_to = "SPEI") %>% 
  replace_na(list("SPEI" = 0))

# set factor order and labels - do this manually based on what is in ts_plot
df_plot$Timescale_factor <- factor(df_plot$Timescale, 
                                   levels = c("SPEI_3mo", "SPEI_12mo"), 
                                   labels = c("(a) 3 month SPEI", "(b) 12 month SPEI"))

p_SPEI <-
  ggplot(df_plot, aes(x = date_mid, y = SPEI, fill = SPEI > 0, color = SPEI > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, color = col.gray) +
  facet_wrap(~Timescale_factor, ncol = 1, scales = "free_x") +
  scale_x_date(name = "Date", expand = c(0,0), date_labels = "%Y") +
  scale_fill_manual(values = c("FALSE" = col.cat.red, "TRUE" = col.cat.blu), guide = "none") +
  scale_color_manual(values = c("FALSE" = col.cat.red, "TRUE" = col.cat.blu), guide = "none") +
  theme(strip.text = element_text(hjust = 0))

ggsave(file.path("figures+tables", "Meteorology_SPEI.png"),
       p_SPEI, width = 190, height = 110, units = "mm", dpi = 500)
