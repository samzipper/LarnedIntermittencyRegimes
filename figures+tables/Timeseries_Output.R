## Timeseries_Output.R
# This plots output for time series model.

source(file.path("code", "paths+packages.R"))

## read data
df_in <- file.path("data", "Timeseries_InputData.csv") %>% 
  readr::read_csv(col_types = "Ddddddddddcdcdc")

df_out_daily <- 
  file.path("data", "Timeseries_FinalModel_DailyOutput.csv") %>% 
  readr::read_csv()

df_out_response <- 
  file.path("data", "Timeseries_FinalModel_ResponseFuncs.csv") %>% 
  readr::read_csv()

# regime shifts
df_regimes <- read_csv(file.path("data", "RegimeShifts_rstars.csv"))
df_regime_dates <- subset(df_regimes, ts_regime_shift != 0)
df_regimes_startend <- tibble(
  date_start = c(min(df_regimes$date_mid), df_regime_dates$date_mid),
  date_end = c(df_regime_dates$date_mid, max(df_regimes$date_mid)),
  regime_category = c("Wet", "Dry", "Wet", "Dry", "Wet")
) %>% 
  mutate(WaterYear_start = decimal_date(date_start + days(92)),
         WaterYear_end = decimal_date(date_end + days(92)))

## plot fit
df_out_daily$simulated <- df_out_daily$obs - df_out_daily$resid
colnames(df_out_daily)[1] <- "Date"

# adjust by minimum observed value to get back to masl
df_out_daily$level_masl <- df_out_daily$obs + min(df_in$stage_masl, na.rm = T)
df_out_daily$simulated_masl <- df_out_daily$simulated + min(df_in$stage_masl, na.rm = T)

# normalize recharge and well to 0
df_out_daily$well_norm <- df_out_daily$well - max(df_out_daily$well)
df_out_daily$recharge_norm <- df_out_daily$recharge - min(df_out_daily$recharge)

## fit stats
fit_kge <- hydroGOF::KGE(df_out_daily$simulated_masl, df_out_daily$level_masl)
fit_rmse <- hydroGOF::rmse(df_out_daily$simulated_masl, df_out_daily$level_masl)
hydroGOF::nrmse(df_out_daily$simulated_masl, df_out_daily$level_masl, norm = "maxmin")

## make plots
p_fit <-
  ggplot(df_out_daily, aes(x = Date)) +
  geom_vline(data = df_regimes_startend[1:4, ], aes(xintercept = date_end), linetype = "dashed") +
  geom_line(aes(y = level_masl), color = col.cat.org, size = 1) +
  geom_line(aes(y = simulated_masl), color = "black") +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(name = "Alluvial Aquifer Head [masl]") +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(face = "plain")) +
  labs(title = paste0("(a) Model fit (KGE = ", round(fit_kge, 2), ", RMSE = ", round(fit_rmse, 2), ")"))

## contribution of each stress to overall variability
p_stress <-
  df_out_daily %>% 
  rename(exchange = river) %>% 
  dplyr::select(Date, well_norm, recharge_norm, exchange) %>% 
  pivot_longer(-Date) %>% 
  ggplot(aes(x = Date, y = value)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_vline(data = df_regimes_startend[1:4, ], aes(xintercept = date_end), linetype = "dashed") +
  geom_line(aes(color = name)) +
  facet_wrap(~name, ncol = 1, scales = "free",
             labeller = as_labeller(c("recharge_norm" = "(c) Diffuse Recharge", 
                                      "exchange" = "(b) Stream-Aquifer Exchange",
                                      "well_norm" = "(d) Pumping"))) +
  scale_y_continuous(name = "Contribution to Head Variability [m]") +
  scale_x_date(expand = c(0,0)) +
  scale_color_manual(values = c("recharge_norm" = col.cat.red, "exchange" = col.cat.blu, "well_norm" = col.cat.grn),
                     guide = "none") +
  theme(axis.title.x = element_blank(),
        strip.text = element_text(hjust = 0))

p_combo <-
  (p_fit + p_stress) +
  plot_layout(ncol = 1, heights = c(1, 2.25))

ggsave(file.path("figures+tables", "Timeseries_OutputDaily_NoLabels.png"),
       p_combo, width = 95, height = 160, units = "mm")

ggsave(file.path("figures+tables", "Timeseries_OutputDaily_NoLabels.pdf"),
       p_combo, width = 95, height = 160, units = "mm", device = cairo_pdf)
