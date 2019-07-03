## Figure_DomainMap+WellSample.R
# This script plots a multi-panel figure including:
#  (a) map of domain including BCs with selected wells highlighted
#  (b) histograms of characteristics of all wells and selected wells

## load data
wells_all <- 
  file.path("results", "RRCA12p_03_WellSample.csv") %>% 
  readr::read_csv() %>% 
  transform(sat_thickness_m = top_m - botm_m,
            WTD_SS_m = ground_m - head_SS_m,
            WTD_end_m = ground_m - head_end_m) %>% 
  transform(transmissivity_m2s = hk_ms*sat_thickness_m) %>% 
  transform(logTransmissivity_m2s = log10(transmissivity_m2s),
            logHk_ms = log10(hk_ms))

## facet plot of well characteristics
wells_all %>% 
  dplyr::select(WellNum, sample_lhs, Qw_m3d_mean, logTransmissivity_m2s, ss, distToClosestStream_m, WTD_SS_m, logHk_ms, sat_thickness_m) %>% 
  reshape2::melt(id = c("WellNum", "sample_lhs")) %>%
  ggplot() +
  geom_histogram(aes(x = value, fill = sample_lhs)) +
  facet_wrap(~ variable, scales = "free", nrow = 2,
             labeller = as_labeller(c("Qw_m3d_mean" = "Pumping Rate, Qw [m3/d]",
                                      "logTransmissivity_m2s" = "log(T) [m2/s]",
                                      "ss" = "Storativity [-]",
                                      "distToClosestStream_m" = "Distance to Closest Stream [m]",
                                      "WTD_SS_m" = "Water Table Depth [m]",
                                      "logHk_ms" = "log(K) [m/s]",
                                      "sat_thickness_m" = "Saturated Thickness [m]"))) +
  scale_x_continuous(name = "Value of Variable", expand = c(0,0)) +
  scale_y_continuous(name = "Number of Wells") +
  scale_fill_discrete(name = "Wells", labels = c("FALSE" = "All Wells", "TRUE" = "Tested Sample")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))
