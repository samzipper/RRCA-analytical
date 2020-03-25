## Figure_DomainMap+WellSample.R
# This script plots a multi-panel figure including:
#  (a) map of domain including BCs with selected wells highlighted
#  (b) histograms of characteristics of all wells and selected wells

source(file.path("src", "paths+packages.R"))

## load data
# well locations and characteristics
wells_all <- 
  file.path("results", "RRCA12p_03_WellSample.csv") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(sat_thickness_m = top_m - botm_m,
                WTD_SS_m = ground_m - head_SS_m,
                WTD_end_m = ground_m - head_end_m,
                transmissivity_m2s = hk_ms*sat_thickness_m,
                logTransmissivity_m2s = log10(transmissivity_m2s),
                logHk_ms = log10(hk_ms),
                Qw_m3d_abs = abs(Qw_m3d_mean),
                distToClosestSurfwat_km = distToClosestSurfwat_m/1000,
                distToClosestEVT_km = distToClosestEVT_m/1000,
                ss_100m = 100*ss_m,
                Qw_1000m3d_abs = Qw_m3d_abs/1000,
                col_km = col*1.60934,
                row_km = row*1.60934)

# model data frame
model_df <- 
  file.path("results", "RRCA12p_03_ModelData.csv") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(sat_thickness_m = top_m - botm_m,
                WTD_SS_m = ground_m - head_SS_m,
                WTD_end_m = ground_m - head_end_m,
                transmissivity_m2s = hk_ms*sat_thickness_m,
                logTransmissivity_m2s = log10(transmissivity_m2s),
                logHk_ms = log10(hk_ms),
                col_km = col*1.60934,
                row_km = row*1.60934)

# surface water
surfwat_df <- 
  file.path("results", "RRCA12p_03_Surfwat.csv") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(col_km = col*1.60934,
                row_km = row*1.60934)

## map of domain and boundary conditions
p.map <- 
  ggplot() +
  # background: plot transmissivity
  geom_raster(data = model_df, aes(x = col_km, y = row_km, fill = logTransmissivity_m2s)) +
  # add boundary conditions
  #geom_point(data = wells_all, aes(x = col, y = row, color = sample_lhs), shape = 21, alpha = 0.5) +
  geom_point(data = subset(wells_all, sample_lhs), aes(x = col_km, y = row_km), color = col.cat.red, shape = 21) +
  geom_tile(data = subset(surfwat_df, BC == "CHB"), aes(x = col_km, y = row_km), fill = col.cat.org) +
  geom_tile(data = subset(surfwat_df, BC == "DRN"), aes(x = col_km, y = row_km), fill = "deepskyblue") +
  geom_tile(data = subset(surfwat_df, BC == "STR"), aes(x = col_km, y = row_km), fill = "black") +
  # annotations
  annotate("text", 
           x = 355, y = 10, size=3, color = col.cat.org, 
           label = "Constant Head") +
  annotate("text", 
           x = 440, y = 48, size=3, color = "black",
           label = "Streams") +
  annotate("text", 
           x = 390, y = 200, size=3, color = "deepskyblue",
           label = "Drains") +
  annotate("text", 
           x = 65, y = 25, size=3, color = col.cat.red,
           label = "Pumping Well\nSample") +
  # aesthetics
  scale_x_continuous(name = NULL, expand = c(0,0)) + 
  scale_y_reverse(name = NULL, expand = c(0,0)) + 
  viridis::scale_fill_viridis(name = "log(Trans)\n[m\u00b2/s]", na.value = "white",
                              limits = c(-4, -1), breaks = seq(-4, -1)) +
  #scale_color_manual(name = "Wells", labels = c("TRUE" = "Tested Sample", "FALSE" = "All Wells"),
  #                   values = c("TRUE" = col.cat.red, "FALSE" = col.gray)) +
  coord_equal() +
  theme(axis.text.y=element_text(angle=90, hjust=0.5),
        legend.position = "bottom")

# set factor order
wells_props <- 
  wells_all %>% 
  subset(sample_lhs) %>% 
  dplyr::select(WellNum, sample_lhs, Qw_1000m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_100m) %>% 
  reshape2::melt(id = c("WellNum", "sample_lhs"))

wells_props$variable <- factor(wells_props$variable, 
                               levels = c("Qw_1000m3d_abs", "WTD_SS_m", 
                                          "distToClosestSurfwat_km", "distToClosestEVT_km", 
                                          "logTransmissivity_m2s", "ss_100m"))

p.wells <- 
  ggplot(wells_props) +
  geom_histogram(aes(x = value), fill = col.cat.red) +
  facet_wrap( ~ variable, scales = "free", ncol = 2,
              labeller = as_labeller(c("Qw_1000m3d_abs" = "Pumping [x1000 m\u00b3/d]",
                                       "logTransmissivity_m2s" = "log(Trans) [m\u00b2/s]",
                                       "ss_100m" = "Sp. Storage [x0.01 m\u207b\u00b9]",
                                       "distToClosestSurfwat_km" = "Distance to Water [km]",
                                       "distToClosestEVT_km" = "Distance to ET [km]",
                                       "WTD_SS_m" = "Water Table Depth [m]"))) +
  scale_x_continuous(name = "Value of Variable", expand = c(0,0)) +
  scale_y_continuous(name = "Number of Wells") +
  scale_fill_manual(name = "Wells", labels = c("FALSE" = "All Wells", "TRUE" = "Tested Sample"),
                    values = c("FALSE" = col.gray, "TRUE" = col.cat.red), guide = F) +
  theme(legend.position = "bottom")

## save plots
cowplot::plot_grid(p.map, p.wells, 
                   axis = "tb",
                   ncol = 2,
                   rel_widths = c(1, 1.1)) %>% 
  cowplot::save_plot(file.path("figures+tables", "Figure_DomainMap+WellSample.png"),
                     plot = .,
                     base_width = 190/25.4, base_height = 95/25.4)

## facet plot of well characteristics - sampled wells and all wells (for SI)
wells_props_all <- 
  wells_all %>% 
  dplyr::select(WellNum, sample_lhs, Qw_1000m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_100m) %>% 
  reshape2::melt(id = c("WellNum", "sample_lhs")) %>%
  transform(sample_factor = factor(sample_lhs, levels = c("TRUE", "FALSE")))

wells_props_all$variable <- factor(wells_props_all$variable, 
                                   levels = c("Qw_1000m3d_abs", "WTD_SS_m", 
                                              "distToClosestSurfwat_km", "distToClosestEVT_km", 
                                              "logTransmissivity_m2s", "ss_100m"))


ggplot(wells_props_all) +
  geom_histogram(aes(x = value, fill = sample_factor)) +
  facet_grid(sample_factor ~ variable, scales = "free",
             labeller = as_labeller(c("Qw_1000m3d_abs" = "Pumping Rate\n[x1000 m\u00b3/d]",
                                      "logTransmissivity_m2s" = "log(Trans) [m\u00b2/s]",
                                      "ss_100m" = "Sp. Storage\n[x0.01 m\u207b\u00b9]",
                                      "distToClosestSurfwat_km" = "Distance to\nWater [km]",
                                      "distToClosestEVT_km" = "Distance to\nET [km]",
                                      "WTD_SS_m" = "Water Table\nDepth [m]",
                                      "FALSE" = "All Wells",
                                      "TRUE" = "Tested Sample"))) +
  scale_x_continuous(name = "Value of Variable", expand = c(0,0)) +
  scale_y_continuous(name = "Number of Wells") +
  scale_fill_manual(name = "Wells", labels = c("FALSE" = "All Wells", "TRUE" = "Tested Sample"),
                    values = c("FALSE" = col.gray, "TRUE" = col.cat.red), guide = F) +
  theme(legend.position = "bottom") +
  ggsave(file.path("figures+tables", "Figure_DomainMap+WellSample_Hist-WellSample+AllWells.png"),
         width = 190, height = 95, units = "mm") +
  NULL
