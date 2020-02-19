## Figure_OverallPerformance.R

source(file.path("src", "paths+packages.R"))

## some parameters controlling which ADF results to use
analytical_model <- "glover"  # analytical model to use: "hunt" or "glover"
str_BCs <- c("STR", "DRN", "CHB")  # surface water BCs to consider: c("STR", "DRN", "CHB")
storage <- "sy_bulk"   # "ss_bulk_m", "ss_well_m", "sy_bulk", or "sy_well"

# best ADF
prox <- "Adjacent+Expanding"
apportionment_eq <- "WebSq"  # depletion apportionment equation: "Web" or "WebSq" or "InvDistSq"

## Load results from ADF
capture_ADF <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Capture_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(analytical = "glover",
                proximity = prox,
                apportionment = apportionment_eq)

depletion_ADF <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Depletion_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(analytical = "glover",
                proximity = prox,
                apportionment = apportionment_eq) 


## Load results from analytical only
capture_anl <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-AnalyticalOnly_Capture_", analytical_model, "_", storage, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(analytical = "AnalyticalOnly",
                proximity = "AnalyticalOnly",
                apportionment = "AnalyticalOnly")

depletion_anl <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-AnalyticalOnly_Depletion_", analytical_model, "_", storage, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(analytical = "AnalyticalOnly",
                proximity = "AnalyticalOnly",
                apportionment = "AnalyticalOnly")

## combine
capture_combo <-  dplyr::bind_rows(capture_ADF, capture_anl)
depletion_combo <- dplyr::bind_rows(depletion_ADF, depletion_anl)

# make variables for combined 
capture_combo$ADF <- paste0(capture_combo$proximity, "_", capture_combo$apportionment)
depletion_combo$ADF <- paste0(depletion_combo$proximity, "_", depletion_combo$apportionment)

# join into single data frame
capture_combo <-
  capture_combo %>% 
  dplyr::rename(flux_m3d_ADF = capture_m3d_ADF,
                flux_m3d_modflow = capture_m3d_modflow) %>% 
  dplyr::mutate(flux = "Capture")

depletion_combo <-
  depletion_combo %>% 
  dplyr::rename(flux_m3d_ADF = depletion_m3d_ADF,
                flux_m3d_modflow = depletion_m3d_modflow) %>% 
  dplyr::mutate(flux = "Depletion")

full_combo <- 
  dplyr::bind_rows(capture_combo, depletion_combo) %>% 
  dplyr::mutate(flux = factor(flux, levels = c("Depletion", "Capture"))) %>% 
  dplyr::left_join(RRCA12p_time, by = "SP")

## plot
axis_min <- min(c(full_combo$flux_m3d_modflow, full_combo$flux_m3d_ADF))
axis_max <- max(c(full_combo$flux_m3d_modflow, full_combo$flux_m3d_ADF))
full_combo %>%
  subset(ADF == paste0(prox, "_", apportionment_eq)) %>%
  ggplot(aes(x = flux_m3d_modflow, y = flux_m3d_ADF, color = season)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  facet_wrap(~flux, ncol = 2, scales = "free",
             labeller = as_labeller(c("Depletion" = "(a) Depletion", "Capture" = "(b) Capture"))) +
  scale_x_continuous(name = "MODFLOW Estimate [m\u00b3/d]", 
                     limits = c(axis_min, axis_max), 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Analytical Depletion Function Estimate [m\u00b3/d]", 
                     limits = c(axis_min, axis_max), 
                     expand = c(0,0)) +
  scale_color_manual(name = "Season", values = pal.season) +
  theme(legend.position = "bottom") +
  ggsave(file.path("figures+tables", "Figure_OverallPerformance_NoLabels.png"),
         width = 190, height = 105, units = "mm", dpi = 500)

## explore weird points
# overestimate cluster of points: 
overestimates <- 
  full_combo %>% 
  subset(ADF == paste0(prox, "_", apportionment_eq)) %>%
  subset(flux == "Depletion") %>% 
  subset((flux_m3d_modflow < 1000) & 
           (flux_m3d_modflow > 100) & 
           (flux_m3d_ADF > 600) &
           (flux_m3d_ADF > flux_m3d_modflow))

# underestimated cluster of points
underestimates <- 
  full_combo %>% 
  subset(ADF == paste0(prox, "_", apportionment_eq)) %>%
  subset(flux == "Depletion") %>% 
  subset((flux_m3d_modflow > 1500) &
           (flux_m3d_ADF < flux_m3d_modflow))

# plot
full_combo %>%
  subset(ADF == paste0(prox, "_", apportionment_eq)) %>%
  subset(flux == "Depletion") %>% 
  ggplot(aes(x = flux_m3d_modflow, y = flux_m3d_ADF)) +
  geom_point(alpha = 0.5) +
  geom_point(data = overestimates, color = "red") +
  geom_point(data = underestimates, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  scale_x_continuous(name = "MODFLOW Estimate [m\u00b3/d]", 
                     limits = c(axis_min, axis_max), 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Analytical Depletion Function Estimate [m\u00b3/d]", 
                     limits = c(axis_min, axis_max), 
                     expand = c(0,0)) +
  scale_color_manual(name = "Season", values = pal.season) +
  theme(legend.position = "bottom")

# load well data
well_info <- 
  file.path("results", "RRCA12p_03_WellSample.csv") %>% 
  readr::read_csv() %>% 
  subset(sample_lhs) %>% 
  dplyr::mutate(
    transmissivity_m2s = hk_ms*(top_m - botm_m),
    logTransmissivity_m2s = log10(transmissivity_m2s),
    Qw_m3d_abs = abs(Qw_m3d_mean),
    WTD_SS_m = ground_m - head_SS_m,
    distToClosestSurfwat_km = distToClosestSurfwat_m/1000,
    distToClosestEVT_km = distToClosestEVT_m/1000) 

## dig into overestimates - virtually all overestimates are WellNum 11520 and 11521
table(overestimates$WellNum)
table(overestimates$SegNum)

overestimates_wells <- 
  well_info %>% 
  subset(WellNum %in% c(11520, 11521))

overestimates_wells$Qw_m3d_mean
quantile(well_info$Qw_m3d_mean, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$Qw_m3d_mean < overestimates_wells$Qw_m3d_mean)/dim(well_info)[1]
# about median pumping rate

overestimates_wells$ss_m
quantile(well_info$ss_m, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$ss_m < overestimates_wells$ss_m)/dim(well_info)[1]
# 80th percentile ss

overestimates_wells$distToClosestSurfwat_km
quantile(well_info$distToClosestSurfwat_km, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$distToClosestSurfwat_km < overestimates_wells$distToClosestSurfwat_km)/dim(well_info)[1]
# they are the two closest wells to streams

overestimates_wells$distToClosestEVT_km
quantile(well_info$distToClosestEVT_km, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$distToClosestEVT_km < overestimates_wells$distToClosestEVT_km)/dim(well_info)[1]
# ...and relatively far from EVT (85th percentile)

overestimates_wells$logTransmissivity_m2s
quantile(well_info$logTransmissivity_m2s, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$logTransmissivity_m2s < overestimates_wells$logTransmissivity_m2s)/dim(well_info)[1]
# ...and pretty low transmissivity (6th percentile)

overestimates_wells$WTD_SS_m
quantile(well_info$WTD_SS_m, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$WTD_SS_m < overestimates_wells$WTD_SS_m)/dim(well_info)[1]
# ...and pretty shallow WTD (3rd percentile)

## dig into underestimates - also primarily from two wells (118 and 121)
table(underestimates$WellNum)
table(underestimates$SegNum)

underestimates_wells <- 
  well_info %>% 
  subset(WellNum %in% c(118, 121))

underestimates_wells$Qw_m3d_mean
quantile(well_info$Qw_m3d_mean, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$Qw_m3d_mean < underestimates_wells$Qw_m3d_mean)/dim(well_info)[1]
# 68th percentile pumping rate

underestimates_wells$ss_m
quantile(well_info$ss_m, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$ss_m < underestimates_wells$ss_m)/dim(well_info)[1]
# 20th percentile ss

underestimates_wells$distToClosestSurfwat_km
quantile(well_info$distToClosestSurfwat_km, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$distToClosestSurfwat_km < underestimates_wells$distToClosestSurfwat_km)/dim(well_info)[1]
# 7th percentile distance to stream

underestimates_wells$distToClosestEVT_km
quantile(well_info$distToClosestEVT_km, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$distToClosestEVT_km < underestimates_wells$distToClosestEVT_km)/dim(well_info)[1]
# 55th percentile distance to ET

underestimates_wells$logTransmissivity_m2s
quantile(well_info$logTransmissivity_m2s, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$logTransmissivity_m2s < underestimates_wells$logTransmissivity_m2s)/dim(well_info)[1]
sum(well_info$logTransmissivity_m2s < underestimates_wells$logTransmissivity_m2s)/dim(well_info)[1]
# high transmissivity (95th percentile)

underestimates_wells$WTD_SS_m
quantile(well_info$WTD_SS_m, c(0.0, 0.25, 0.5, 0.75, 1.0))
sum(well_info$WTD_SS_m < underestimates_wells$WTD_SS_m)/dim(well_info)[1]
# 30th percentile WTD

ggplot(well_info, aes(x = col, y = row)) +
  geom_point() +
  geom_point(data = overestimates_wells, color = "red") +
  geom_point(data = underestimates_wells, color = "blue") +
  scale_y_reverse()
