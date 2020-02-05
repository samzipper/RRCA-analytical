## Figure_FitByWell.R
# Fit statistics for each well

source(file.path("src", "paths+packages.R"))

## some parameters controlling which ADF results to use
analytical_model <- "glover"  # analytical model to use: "hunt" or "glover"
str_BCs <- c("STR", "DRN", "CHB")  # surface water BCs to consider: c("STR", "DRN", "CHB")
apportionment_eq <- "WebSq"  # depletion apportionment equation: "Web" or "WebSq" or "InvDistSq"
storage <- "sy_bulk"   # "ss_bulk_m", "ss_well_m", "sy_bulk", or "sy_well"
prox <- "Adjacent+Expanding"

## load well info
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

## Load results from ADF
capture_ADF <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Capture_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv()

depletion_ADF <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Depletion_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv()

mostAffected_ADF <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_MostAffected_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv()

## fit statistics
# number of years (from end of simulation) to include in table
n_yrs <- 100
first_yr <- max(RRCA12p_time$year) - n_yrs + 1
first_SP <- min(RRCA12p_time$SP[RRCA12p_time$year >= first_yr])

# calculate most affected percent correct
match_prc <- 
  mostAffected_ADF %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(WellNum) %>% 
  dplyr::summarize(n_total = n(),
                   n_match = sum(SegNum_modflow == SegNum_ADF)) %>% 
  dplyr::mutate(prc_match = n_match/n_total)

# calculate MAD, most affected segment 
match_MAD <- 
  mostAffected_ADF %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(WellNum) %>% 
  dplyr::summarize(MAD_match = hydroGOF::mae(depletion_m3d_ADF, depletion_m3d_modflow),
                   depletion_min_modflow = min(depletion_m3d_modflow),
                   depletion_max_modflow = max(depletion_m3d_modflow)) %>% 
  dplyr::mutate(MAD_match_norm = MAD_match/(depletion_max_modflow - depletion_min_modflow))

# calculate MAD, total capture
capture_MAD <-
  capture_ADF %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(WellNum) %>% 
  dplyr::summarize(MAD_capture = hydroGOF::mae(capture_m3d_ADF, capture_m3d_modflow),
                   capture_min_modflow = min(capture_m3d_modflow),
                   capture_max_modflow = max(capture_m3d_modflow)) %>% 
  dplyr::mutate(MAD_capture_norm = MAD_capture/(capture_max_modflow - capture_min_modflow))

# calculate bias, total capture
capture_bias <-
  capture_ADF %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(WellNum) %>% 
  dplyr::summarize(bias_capture = hydroGOF::pbias(capture_m3d_ADF, capture_m3d_modflow))

# calculate KGE, depletion
depletion_KGE <-
  depletion_ADF %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(WellNum) %>% 
  dplyr::summarize(KGE = hydroGOF::KGE(depletion_m3d_ADF, depletion_m3d_modflow, method = "2012"))

## join all fit metrics together
fit_all <- 
  match_prc %>% 
  dplyr::left_join(match_MAD, by = c("WellNum")) %>% 
  dplyr::left_join(capture_MAD, by = c("WellNum")) %>% 
  dplyr::left_join(depletion_KGE, by = c("WellNum")) %>% 
  dplyr::left_join(capture_bias, by = c("WellNum")) %>% 
  dplyr::left_join(well_info, by = c("WellNum"))

## ecdf tests
fit_ecdf <- 
  fit_all %>% 
  dplyr::select(WellNum, prc_match, MAD_match_norm, KGE, bias_capture, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "prc_match", "MAD_match_norm", "KGE", "bias_capture"), value.name = "value", variable.name = "parameter") %>% 
  reshape2::melt(id = c("WellNum", "parameter", "value"), value.name = "fit", variable.name = "metric")

# thresholds to categorize into "acceptable" and "unacceptable"
prc_thres <- 0.5
MAD_norm_thres <- 0.25
KGE_thres <- -0.41
bias_thres <- 50

# make sure data are roughly evenly distributed about thresholds
sum(fit_all$prc_match > prc_thres)
sum(fit_all$prc_match < prc_thres)

sum(fit_all$MAD_match_norm > MAD_norm_thres)
sum(fit_all$MAD_match_norm < MAD_norm_thres)

sum(fit_all$KGE > KGE_thres, na.rm = T)
sum(fit_all$KGE < KGE_thres, na.rm = T)

sum(abs(fit_all$bias_capture) < bias_thres, na.rm = T)
sum(abs(fit_all$bias_capture) > bias_thres, na.rm = T)

# categorize
fit_ecdf$fit_group <- "Good"
fit_ecdf$fit_group[fit_ecdf$metric == "prc_match" & fit_ecdf$fit < prc_thres] <- "Poor"
fit_ecdf$fit_group[fit_ecdf$metric == "MAD_match_norm" & fit_ecdf$fit > MAD_norm_thres] <- "Poor"
fit_ecdf$fit_group[fit_ecdf$metric == "KGE" & fit_ecdf$fit < KGE_thres] <- "Poor"
fit_ecdf$fit_group[fit_ecdf$metric == "bias_capture" & abs(fit_ecdf$fit) > bias_thres] <- "Poor"

ggplot(fit_ecdf, aes(x = value, color = fit_group)) +
  stat_ecdf() +
  facet_grid(metric~parameter, scales = "free", 
             labeller = as_labeller(c(labs_wellProperties, 
                                      "prc_match" = "Percent Match", 
                                      "MAD_match_norm" = "Normalized MAD, Depletion",
                                      "KGE" = "KGE",
                                      "bias_capture" = "Bias, Capture Fraction"))) +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous(name = "ECDF", limits = c(0,1), expand = c(0,0)) +
  scale_color_discrete(name = "Fit") +
  theme(legend.position = "bottom")





## scatterplots
fit_all %>% 
  dplyr::select(WellNum, prc_match, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "prc_match")) %>% 
  ggplot(aes(y = prc_match, x = value)) +
  geom_point() +
  facet_wrap(~variable, scales = "free", labeller = as_labeller(labs_wellProperties)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous(name = "% of Timesteps Most Affected Segment Predicted Correctly",
                     labels = scales::percent) +
  coord_cartesian(ylim=c(0,1))

fit_all %>% 
  dplyr::select(WellNum, MAD_match_norm, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "MAD_match_norm")) %>% 
  ggplot(aes(y = MAD_match_norm, x = value)) +
  geom_point() +
  facet_wrap(~variable, scales = "free", labeller = as_labeller(labs_wellProperties)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous()

fit_all %>% 
  dplyr::select(WellNum, MAD_match, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "MAD_match")) %>% 
  ggplot(aes(y = MAD_match, x = value)) +
  geom_point() +
  facet_wrap(~variable, scales = "free", labeller = as_labeller(labs_wellProperties)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous()

fit_all %>% 
  dplyr::select(WellNum, KGE, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "KGE")) %>% 
  ggplot(aes(y = KGE, x = value)) +
  geom_point() +
  facet_wrap(~variable, scales = "free", labeller = as_labeller(labs_wellProperties)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous() +
  coord_cartesian(ylim = c(-5, 1))

fit_all %>% 
  dplyr::select(WellNum, bias_capture, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "bias_capture")) %>% 
  ggplot(aes(y = bias_capture, x = value)) +
  geom_point() +
  facet_wrap(~variable, scales = "free", labeller = as_labeller(labs_wellProperties)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous() +
  coord_cartesian(ylim = c(-100, 100))
