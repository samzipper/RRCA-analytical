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
  # multiply ss by 100 so scales look better
  dplyr::mutate(ss_100m = 100*ss_m,
                Qw_1000m3d_abs = Qw_m3d_abs/1000) %>% 
  dplyr::select(WellNum, prc_match, MAD_match_norm, KGE, bias_capture, Qw_1000m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_100m) %>% 
  reshape2::melt(id = c("WellNum", "prc_match", "MAD_match_norm", "KGE", "bias_capture"), value.name = "value", variable.name = "parameter") %>% 
  reshape2::melt(id = c("WellNum", "parameter", "value"), value.name = "fit", variable.name = "metric")

# define thresholds to categorize into "acceptable" and "unacceptable"
quantile(fit_all$prc_match, 0.5)
prc_thres <- 0.5
sum(fit_all$prc_match > prc_thres)
sum(fit_all$prc_match < prc_thres)

quantile(fit_all$MAD_match_norm, 0.5)
MAD_norm_thres <- 0.25
sum(fit_all$MAD_match_norm > MAD_norm_thres)
sum(fit_all$MAD_match_norm < MAD_norm_thres)

quantile(fit_all$KGE, 0.5, na.rm = T)
KGE_thres <- -0.41
sum(fit_all$KGE > KGE_thres, na.rm = T)
sum(fit_all$KGE < KGE_thres, na.rm = T)

quantile(abs(fit_all$bias_capture), 0.5)
bias_thres <- 75
sum(abs(fit_all$bias_capture) < bias_thres, na.rm = T)
sum(abs(fit_all$bias_capture) > bias_thres, na.rm = T)

# categorize
fit_ecdf$fit_group <- "Good"
fit_ecdf$fit_group[fit_ecdf$metric == "prc_match" & fit_ecdf$fit < prc_thres] <- "Poor"
fit_ecdf$fit_group[fit_ecdf$metric == "MAD_match_norm" & fit_ecdf$fit > MAD_norm_thres] <- "Poor"
fit_ecdf$fit_group[fit_ecdf$metric == "KGE" & fit_ecdf$fit < KGE_thres] <- "Poor"
fit_ecdf$fit_group[fit_ecdf$metric == "bias_capture" & abs(fit_ecdf$fit) > bias_thres] <- "Poor"

# test significance
start_flag <- T
for (par in unique(fit_ecdf$parameter)){
  for (met in unique(fit_ecdf$metric)){
    
    # separate good and poor fits
    vals_good <- subset(fit_ecdf, parameter == par & metric == met & fit_group == "Good")$value
    vals_poor <- subset(fit_ecdf, parameter == par & metric == met & fit_group == "Poor")$value
    
    # ks test
    fit_ks <- ks.test(vals_good, vals_poor)
    
    # grab p-value
    p_ks <- fit_ks$p.value
    
    # make data frame
    ks_p <- tibble::tibble(parameter = par,
                           metric = met, 
                           p = p_ks,
                           sig = p_ks < 0.05)
    
    if (start_flag){
      ks_results <- ks_p
      start_flag <- F
    } else {
      ks_results <- dplyr::bind_rows(ks_results, ks_p)
    }
    
  }
}


# put parameters and metrics in factor order for plots
fit_ecdf$parameter <- factor(fit_ecdf$parameter, 
                             levels = c("Qw_1000m3d_abs", "WTD_SS_m", 
                                        "distToClosestSurfwat_km", "distToClosestEVT_km", 
                                        "logTransmissivity_m2s", "ss_100m"))
fit_ecdf$metric <- factor(fit_ecdf$metric, 
                          levels = c("prc_match", "MAD_match_norm", "KGE", "bias_capture"))
ks_results$parameter <- factor(ks_results$parameter, 
                               levels = c("Qw_1000m3d_abs", "WTD_SS_m", 
                                          "distToClosestSurfwat_km", "distToClosestEVT_km", 
                                          "logTransmissivity_m2s", "ss_100m"))
ks_results$metric <- factor(ks_results$metric, 
                            levels = c("prc_match", "MAD_match_norm", "KGE", "bias_capture"))

# select only parameters that have a significant difference
#pars_sig <- 
#  subset(ks_results, sig)$parameter %>% 
#  unique()

# plot
ggplot() +
  #  stat_ecdf(data = subset(fit_ecdf, parameter %in% pars_sig), 
  #            aes(x = value, color = fit_group)) +
  #  geom_rect(data = subset(ks_results, parameter %in% pars_sig), 
  #            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = sig)) +
  stat_ecdf(data = fit_ecdf,
            aes(x = value, color = fit_group)) +
  geom_rect(data = ks_results,
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = sig)) +
  facet_grid(metric~parameter, scales = "free", 
             labeller = as_labeller(c("Qw_1000m3d_abs" = "Pumping Rate\n[x1000 m\u00b3/d]",
                                      "logTransmissivity_m2s" = "log(Trans)\n[m\u00b2/s]",
                                      "ss_100m" = "Specific Storage\n[x0.01 m\u207b\u00b9]",
                                      "distToClosestSurfwat_km" = "Distance to\nWater [km]",
                                      "distToClosestEVT_km" = "Distance to\nET [km]",
                                      "WTD_SS_m" = "Water Table\nDepth [m]", 
                                      "prc_match" = "% most-affected\ncorrect", 
                                      "MAD_match_norm" = "MAD depletion,\nnormalized",
                                      "KGE" = "KGE,\ndepletion",
                                      "bias_capture" = "Bias,\ncapture fraction"))) +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous(name = "Cumulative Proportion", limits = c(0,1), expand = c(0,0), 
                     breaks = seq(0,1,0.2),
                     labels = c("0.0", "", "0.4", "", "0.8", "")) +
  scale_color_manual(name = "Fit", values = c("Good" = col.cat.blu, "Poor" = col.cat.red)) +
  scale_alpha_manual(name = "", values = c("FALSE" = 0.25, "TRUE" = 0),
                     labels = c("FALSE" = "Difference Not Significant", "TRUE" = "")) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(order = 1),
         alpha = guide_legend(order = 2)) +
  ggsave(file.path("figures+tables", "Figure_FitByWell_ECDFs_NoLabels.pdf"),
         width = 190, height = 145, units = "mm", device = cairo_pdf)






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
