## calculate fit metrics
min_value_for_fit <- -Inf

# fit by well - overall
capture_fit_all <-
  capture_all %>% 
  subset(capture_m3d_ADF > min_value_for_fit | capture_m3d_modflow > min_value_for_fit) %>% 
  dplyr::group_by(WellNum) %>% 
  dplyr::summarize(KGE = hydroGOF::KGE(capture_m3d_ADF, capture_m3d_modflow, method = "2012"),
                   RMSE = hydroGOF::rmse(capture_m3d_ADF, capture_m3d_modflow),
                   NRMSE = hydroGOF::nrmse(capture_m3d_ADF, capture_m3d_modflow, norm = "maxmin"),
                   MAE = hydroGOF::mae(capture_m3d_ADF, capture_m3d_modflow),
                   R2 = R2(capture_m3d_ADF, capture_m3d_modflow),
                   capture_m3d_max = max(capture_m3d_modflow, na.rm = T)) %>% 
  dplyr::mutate(season = "All", 
                metric = "Capture")

depletion_fit_all <-
  depletion_all %>% 
  subset(depletion_m3d_ADF > min_value_for_fit | depletion_m3d_modflow > min_value_for_fit) %>% 
  dplyr::group_by(WellNum) %>% 
  dplyr::summarize(KGE = hydroGOF::KGE(depletion_m3d_ADF, depletion_m3d_modflow, method = "2012"),
                   RMSE = hydroGOF::rmse(depletion_m3d_ADF, depletion_m3d_modflow),
                   NRMSE = hydroGOF::nrmse(depletion_m3d_ADF, depletion_m3d_modflow, norm = "maxmin"),
                   MAE = hydroGOF::mae(depletion_m3d_ADF, depletion_m3d_modflow),
                   R2 = R2(depletion_m3d_ADF, depletion_m3d_modflow),
                   depletion_m3d_max = max(depletion_m3d_modflow, na.rm = T)) %>% 
  dplyr::mutate(season = "All",
                metric = "Depletion")

# fit by well - seasonal
capture_fit_season <-
  capture_all %>% 
  dplyr::left_join(RRCA12p_time, by = "SP") %>% 
  dplyr::group_by(WellNum, season) %>% 
  dplyr::summarize(KGE = hydroGOF::KGE(capture_m3d_ADF, capture_m3d_modflow, method = "2012"),
                   RMSE = hydroGOF::rmse(capture_m3d_ADF, capture_m3d_modflow),
                   NRMSE = hydroGOF::nrmse(capture_m3d_ADF, capture_m3d_modflow, norm = "maxmin"),
                   MAE = hydroGOF::mae(capture_m3d_ADF, capture_m3d_modflow),
                   R2 = R2(capture_m3d_ADF, capture_m3d_modflow),
                   capture_m3d_max = max(capture_m3d_modflow, na.rm = T)) %>% 
  dplyr::mutate(metric = "Capture")

depletion_fit_season <-
  depletion_all %>% 
  dplyr::left_join(RRCA12p_time, by = "SP") %>% 
  dplyr::group_by(WellNum, season) %>% 
  dplyr::summarize(KGE = hydroGOF::KGE(depletion_m3d_ADF, depletion_m3d_modflow, method = "2012"),
                   RMSE = hydroGOF::rmse(depletion_m3d_ADF, depletion_m3d_modflow),
                   NRMSE = hydroGOF::nrmse(depletion_m3d_ADF, depletion_m3d_modflow, norm = "maxmin"),
                   R2 = R2(depletion_m3d_ADF, depletion_m3d_modflow),
                   depletion_m3d_max = max(depletion_m3d_modflow, na.rm = T)) %>% 
  dplyr::mutate(metric = "Depletion")

# combine all fit metrics
fit_all <- dplyr::bind_rows(capture_fit_all, capture_fit_season, depletion_fit_all, depletion_fit_season)
fit_all_wel <- dplyr::left_join(fit_all, wells_df, by = "WellNum")

## calculate match percent
fit_match_byWell <- 
  mostAffected_all %>% 
  dplyr::mutate(match = SegNum_modflow==SegNum_ADF) %>% 
  dplyr::group_by(WellNum) %>% 
  dplyr::summarize(n_match = sum(match),
                   n_total = n()) %>% 
  dplyr::mutate(prc_match = n_match/n_total) %>% 
  dplyr::left_join(wells_df, by = "WellNum")

fit_match_bySP <- 
  mostAffected_all %>% 
  dplyr::mutate(match = SegNum_modflow==SegNum_ADF) %>% 
  dplyr::group_by(SP) %>% 
  dplyr::summarize(n_match = sum(match),
                   n_total = n()) %>% 
  dplyr::mutate(prc_match = n_match/n_total) %>% 
  dplyr::left_join(RRCA12p_time, by = "SP")

## plots for all wells
# capture scatter
p.scatter.capture <- 
  dplyr::left_join(capture_all, RRCA12p_time, by = "SP") %>%
  ggplot(aes(x = capture_m3d_ADF, y = capture_m3d_modflow, color = season)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  scale_x_continuous(name = "Monthly Mean Capture, ADF [m\u00b3/d]") +
  scale_y_continuous(name = "Monthly Mean Capture, MODFLOW [m\u00b3/d]") +
  scale_color_manual(name = "Season", values = pal.season) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_ScatterCapture_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 120, height = 95, units = "mm")
hydroGOF::KGE(capture_all$capture_m3d_ADF, capture_all$capture_m3d_modflow, method = "2012")
hydroGOF::rmse(capture_all$capture_m3d_ADF, capture_all$capture_m3d_modflow)
hydroGOF::nrmse(capture_all$capture_m3d_ADF, capture_all$capture_m3d_modflow, norm = "maxmin")
hydroGOF::mae(capture_all$capture_m3d_ADF, capture_all$capture_m3d_modflow)

# depletion scatter
p.scatter.depletion <- 
  dplyr::left_join(depletion_all, RRCA12p_time, by = "SP") %>% 
  ggplot(aes(x = depletion_m3d_ADF, y = depletion_m3d_modflow, color = season)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  scale_x_continuous(name = "Monthly Mean Depletion, ADF [m\u00b3/d]") +
  scale_y_continuous(name = "Monthly Mean Depletion, MODFLOW [m\u00b3/d]") +
  scale_color_manual(name = "Season", values = pal.season) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_ScatterDepletion_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 120, height = 95, units = "mm")
hydroGOF::KGE(depletion_all$depletion_m3d_ADF, depletion_all$depletion_m3d_modflow, method = "2012")
hydroGOF::rmse(depletion_all$depletion_m3d_ADF, depletion_all$depletion_m3d_modflow)
hydroGOF::nrmse(depletion_all$depletion_m3d_ADF, depletion_all$depletion_m3d_modflow, norm = "maxmin")

## plot fit as function of well characteristics
fit_match_byWell %>% 
  dplyr::mutate(
    logTransmissivity_m2s = log10(transmissivity_m2s),
    Qw_m3d_abs = abs(Qw_m3d_mean),
    distToClosestSurfwat_km = distToClosestSurfwat_m/1000,
    distToClosestEVT_km = distToClosestEVT_m/1000) %>% 
  dplyr::select(WellNum, prc_match, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "prc_match")) %>% 
  ggplot(aes(y = prc_match, x = value)) +
  geom_point() +
  facet_wrap(~variable, scales = "free_x", labeller = as_labeller(labs_wellProperties)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous(name = "% of Timesteps Most Affected Segment Predicted Correctly") +
  coord_cartesian(ylim=c(0,1)) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_Fit-MatchPrcByWell_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 190, height = 120, units = "mm")

ggplot(fit_match_bySP, aes(y = prc_match, x = SP, color = n_total)) +
  geom_line() +
  scale_x_continuous(name = "Stress Period") +
  scale_y_continuous(name = "% of Wells Most Affected Segment Predicted Correctly") +
  viridis::scale_color_viridis(name = "Wells Tested") +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_Fit-MatchPrcBySP_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 190, height = 95, units = "mm")




# only look at wells with capture reaching at least 1% of mean pumping rate
fit_all_wel %>% 
  subset(metric == "Capture" & capture_m3d_max/Qw_m3d_mean > 0.01 & season == "All") %>% 
  dplyr::mutate(
    logTransmissivity_m2s = log10(transmissivity_m2s),
    Qw_m3d_abs = abs(Qw_m3d_mean),
    distToClosestSurfwat_km = distToClosestSurfwat_m/1000,
    distToClosestEVT_km = distToClosestEVT_m/1000) %>% 
  dplyr::select(WellNum, KGE, metric, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "KGE", "metric")) %>% 
  ggplot(aes(y = KGE, x = value)) +
  geom_hline(yintercept = -0.41, color = col.gray) +
  geom_point() +
  facet_wrap(~variable, scales = "free_x", labeller = as_labeller(labs_wellProperties)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous(name = "KGE", limits = c(-2.5, 1)) +
  # coord_cartesian(ylim=c(-2.5,1)) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_Fit-OnlyBigWells-Capture_KGEByWell_", analytical_model, "_", storage, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 150, height = 150, units = "mm") + 
  NULL

fit_match_byWell %>% 
  subset(metric == "Capture" & capture_m3d_max/Qw_m3d_mean > 0.01 & season == "All") %>% 
  dplyr::mutate(
    logTransmissivity_m2s = log10(transmissivity_m2s),
    Qw_m3d_abs = abs(Qw_m3d_mean),
    distToClosestSurfwat_km = distToClosestSurfwat_m/1000,
    distToClosestEVT_km = distToClosestEVT_m/1000) %>% 
  dplyr::select(WellNum, prc_match, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "prc_match")) %>% 
  ggplot(aes(y = prc_match, x = value)) +
  geom_point() +
  facet_wrap(~variable, scales = "free_x", labeller = as_labeller(labs_wellProperties)) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous(name = "% of Timesteps Most Affected Segment Predicted Correctly") +
  coord_cartesian(ylim=c(0,1)) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_Fit-MatchPrcByWell_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 190, height = 120, units = "mm")





fit_all_wel %>% 
  subset(season == "All") %>% 
  dplyr::mutate(
    logTransmissivity_m2s = log10(transmissivity_m2s),
    Qw_m3d_abs = abs(Qw_m3d_mean),
    distToClosestSurfwat_km = distToClosestSurfwat_m/1000,
    distToClosestEVT_km = distToClosestEVT_m/1000) %>% 
  dplyr::select(WellNum, RMSE, metric, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "RMSE", "metric")) %>% 
  ggplot(aes(y = RMSE, x = value)) +
  geom_point() +
  facet_grid(metric~variable, scales = "free", labeller=as_labeller(c(labs_wellProperties,
                                                                      "Depletion"="Depletion", 
                                                                      "Capture"="Capture"))) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous(name = "RMSE") +
  # coord_cartesian(ylim=c(-2.5,1)) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_Fit-RMSEByWell_", analytical_model, "_", storage, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 360, height = 200, units = "mm")

fit_all_wel %>% 
  subset(season == "All") %>% 
  dplyr::mutate(
    logTransmissivity_m2s = log10(transmissivity_m2s),
    Qw_m3d_abs = abs(Qw_m3d_mean),
    distToClosestSurfwat_km = distToClosestSurfwat_m/1000,
    distToClosestEVT_km = distToClosestEVT_m/1000) %>% 
  dplyr::select(WellNum, NRMSE, metric, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "NRMSE", "metric")) %>% 
  ggplot(aes(y = NRMSE, x = value)) +
  geom_point() +
  facet_grid(metric~variable, scales = "free", labeller=as_labeller(c(labs_wellProperties,
                                                                      "Depletion"="Depletion", 
                                                                      "Capture"="Capture"))) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous(name = "NRMSE") +
  # coord_cartesian(ylim=c(-2.5,1)) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_Fit-NRMSEByWell_", analytical_model, "_", storage, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 360, height = 200, units = "mm")

fit_all_wel %>% 
  subset(season == "All") %>% 
  dplyr::mutate(
    logTransmissivity_m2s = log10(transmissivity_m2s),
    Qw_m3d_abs = abs(Qw_m3d_mean),
    distToClosestSurfwat_km = distToClosestSurfwat_m/1000,
    distToClosestEVT_km = distToClosestEVT_m/1000) %>% 
  dplyr::select(WellNum, R2, metric, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "R2", "metric")) %>% 
  ggplot(aes(y = R2, x = value)) +
  geom_point() +
  facet_grid(metric~variable, scales = "free", labeller=as_labeller(c(labs_wellProperties,
                                                                      "Depletion"="Depletion", 
                                                                      "Capture"="Capture"))) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous(name = "R2") +
  # coord_cartesian(ylim=c(-2.5,1)) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_Fit-R2ByWell_", analytical_model, "_", storage, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 360, height = 200, units = "mm")

fit_all_wel %>% 
  subset(season == "All") %>% 
  dplyr::mutate(
    logTransmissivity_m2s = log10(transmissivity_m2s),
    Qw_m3d_abs = abs(Qw_m3d_mean),
    distToClosestSurfwat_km = distToClosestSurfwat_m/1000,
    distToClosestEVT_km = distToClosestEVT_m/1000) %>% 
  dplyr::select(WellNum, KGE, metric, Qw_m3d_abs, logTransmissivity_m2s, distToClosestSurfwat_km, distToClosestEVT_km, WTD_SS_m, ss_m) %>% 
  reshape2::melt(id = c("WellNum", "KGE", "metric")) %>% 
  ggplot(aes(y = KGE, x = value)) +
  geom_hline(yintercept = -0.41, color = col.gray) +
  geom_point() +
  facet_grid(metric~variable, scales = "free", labeller=as_labeller(c(labs_wellProperties,
                                                                      "Depletion"="Depletion", 
                                                                      "Capture"="Capture"))) +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Value of Variable") +
  scale_y_continuous(name = "KGE", limits = c(-2.5, 1)) +
  # coord_cartesian(ylim=c(-2.5,1)) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_Fit-KGEByWell_", analytical_model, "_", storage, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 360, height = 200, units = "mm")

ggplot(fit_all_wel, aes(y = KGE, x = abs(Qw_m3d_mean), color = season)) +
  geom_hline(yintercept = -0.41, color = col.gray) +
  geom_point() +
  facet_grid(metric ~ season) +
  stat_smooth(method = "lm") +
  scale_y_continuous(limits = c(-2.5, 1)) +
  scale_color_manual(values = pal.season)

ggplot(fit_all_wel, aes(y = KGE, x = ss_m, color = season)) +
  geom_hline(yintercept = -0.41, color = col.gray) +
  geom_point() +
  facet_grid(metric ~ season) +
  stat_smooth(method = "lm") +
  scale_y_continuous(limits = c(-2.5, 1)) +
  scale_color_manual(values = pal.season)

ggplot(fit_all_wel, aes(y = KGE, x = distToClosestSurfwat_m, color = season)) +
  geom_hline(yintercept = -0.41, color = col.gray) +
  geom_point() +
  facet_grid(metric ~ season) +
  stat_smooth(method = "lm") +
  scale_y_continuous(limits = c(-2.5, 1)) +
  scale_color_manual(values = pal.season)

ggplot(fit_all_wel, aes(y = KGE, x = distToClosestEVT_m, color = season)) +
  geom_hline(yintercept = -0.41, color = col.gray) +
  geom_point() +
  facet_grid(metric ~ season) +
  stat_smooth(method = "lm") +
  scale_y_continuous(limits = c(-2.5, 1)) +
  scale_color_manual(values = pal.season)

ggplot(fit_all_wel, aes(y = KGE, x = log10(transmissivity_m2s), color = season)) +
  geom_hline(yintercept = -0.41, color = col.gray) +
  geom_point() +
  facet_grid(metric ~ season) +
  stat_smooth(method = "lm") +
  scale_y_continuous(limits = c(-2.5, 1)) +
  scale_color_manual(values = pal.season)

ggplot(fit_all_wel, aes(y = KGE, x = WTD_SS_m, color = season)) +
  geom_hline(yintercept = -0.41, color = col.gray) +
  geom_point() +
  facet_grid(metric ~ season) +
  stat_smooth(method = "lm") +
  scale_y_continuous(limits = c(-2.5, 1)) +
  scale_color_manual(values = pal.season)

### investigate depletion and capture for a specific well
w <- 9967
SP_start <- min(modflow_budget_df$SP[modflow_budget_df$WellNum == w & modflow_budget_df$variable == "WEL_NET"])  # when pumping starts

# check for outliers in change in mass balance error
df_bal_change <- tibble::tibble(SP = modflow_budget_df$SP[modflow_budget_df$WellNum == w & modflow_budget_df$variable == "BALANCE_NET"],
                                BalChange = modflow_budget_df$flux_change[modflow_budget_df$WellNum == w & modflow_budget_df$variable == "BALANCE_NET"])
SP_outliers <- df_bal_change$SP[tsoutliers::tso(ts(df_bal_change$BalChange), type = "AO", cval = 5)[["outliers"]]$ind]
SP_end <- min(c(Inf, SP_outliers))  # will use entire timeseries if no outliers found

ggplot() + 
  geom_line(data = df_bal_change, aes(x=SP, y=BalChange)) +
  geom_point(data = subset(df_bal_change, SP %in% SP_outliers), aes(x = SP, y = BalChange), color="red")

## compile depletion estimates
w_modflow <- 
  modflow_df %>% 
  subset(WellNum == w) %>% 
  subset(SP >= SP_start & SP < SP_end) %>% 
  tidyr::replace_na(list("depletion_m3d" = 0)) %>% 
  dplyr::select(SP, SegNum, depletion_m3d)

w_ADF <- 
  ADF_df %>% 
  subset(WellNum == w) %>% 
  dplyr::right_join(RRCA12p_time, by = c("time_days" = "day_end")) %>% 
  subset(SP >= SP_start & SP < SP_end) %>% 
  dplyr::select(SP, SegNum, depletion_m3d)

w_depletion <- 
  dplyr::full_join(w_ADF[ , c("SP", "SegNum", "depletion_m3d")], w_modflow[ , c("SP", "SegNum", "depletion_m3d")],
                   by = c("SP", "SegNum"), suffix = c("_ADF", "_modflow")) %>% 
  tidyr::replace_na(list("depletion_m3d_ADF" = 0, 
                         "depletion_m3d_modflow" = 0)) %>% 
  dplyr::mutate(WellNum = w)

unique(w_modflow$SegNum)
unique(w_ADF$SegNum)
unique(w_depletion$SegNum)

w_spd <- subset(wel_spd, WellNum==w) %>% 
  dplyr::right_join(RRCA12p_time, by="kstpkper") %>% 
  replace_na(list("Qw_m3d"=0)) %>% 
  subset(SP >= SP_start & SP < SP_end)

ggplot() + 
  geom_line(data=w_modflow, aes(x =SP, y = depletion_m3d, color = factor(SegNum))) +
  geom_line(data=w_spd, aes(x=SP, y=Qw_m3d), color = col.gray) +
  scale_color_manual(name="Segment", values=c("#ff7f00", "#984ea3", "#4daf4a", "#377eb8", "#e41a1c")) +
  scale_x_continuous(name = "Stress Period", expand=c(0,0)) +
  scale_y_continuous(name = "Depletion [m\u00b3/d]") +
  labs(title = paste0("Segment-level Streamflow Depletion, Well ", w)) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_SingleWellTimeseries_Well", w, "_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 190, height = 95, units = "mm")

### inspect negative depletion values
depletion_balance_all <-
  dplyr::left_join(depletion_all, modflow_budget_df[modflow_budget_df$variable=="BALANCE_NET", ],
                   by = c("SP", "WellNum"))

ggplot(depletion_balance_all, aes(x=flux_pumped, y=depletion_m3d_modflow)) + 
  geom_point(shape=21) +
  scale_x_continuous(name = "Mass Balance Error (pumped scenario)") +
  scale_y_continuous(name = "MODFLOW Depletion") +
  geom_hline(yintercept=0, color=col.gray) +
  geom_vline(xintercept=0, color=col.gray) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_Scatter-MassBalanceVsDepletion_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 95, height = 95, units = "mm")

sum(depletion_balance_all$depletion_m3d_modflow < 0)
sum(depletion_balance_all$depletion_m3d_modflow < 0 & 
      abs(depletion_balance_all$depletion_m3d_modflow) < abs(depletion_balance_all$flux_change))
sum(depletion_balance_all$depletion_m3d_modflow < -10)


# map of weird one
surfwat_df <- 
  file.path("results", "RRCA12p_03_Surfwat.csv") %>% 
  readr::read_csv()

# summarize segment size
seg_size <-
  surfwat_df %>% 
  dplyr::group_by(SegNum, BC) %>% 
  dplyr::summarize(n_cells = n())

w_surfwat <- subset(surfwat_df, SegNum %in% w_modflow$SegNum)

ggplot() +
  # geom_tile(data = surfwat_df, aes(x = col, y = row), fill = col.gray) +
  geom_tile(data = subset(surfwat_df, SegNum %in% w_modflow$SegNum), aes(x = col, y = row, fill = factor(SegNum)), alpha=0.5, color="black") +
  geom_point(data = subset(wells_df, WellNum==w), aes(x=col, y=row), color="red") +
  scale_fill_manual(name="Segment", values=c("#ff7f00", "#984ea3", "#4daf4a", "#377eb8", "#e41a1c")) +
  coord_cartesian(xlim=c(50,100), ylim=c(100,130)) +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_SingleWellMap_Well", w, "_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 120, height = 95, units = "mm")

subset(modflow_df, WellNum==w & SegNum==97) %>% 
  ggplot(aes(x=SP, y = leakage_m3d_baseline, color=depletion_m3d)) + 
  geom_point() +
  scale_color_gradient2()

depletion_balance_segsize_all <-
  dplyr::left_join(depletion_all, modflow_budget_df[modflow_budget_df$variable=="BALANCE_NET", ],
                   by = c("SP", "WellNum")) %>% 
  dplyr::left_join(seg_size, by = "SegNum")


ggplot(subset(depletion_balance_segsize_all, is.finite(n_cells)), aes(x=flux_pumped, y=depletion_m3d_modflow, 
                                                                      color=cut(n_cells, breaks=c(0,1,5,10,25,100), include.lowest=T))) + 
  geom_point(shape=21) +
  scale_x_continuous(name = "Mass Balance Error (pumped scenario)") +
  scale_y_continuous(name = "MODFLOW Depletion") +
  geom_hline(yintercept=0, color=col.gray) +
  geom_vline(xintercept=0, color=col.gray) +
  scale_color_discrete(name="Cells in Segment") +
  ggsave(file.path(onedrive_ws, "plots", paste0("RRCA12p_08_CompareMODFLOW-ADF_Scatter-MassBalanceVsDepletion+SegSize_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".png")),
         width = 120, height = 95, units = "mm")
