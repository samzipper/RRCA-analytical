## RRCA12p_09_CalculateFit.R
# This script calculates fit statistics for combined MODFLOW and ADF data.

source(file.path("src", "paths+packages.R"))

## some parameters controlling which ADF results to use
analytical_model <- "glover"  # analytical model to use: "hunt" or "glover"
str_BCs <- c("STR", "DRN", "CHB")  # surface water BCs to consider: c("STR", "DRN", "CHB")
apportionment_eq <- "WebSq"  # depletion apportionment equation: "Web" or "WebSq"
storage <- "sy_bulk"   # "ss_bulk_m", "ss_well_m", "sy_bulk", or "sy_well"
prox <- "Adjacent+Expanding"

## Load results from ADF
capture_ADF <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Capture_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(analytical = "ADF")

depletion_ADF <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Depletion_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(analytical = "ADF")

mostAffected_ADF <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_MostAffected_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(analytical = "ADF")

## Load results from analytical only
capture_anl <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-AnalyticalOnly_Capture_", analytical_model, "_", storage, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(analytical = "AnalyticalOnly")

depletion_anl <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-AnalyticalOnly_Depletion_", analytical_model, "_", storage, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(analytical = "AnalyticalOnly")

mostAffected_anl <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-AnalyticalOnly_MostAffected_", analytical_model, "_", storage, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(analytical = "AnalyticalOnly")

## combine
capture_all <-  dplyr::bind_rows(capture_ADF, capture_anl)
depletion_all <- dplyr::bind_rows(depletion_ADF, depletion_anl)
mostAffected_all <- dplyr::bind_rows(mostAffected_ADF, mostAffected_anl)

## fit statistics

# fit by well - overall
capture_fit_byWell <-
  capture_all %>% 
  dplyr::group_by(analytical, WellNum) %>% 
  dplyr::summarize(KGE = hydroGOF::KGE(capture_m3d_ADF, capture_m3d_modflow, method = "2012"),
                   RMSE = hydroGOF::rmse(capture_m3d_ADF, capture_m3d_modflow),
                   NRMSE = hydroGOF::nrmse(capture_m3d_ADF, capture_m3d_modflow, norm = "maxmin"),
                   MAE = hydroGOF::mae(capture_m3d_ADF, capture_m3d_modflow),
                   R2 = R2(capture_m3d_ADF, capture_m3d_modflow)) %>% 
  dplyr::mutate(season = "All", 
                metric = "Capture")

depletion_fit_byWell <-
  depletion_all %>% 
  dplyr::group_by(analytical, WellNum) %>% 
  dplyr::summarize(KGE = hydroGOF::KGE(depletion_m3d_ADF, depletion_m3d_modflow, method = "2012"),
                   RMSE = hydroGOF::rmse(depletion_m3d_ADF, depletion_m3d_modflow),
                   NRMSE = hydroGOF::nrmse(depletion_m3d_ADF, depletion_m3d_modflow, norm = "maxmin"),
                   MAE = hydroGOF::mae(depletion_m3d_ADF, depletion_m3d_modflow),
                   R2 = R2(depletion_m3d_ADF, depletion_m3d_modflow)) %>% 
  dplyr::mutate(season = "All",
                metric = "Depletion")

# fit by well - seasonal
capture_fit_byWell_season <-
  capture_all %>% 
  dplyr::left_join(RRCA12p_time, by = "SP") %>% 
  dplyr::group_by(analytical, WellNum, season) %>% 
  dplyr::summarize(KGE = hydroGOF::KGE(capture_m3d_ADF, capture_m3d_modflow, method = "2012"),
                   RMSE = hydroGOF::rmse(capture_m3d_ADF, capture_m3d_modflow),
                   NRMSE = hydroGOF::nrmse(capture_m3d_ADF, capture_m3d_modflow, norm = "maxmin"),
                   MAE = hydroGOF::mae(capture_m3d_ADF, capture_m3d_modflow),
                   R2 = R2(capture_m3d_ADF, capture_m3d_modflow)) %>% 
  dplyr::mutate(metric = "Capture")

depletion_fit_byWell_season <-
  depletion_all %>% 
  dplyr::left_join(RRCA12p_time, by = "SP") %>% 
  dplyr::group_by(analytical, WellNum, season) %>% 
  dplyr::summarize(KGE = hydroGOF::KGE(depletion_m3d_ADF, depletion_m3d_modflow, method = "2012"),
                   RMSE = hydroGOF::rmse(depletion_m3d_ADF, depletion_m3d_modflow),
                   NRMSE = hydroGOF::nrmse(depletion_m3d_ADF, depletion_m3d_modflow, norm = "maxmin"),
                   MAE = hydroGOF::mae(depletion_m3d_ADF, depletion_m3d_modflow),
                   R2 = R2(depletion_m3d_ADF, depletion_m3d_modflow)) %>% 
  dplyr::mutate(metric = "Depletion")

# combine all byWell fits
fit_byWell_all <- 
  dplyr::bind_rows(capture_fit_byWell, capture_fit_byWell_season, depletion_fit_byWell, depletion_fit_byWell_season)

## performance through time - only when there are lots of wells active
# figure out how many wells have depletion for each SP
wells_active <- 
  capture_all %>% 
  dplyr::group_by(analytical, SP) %>% 
  dplyr::summarize(n_active = n())
ggplot(wells_active, aes(x = SP, y = n_active, color = analytical)) + geom_line() # spike at SP=585?

# match
fit_match_bySP <- 
  mostAffected_all %>% 
  dplyr::mutate(match = SegNum_modflow==SegNum_ADF) %>% 
  dplyr::group_by(analytical, SP) %>% 
  dplyr::summarize(n_match = sum(match),
                   n_total = n()) %>% 
  dplyr::mutate(prc_match = n_match/n_total) %>% 
  dplyr::left_join(RRCA12p_time, by = "SP")

# fit by stress period
capture_fit_bySP <-
  capture_all %>% 
  dplyr::group_by(analytical, SP) %>% 
  dplyr::summarize(KGE = hydroGOF::KGE(capture_m3d_ADF, capture_m3d_modflow, method = "2012"),
                   RMSE = hydroGOF::rmse(capture_m3d_ADF, capture_m3d_modflow),
                   NRMSE = hydroGOF::nrmse(capture_m3d_ADF, capture_m3d_modflow, norm = "maxmin"),
                   MAE = hydroGOF::mae(capture_m3d_ADF, capture_m3d_modflow),
                   R2 = R2(capture_m3d_ADF, capture_m3d_modflow)) %>% 
  dplyr::mutate(metric = "Capture")

depletion_fit_bySP <-
  depletion_all %>% 
  dplyr::group_by(analytical, SP) %>% 
  dplyr::summarize(KGE = hydroGOF::KGE(depletion_m3d_ADF, depletion_m3d_modflow, method = "2012"),
                   RMSE = hydroGOF::rmse(depletion_m3d_ADF, depletion_m3d_modflow),
                   NRMSE = hydroGOF::nrmse(depletion_m3d_ADF, depletion_m3d_modflow, norm = "maxmin"),
                   MAE = hydroGOF::mae(depletion_m3d_ADF, depletion_m3d_modflow),
                   R2 = R2(depletion_m3d_ADF, depletion_m3d_modflow)) %>% 
  dplyr::mutate(metric = "Depletion")

ggplot(fit_match_bySP, aes(x = SP, y = prc_match, color = analytical)) + geom_line()
ggplot(depletion_fit_bySP, aes(x = SP, y = KGE, color = analytical)) + geom_line()
ggplot(capture_fit_bySP, aes(x = SP, y = MAE, color = analytical)) + geom_line()
