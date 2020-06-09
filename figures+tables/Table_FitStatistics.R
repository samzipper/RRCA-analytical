## Table_FitStatistics.R
# Calculate fit statistics for different ADFs.

source(file.path("src", "paths+packages.R"))

## some parameters controlling which ADF results to use
analytical_model <- "glover"  # analytical model to use: "hunt" or "glover"
str_BCs <- c("STR", "DRN", "CHB")  # surface water BCs to consider: c("STR", "DRN", "CHB")
apportionment_eqs <- c("Web", "WebSq", "InvDist", "InvDistSq")  # depletion apportionment equation: "Web" or "WebSq" or "InvDistSq"
storage <- "sy_bulk"   # "ss_bulk_m", "ss_well_m", "sy_bulk", or "sy_well"
proximity <- c("Adjacent", "Adjacent+Expanding")

# gather all data
start_flag <- T
for (prox in proximity){
  for (apportionment_eq in apportionment_eqs){
    
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
    
    mostAffected_ADF <-
      file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_MostAffected_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
      readr::read_csv() %>% 
      dplyr::mutate(analytical = "glover",
                    proximity = prox,
                    apportionment = apportionment_eq)
    
    if (start_flag){
      capture_all <- capture_ADF
      depletion_all <- depletion_ADF
      mostAffected_all <- mostAffected_ADF
      start_flag <- F
    } else {
      capture_all <-  dplyr::bind_rows(capture_all, capture_ADF)
      depletion_all <- dplyr::bind_rows(depletion_all, depletion_ADF)
      mostAffected_all <- dplyr::bind_rows(mostAffected_all, mostAffected_ADF)
    }
  }
}

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

mostAffected_anl <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-AnalyticalOnly_MostAffected_", analytical_model, "_", storage, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(analytical = "AnalyticalOnly",
                proximity = "AnalyticalOnly",
                apportionment = "AnalyticalOnly")

## combine
capture_all <-  dplyr::bind_rows(capture_all, capture_anl)
depletion_all <- dplyr::bind_rows(depletion_all, depletion_anl)
mostAffected_all <- dplyr::bind_rows(mostAffected_all, mostAffected_anl)

# make variables for combined 
capture_all$ADF <- paste0(capture_all$proximity, "_", capture_all$apportionment)
depletion_all$ADF <- paste0(depletion_all$proximity, "_", depletion_all$apportionment)
mostAffected_all$ADF <- paste0(mostAffected_all$proximity, "_", mostAffected_all$apportionment)


## fit statistics
# number of years (from end of simulation) to include in table
n_yrs <- 20
first_yr <- max(RRCA12p_time$year) - n_yrs + 1
first_SP <- min(RRCA12p_time$SP[RRCA12p_time$year >= first_yr])

# calculate most affected percent correct
match_prc_SP <- 
  mostAffected_all %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(ADF, SP) %>% 
  dplyr::summarize(n_total = n(),
                   n_match = sum(SegNum_modflow == SegNum_ADF)) %>% 
  dplyr::mutate(prc_match_SP = n_match/n_total)

match_prc <- 
  match_prc_SP %>% 
  dplyr::group_by(ADF) %>% 
  dplyr::summarize(prc_match = 100*mean(prc_match_SP))

anova(lm(prc_match_SP ~ ADF, data = match_prc_SP)) # test anova - need significant F for tukey to be valid
match_prc_diffs <- generate_label_df(TukeyHSD(aov(lm(prc_match_SP ~ ADF, data = match_prc_SP))), "ADF")

# calculate MAD, most affected segment 
match_MAD_SP <- 
  mostAffected_all %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(ADF, SP) %>% 
  dplyr::summarize(MAD_SP = hydroGOF::mae(depletion_m3d_ADF, depletion_m3d_modflow),
                   depletion_min_modflow = min(depletion_m3d_modflow),
                   depletion_max_modflow = max(depletion_m3d_modflow)) %>% 
  dplyr::mutate(MAD_SP_norm = MAD_SP/(depletion_max_modflow - depletion_min_modflow))

match_MAD <- 
  match_MAD_SP %>% 
  dplyr::group_by(ADF) %>% 
  dplyr::summarize(MAD_match = mean(MAD_SP),
                   MAD_match_norm = mean(MAD_SP_norm))
  
anova(lm(MAD_SP_norm ~ ADF, data = match_MAD_SP))
match_MAD_diffs <- generate_label_df(TukeyHSD(aov(lm(MAD_SP_norm ~ ADF, data = match_MAD_SP))), "ADF")

# calculate MAD, total capture
capture_MAD_SP <-
  capture_all %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(ADF, SP) %>% 
  dplyr::summarize(MAD_SP = hydroGOF::mae(capture_m3d_ADF, capture_m3d_modflow),
                   capture_min_modflow = min(capture_m3d_modflow),
                   capture_max_modflow = max(capture_m3d_modflow)) %>% 
  dplyr::mutate(MAD_SP_norm = MAD_SP/(capture_max_modflow - capture_min_modflow))

capture_MAD <- 
  capture_MAD_SP %>% 
  dplyr::group_by(ADF) %>% 
  dplyr::summarize(MAD_capture = mean(MAD_SP),
                   MAD_capture_norm = mean(MAD_SP_norm))

anova(lm(MAD_SP_norm ~ ADF, data = capture_MAD_SP))
capture_diffs <- generate_label_df(TukeyHSD(aov(lm(MAD_SP_norm ~ ADF, data = capture_MAD_SP))), "ADF")

# calculate bias, total capture
capture_bias_SP <-
  capture_all %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(ADF, SP) %>% 
  dplyr::summarize(bias_capture_SP = hydroGOF::pbias(capture_m3d_ADF, capture_m3d_modflow))

capture_bias <- 
  capture_bias_SP %>% 
  dplyr::group_by(ADF) %>% 
  dplyr::summarize(bias_capture = mean(bias_capture_SP))

anova(lm(bias_capture_SP ~ ADF, data = capture_bias_SP))
capture_bias_diffs <- generate_label_df(TukeyHSD(aov(lm(bias_capture_SP ~ ADF, data = capture_bias_SP))), "ADF")

# calculate KGE, depletion
depletion_KGE_SP <-
  depletion_all %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(ADF, SP) %>% 
  dplyr::summarize(KGE_SP = hydroGOF::KGE(depletion_m3d_ADF, depletion_m3d_modflow, method = "2012"))
  
depletion_KGE <-
  depletion_KGE_SP %>% 
  dplyr::group_by(ADF) %>% 
  dplyr::summarize(KGE_depletion = mean(KGE_SP))

anova(lm(KGE_SP ~ ADF, data = depletion_KGE_SP))
depletion_diffs <- generate_label_df(TukeyHSD(aov(lm(KGE_SP ~ ADF, data = depletion_KGE_SP))), "ADF")

## join all fit metrics together
fit_all <- 
  match_prc %>% 
  dplyr::left_join(match_MAD, by = c("ADF")) %>% 
  dplyr::left_join(capture_MAD, by = c("ADF")) %>% 
  dplyr::left_join(capture_bias, by = c("ADF")) %>% 
  dplyr::left_join(depletion_KGE, by = c("ADF")) %>% 
  dplyr::left_join(match_prc_diffs, by = c("ADF" = "treatment")) %>% 
  dplyr::rename(prc_match_letters = Letters) %>% 
  dplyr::left_join(match_MAD_diffs, by = c("ADF" = "treatment")) %>% 
  dplyr::rename(MAD_match_norm_letters = Letters) %>% 
  dplyr::left_join(capture_diffs, by = c("ADF" = "treatment")) %>% 
  dplyr::rename(MAD_capture_norm_letters = Letters) %>%
  dplyr::left_join(capture_bias_diffs, by = c("ADF" = "treatment")) %>% 
  dplyr::rename(bias_capture_letters = Letters) %>% 
  dplyr::left_join(depletion_diffs, by = c("ADF" = "treatment")) %>% 
  dplyr::rename(KGE_depletion_letters = Letters)

# save output
fit_all %>% 
  dplyr::select(ADF, prc_match, MAD_match_norm, KGE_depletion, bias_capture, 
                prc_match_letters, MAD_match_norm_letters, KGE_depletion_letters, bias_capture_letters) %>% 
  dfDigits(digits = 3) %>% 
  readr::write_csv(file.path("figures+tables", "Table_FitStatistics.csv"))