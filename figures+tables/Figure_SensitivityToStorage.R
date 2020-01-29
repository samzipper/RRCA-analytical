## Figure_SensitivityToStorage.R

source(file.path("src", "paths+packages.R"))

## some parameters controlling which ADF results to use
analytical_model <- "glover"  # analytical model to use: "hunt" or "glover"
str_BCs <- c("STR", "DRN", "CHB")  # surface water BCs to consider: c("STR", "DRN", "CHB")

# best ADF
prox <- "Adjacent+Expanding"
apportionment_eq <- "WebSq"  # depletion apportionment equation: "Web" or "WebSq" or "InvDistSq"

## Load results from ADF
depletion_sy <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Depletion_", analytical_model, "_Sy_bulk_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(storage = "Specific Yield") 

depletion_ss <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Depletion_", analytical_model, "_Ss_bulk_m_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(storage = "Specific Storage") %>% 
  dplyr::left_join(RRCA12p_time, by = "SP")

capture_ss <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Capture_", analytical_model, "_Ss_bulk_m_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(storage = "Specific Storage") %>% 
  dplyr::left_join(RRCA12p_time, by = "SP")

mostAffected_ss <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_MostAffected_", analytical_model, "_Ss_bulk_m_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(storage = "Specific Storage") %>% 
  dplyr::left_join(RRCA12p_time, by = "SP")

## join
depletion_both <-
  dplyr::full_join(depletion_sy, depletion_ss, by = c("SP", "WellNum", "SegNum", "depletion_m3d_modflow"), 
                   suffix = c("_sy", "_ss")) %>% 
  replace(is.na(.), 0)  # NA indicates depletion was not predicted at the SP and should therefore be 0
  

  

## panel 1: comparison to modflow for ss
p_ADFvMODFLOW <- 
  depletion_ss %>% 
  #dplyr::sample_n(1000) %>% 
  ggplot(aes(x = depletion_m3d_modflow, y = depletion_m3d_ADF, color = season)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  scale_x_continuous(name = "Depletion, MODFLOW [m\u00b3/d]", 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Depletion, Analytical Depletion Function\nusing specific storage [m\u00b3/d]", 
                     expand = c(0,0)) +
  scale_color_manual(name = "Season", values = pal.season) +
  theme(legend.position = c(0.01, 0.99), 
        legend.justification = c(0, 1))

## panel 2: comparison between sy and ss
p_ADFvADF <- 
  depletion_both %>% 
  #dplyr::sample_n(1000) %>% 
  ggplot(aes(x = depletion_m3d_ADF_sy, y = depletion_m3d_ADF_ss, color = season)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  scale_x_continuous(name = "Depletion, Analytical Depletion Function\nusing specific yield [m\u00b3/d]", 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Depletion, Analytical Depletion Function\nusing specific storage [m\u00b3/d]", 
                     expand = c(0,0)) +
  scale_color_manual(name = "Season", values = pal.season, guide = F)

## save plots
# combined
#(p_ADFvMODFLOW + p_ADFvADF) %>% 
#  ggsave(filename = file.path("figures+tables", "Figure_SensitivityToStorage.png"),
#         plot = .,
#         width = 190, height = 95, units = "mm")

# just one plot
(p_ADFvMODFLOW + theme(legend.position = "bottom")) %>% 
  ggsave(filename = file.path("figures+tables", "Figure_SensitivityToStorage.png"),
         plot = .,
         width = 95, height = 95, units = "mm")

## calculate fit statistics - same as those in table 1
# number of years (from end of simulation) to include in table
n_yrs <- 20
first_yr <- max(RRCA12p_time$year) - n_yrs + 1
first_SP <- min(RRCA12p_time$SP[RRCA12p_time$year >= first_yr])

# calculate most affected percent correct
match_prc_SP <- 
  mostAffected_ss %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(SP) %>% 
  dplyr::summarize(n_total = n(),
                   n_match = sum(SegNum_modflow == SegNum_ADF)) %>% 
  dplyr::mutate(prc_match_SP = n_match/n_total)

match_prc <- 100*mean(match_prc_SP$prc_match_SP)

# calculate MAD, most affected segment 
match_MAD_SP <- 
  mostAffected_ss %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(SP) %>% 
  dplyr::summarize(MAD_SP = hydroGOF::mae(depletion_m3d_ADF, depletion_m3d_modflow),
                   depletion_min_modflow = min(depletion_m3d_modflow),
                   depletion_max_modflow = max(depletion_m3d_modflow)) %>% 
  dplyr::mutate(MAD_SP_norm = MAD_SP/(depletion_max_modflow - depletion_min_modflow))

MAD_match_norm <- mean(match_MAD_SP$MAD_SP_norm)

# calculate MAD, total capture
capture_MAD_SP <-
  capture_ss %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(SP) %>% 
  dplyr::summarize(MAD_SP = hydroGOF::mae(capture_m3d_ADF, capture_m3d_modflow),
                   capture_min_modflow = min(capture_m3d_modflow),
                   capture_max_modflow = max(capture_m3d_modflow)) %>% 
  dplyr::mutate(MAD_SP_norm = MAD_SP/(capture_max_modflow - capture_min_modflow))

MAD_capture_norm <- mean(capture_MAD_SP$MAD_SP_norm)

# calculate bias, total capture
capture_bias_SP <-
  capture_ss %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(SP) %>% 
  dplyr::summarize(bias_capture_SP = hydroGOF::pbias(capture_m3d_ADF, capture_m3d_modflow))

bias_capture <- mean(capture_bias_SP$bias_capture_SP)

# calculate KGE, depletion
depletion_KGE_SP <-
  depletion_ss %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(SP) %>% 
  dplyr::summarize(KGE_SP = hydroGOF::KGE(depletion_m3d_ADF, depletion_m3d_modflow, method = "2012"))

KGE_depletion = mean(depletion_KGE_SP$KGE_SP)

# final fit stats
match_prc
MAD_match_norm
KGE_depletion
bias_capture

# calculate bias for pumping and non-pumping season
depletion_ss %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarize(bias = hydroGOF::pbias(depletion_m3d_ADF, depletion_m3d_modflow))
