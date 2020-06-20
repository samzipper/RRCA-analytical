## Figure_SensitivityToBCs.R

source(file.path("src", "paths+packages.R"))

## some parameters controlling which ADF results to use
analytical_model <- "glover"  # analytical model to use: "hunt" or "glover"
prox <- "Adjacent+Expanding"
apportionment_eq <- "WebSq"  # depletion apportionment equation: "Web" or "WebSq" or "InvDistSq"
storage <- "sy_bulk"   # "ss_bulk_m", "ss_well_m", "sy_bulk", or "sy_well"

## Load results from ADF
depletion_DRN <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Depletion_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_STR-DRN-CHB.csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(BCs = "With DRN") 

depletion_noDRN <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Depletion_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_STR-CHB.csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(BCs = "No DRN")

capture_noDRN <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Capture_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_STR-CHB.csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(BCs = "No DRN")

mostAffected_noDRN <-
  file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_MostAffected_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_STR-CHB.csv")) %>% 
  readr::read_csv() %>% 
  dplyr::mutate(BCs = "No DRN")

# surface water
surfwat_df <- 
  file.path("results", "RRCA12p_03_Surfwat.csv") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(col_km = col*1.60934,
                row_km = row*1.60934)
surfwat_BCs <- 
  surfwat_df %>% 
  dplyr::select(SegNum, BC) %>% 
  unique()

## join
depletion_both <-
  dplyr::full_join(depletion_DRN, depletion_noDRN, by = c("SP", "WellNum", "SegNum", "depletion_m3d_modflow"), 
                   suffix = c("_DRN", "_noDRN")) %>% 
  dplyr::left_join(RRCA12p_time, by = "SP") %>% 
  dplyr::left_join(surfwat_BCs, by = "SegNum") %>% 
  tidyr::replace_na(list("depletion_m3d_ADF_DRN" = 0, 
                         "depletion_m3d_ADF_noDRN" = 0))  # NA indicates depletion was not predicted at the SP and should therefore be 0

## scatterplot for each BC

axis_min <- min(c(depletion_both$depletion_m3d_modflow, depletion_both$depletion_m3d_ADF_DRN))
axis_max <- max(c(depletion_both$depletion_m3d_modflow, depletion_both$depletion_m3d_ADF_DRN))

depletion_both %>% 
  #dplyr::sample_n(50000) %>% 
  ggplot(aes(x = depletion_m3d_modflow, y = depletion_m3d_ADF_DRN, color = season)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  scale_x_continuous(name = "MODFLOW Streamflow Depletion [m\u00b3/d]", 
                     limits = c(axis_min, axis_max), 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Analytical Depletion Function\nStreamflow Depletion [m\u00b3/d]", 
                     limits = c(axis_min, axis_max), 
                     expand = c(0,0)) +
  scale_color_manual(name = "Season", values = pal.season) +
  theme(legend.position = "bottom") +
  facet_wrap(~factor(BC, levels = c("CHB", "STR", "DRN"),
                     labels = c("(a) CHB", "(b) STR", "(c) DRN")),
             scales = "free") +
  ggsave(filename = file.path("figures+tables", "Figure_SensitivityToBCs-ScatterByBC.png"),
         width = 190, height = 80, units = "mm") +
  ggsave(filename = file.path("figures+tables", "Figure4.pdf"),
         width = 190, height = 80, units = "mm", device = cairo_pdf)

## comparison with/without DRN
p_ADFvADF <- 
  depletion_both %>% 
  #dplyr::sample_n(1000) %>% 
  ggplot(aes(x = depletion_m3d_ADF_DRN, y = depletion_m3d_ADF_noDRN, color = season)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  scale_x_continuous(name = "Depletion, Analytical Depletion Function\nwith DRN [m\u00b3/d]", 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Depletion, Analytical Depletion Function\nwithout DRN [m\u00b3/d]", 
                     expand = c(0,0)) +
  scale_color_manual(name = "Season", values = pal.season)

## save plot
(p_ADFvADF + theme(legend.position = "bottom")) %>% 
  ggsave(filename = file.path("figures+tables", "Figure_SensitivityToBCs-DRNnoDRN.png"),
         plot = .,
         width = 95, height = 95, units = "mm")

## calculate fit statistics - same as those in table 1
# number of years (from end of simulation) to include in table
n_yrs <- 20
first_yr <- max(RRCA12p_time$year) - n_yrs + 1
first_SP <- min(RRCA12p_time$SP[RRCA12p_time$year >= first_yr])

# calculate most affected percent correct
match_prc_SP <- 
  mostAffected_noDRN %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(SP) %>% 
  dplyr::summarize(n_total = n(),
                   n_match = sum(SegNum_modflow == SegNum_ADF)) %>% 
  dplyr::mutate(prc_match_SP = n_match/n_total)

match_prc <- 100*mean(match_prc_SP$prc_match_SP)

# calculate MAD, most affected segment 
match_MAD_SP <- 
  mostAffected_noDRN %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(SP) %>% 
  dplyr::summarize(MAD_SP = hydroGOF::mae(depletion_m3d_ADF, depletion_m3d_modflow),
                   depletion_min_modflow = min(depletion_m3d_modflow),
                   depletion_max_modflow = max(depletion_m3d_modflow)) %>% 
  dplyr::mutate(MAD_SP_norm = MAD_SP/(depletion_max_modflow - depletion_min_modflow))

MAD_match_norm <- mean(match_MAD_SP$MAD_SP_norm)

# calculate MAD, total capture
capture_MAD_SP <-
  capture_noDRN %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(SP) %>% 
  dplyr::summarize(MAD_SP = hydroGOF::mae(capture_m3d_ADF, capture_m3d_modflow),
                   capture_min_modflow = min(capture_m3d_modflow),
                   capture_max_modflow = max(capture_m3d_modflow)) %>% 
  dplyr::mutate(MAD_SP_norm = MAD_SP/(capture_max_modflow - capture_min_modflow))

MAD_capture_norm <- mean(capture_MAD_SP$MAD_SP_norm)

# calculate bias, total capture
capture_bias_SP <-
  capture_noDRN %>% 
  subset(SP >= first_SP) %>% 
  dplyr::group_by(SP) %>% 
  dplyr::summarize(bias_capture_SP = hydroGOF::pbias(capture_m3d_ADF, capture_m3d_modflow))

bias_capture <- mean(capture_bias_SP$bias_capture_SP)

# calculate KGE, depletion
depletion_KGE_SP <-
  depletion_noDRN %>% 
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
