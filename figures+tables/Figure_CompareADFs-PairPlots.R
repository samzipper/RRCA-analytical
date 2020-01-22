## Figure_CompareADFs-PairPlots.R
# Scatterplots of ADF output for all formaulations against the best-performing ADF (Adjacent, Web)

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
capture_combo <-  dplyr::bind_rows(capture_all, capture_anl)
depletion_combo <- dplyr::bind_rows(depletion_all, depletion_anl)
mostAffected_combo <- dplyr::bind_rows(mostAffected_all, mostAffected_anl)

# add MODFLOW to the end
capture_combo_modflow <- 
  unique(capture_ADF[,c("SP", "WellNum", "capture_m3d_modflow")]) %>% 
  mutate(analytical = "MODFLOW",
         proximity = "MODFLOW",
         apportionment = "MODFLOW") %>% 
  dplyr::rename(capture_m3d = capture_m3d_modflow)

capture_combo <-
  capture_combo %>% 
  dplyr::rename(capture_m3d = capture_m3d_ADF) %>% 
  dplyr::select(-capture_m3d_modflow) %>% 
  dplyr::bind_rows(capture_combo_modflow)

depletion_combo_modflow <- 
  unique(depletion_ADF[,c("SP", "SegNum", "WellNum", "depletion_m3d_modflow")]) %>% 
  mutate(analytical = "MODFLOW",
         proximity = "MODFLOW",
         apportionment = "MODFLOW") %>% 
  dplyr::rename(depletion_m3d = depletion_m3d_modflow)

depletion_combo <-
  depletion_combo %>% 
  dplyr::rename(depletion_m3d = depletion_m3d_ADF) %>% 
  dplyr::select(-depletion_m3d_modflow) %>% 
  dplyr::bind_rows(depletion_combo_modflow)

# make variables for combined 
capture_combo$ADF <- paste0(capture_combo$proximity, "_", capture_combo$apportionment)
depletion_combo$ADF <- paste0(depletion_combo$proximity, "_", depletion_combo$apportionment)

# cast for pairs and correlation comparison
capture_cast <-
  capture_combo %>% 
  dplyr::select(SP, WellNum, ADF, capture_m3d) %>% 
  reshape2::dcast(SP + WellNum ~ ADF,
                  value.var = "capture_m3d") %>% 
  replace(is.na(.), 0) %>%  # NA indicates depletion was not predicted at the SP and should therefore be 0
  dplyr::select(-SP, -WellNum)

depletion_cast <-
  depletion_combo %>% 
  dplyr::select(SP, WellNum, SegNum, ADF, depletion_m3d) %>% 
  reshape2::dcast(SP + WellNum + SegNum ~ ADF,
                  value.var = "depletion_m3d") %>% 
  replace(is.na(.), 0) %>%  # NA indicates depletion was not predicted at the SP and should therefore be 0
  dplyr::select(-SP, -WellNum, -SegNum)

# melt for facet plots
n_sample <- 10000

set.seed(1)
capture_melt <-
  capture_cast %>% 
  dplyr::sample_n(n_sample) %>% 
  reshape2::melt(id = c("Adjacent+Expanding_WebSq"), variable.name = "ADF", value.name = "capture_m3d")
  
set.seed(1)
depletion_melt <-
  depletion_cast %>% 
  dplyr::sample_n(n_sample) %>% 
  reshape2::melt(id = c("Adjacent+Expanding_WebSq"), variable.name = "ADF", value.name = "depletion_m3d")
  
## facet plots
p_pairs_capture <- 
  ggplot(capture_melt, aes(x = capture_m3d, y = `Adjacent+Expanding_WebSq`)) +
  geom_point(shape = 21) +
  geom_abline(intercept = 0, slope = 1, color = col.cat.red) +
  facet_wrap(~ADF, scales = "free", labeller = as_labeller(labs_ADF)) +
  scale_x_continuous(name = "Capture [m\u00b3/d] from other approach") +
  scale_y_continuous(name = "Capture [m\u00b3/d] from best analytical depletion function") +
  ggsave(file.path("figures+tables", "Figure_CompareADFs-PairPlots_CapturePairs.png"),
         width = 190, height = 190, units = "mm")

p_pairs_depletion <- 
  ggplot(depletion_melt, aes(x = depletion_m3d, y = `Adjacent+Expanding_WebSq`)) +
  geom_point(shape = 21) +
  geom_abline(intercept = 0, slope = 1, color = col.cat.red) +
  facet_wrap(~ADF, scales = "free", labeller = as_labeller(labs_ADF)) +
  scale_x_continuous(name = "Depletion [m\u00b3/d] from other approach") +
  scale_y_continuous(name = "Depletion [m\u00b3/d] from best analytical depletion function") +
  ggsave(file.path("figures+tables", "Figure_CompareADFs-PairPlots_DepletionPairs.png"),
         width = 190, height = 190, units = "mm")
