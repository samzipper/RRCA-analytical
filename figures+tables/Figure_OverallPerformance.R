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
full_combo %>%
  subset(ADF == paste0(prox, "_", apportionment_eq)) %>%
  ggplot(aes(x = flux_m3d_modflow, y = flux_m3d_ADF, color = season)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  facet_wrap(~flux, ncol = 1, scales = "free",
             labeller = as_labeller(c("Depletion" = "(a) Depletion", "Capture" = "(b) Capture"))) +
  scale_x_continuous(name = "MODFLOW Estimate [m\u00b3/d]", 
                     limits = c(min(full_combo$flux_m3d_modflow), max(full_combo$flux_m3d_modflow)), 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Analytical Depletion Function Estimate [m\u00b3/d]", 
                     limits = c(min(full_combo$flux_m3d_ADF), max(full_combo$flux_m3d_ADF)),
                     expand = c(0,0)) +
  scale_color_manual(name = "Season", values = pal.season) +
  theme(legend.position = "bottom") +
  ggsave(file.path("figures+tables", "Figure_OverallPerformance.png"),
         width = 95, height = 190, units = "mm")

# # plot including analytical-only scatter
# p_overall_scatter <- 
#   full_combo %>% 
#   subset(flux == "Depletion" | (flux == "Capture" & ADF == paste0(prox, "_", apportionment_eq))) %>% 
#   ggplot(aes(x = flux_m3d_modflow, y = flux_m3d_ADF, color = season)) +
#   geom_point(alpha = 0.5) +
#   geom_abline(intercept = 0, slope = 1, color = "black") +
#   facet_grid(ADF ~ flux,
#              labeller = as_labeller(c(labs_ADF, 
#                                       c("Capture" = "Capture", "Depletion" = "Depletion")))) +
#   scale_x_continuous(name = "MODFLOW Estimate [m\u00b3/d]", expand = c(0,0)) +
#   scale_y_continuous(name = "Analytical Estimate [m\u00b3/d]", expand = c(0,0)) +
#   coord_equal() +
#   scale_color_manual(name = "Season", values = pal.season) +
#   ggsave(file.path("figures+tables", "Figure_OverallPerformance.png"),
#          width = 190, height = 145, units = "mm")

