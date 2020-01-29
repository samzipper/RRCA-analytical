## RRCA12p_08_CompareMODFLOW-ADF.R
# Makes and saves tables combining MODFLOW and ADF output.
#  - MODFLOW output is from RRCA12p_05_MODFLOW-SummarizeLeakage.R
#  - ADF output is from RRCA12p_07_ADF-CalculateDepletion.R

source(file.path("src", "paths+packages.R"))

## some parameters controlling which ADF results to use
analytical_model <- "glover"  # analytical model to use: "hunt" or "glover"
str_BCs <- c("STR", "DRN", "CHB")  # surface water BCs to consider: c("STR", "DRN", "CHB")
storage <- "sy_bulk"   # "ss_bulk_m", "ss_well_m", "sy_bulk", or "sy_well"

proximity <- c("Adjacent", "Adjacent+Expanding")
apportionment_eqs <- c("Web", "WebSq", "InvDist", "InvDistSq")  # depletion apportionment equation: "Web" or "WebSq" or "InvDist" or "InvDistSq"

## load depletion estimates
modflow_df <- 
  file.path("results", "RRCA12p_05_MODFLOW-SummarizeLeakage.csv") %>% 
  readr::read_csv() %>% 
  dplyr::rename(depletion_m3d = leakage_change_m3d)

modflow_budget_df <- 
  file.path("results", "RRCA12p_05_MODFLOW-SummarizeBudget.csv") %>% 
  readr::read_csv()

ADF_all <- 
  paste0("RRCA12p_07_ADF-CalculationDepletion_", analytical_model, "_", storage, "_", 
         paste(proximity, collapse = "-"), "_", paste(apportionment_eqs, collapse = "-"), 
         "_", paste(str_BCs, collapse = "-"), ".csv") %>% 
  file.path(onedrive_ws, "results", .) %>% 
  readr::read_csv()

## load well stress period data
wells_df <- 
  file.path("results", "RRCA12p_03_WellSample.csv") %>% 
  readr::read_csv() %>% 
  subset(sample_lhs) %>% 
  dplyr::mutate(sat_thickness_m = top_m - botm_m,
                WTD_SS_m = ground_m - head_SS_m,
                WTD_end_m = ground_m - head_end_m,
                transmissivity_m2s = hk_ms*sat_thickness_m)

wel_spd <- 
  file.path(model_ws_simple, "RRCA12p_WEL_StressPeriodData.csv") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(Qw_m3d = abs(Qw*0.3048*0.3048*0.3048*86400))  # original units: ft3/s
wel_spd[,c("lay", "row", "col")] <- wel_spd[,c("lay", "row", "col")]+1  # python has 0-based indexing
wel_spd <- dplyr::left_join(wel_spd, wells_df, by = c("row", "col"))

for (apportionment_eq in apportionment_eqs){
  
  for (prox in proximity){
    ADF_df <- 
      ADF_all %>% 
      subset(apportionment == apportionment_eq & proximity == prox)
    
    ## loop through wells
    wells_all <- unique(wells_df$WellNum)
    for (i in 1:length(wells_all)){
      w <- wells_all[i]
      SP_start <- min(modflow_budget_df$SP[modflow_budget_df$WellNum == w & modflow_budget_df$variable == "WEL_NET"])  # when pumping starts
      
      # three-step outlier check
      #  step 1: anomalous mass balance error
      df_bal_change <- tibble::tibble(SP = modflow_budget_df$SP[modflow_budget_df$WellNum == w & modflow_budget_df$variable == "BALANCE_NET"],
                                      BalChange = modflow_budget_df$flux_change[modflow_budget_df$WellNum == w & modflow_budget_df$variable == "BALANCE_NET"])
      SP_outliers <- df_bal_change$SP[tsoutliers::tso(ts(df_bal_change$BalChange), type = "AO", cval = 5)[["outliers"]]$ind]
      
      #  step 2: negative depletion exceeding 1% of max pumping rate
      Qw_max <- wells_df$Qw_m3d_max[wells_df$WellNum == w]
      w_modflow_full <- 
        modflow_df %>% 
        subset(WellNum == w)
      SP_outliers <- c(SP_outliers, w_modflow_full$SP[which(w_modflow_full$depletion_m3d/Qw_max < -0.01)])
      
      #  step 3: negative capture exceeding 1% of max pumping rate
      capture_modflow_full <-
        w_modflow_full %>% 
        dplyr::group_by(SP) %>% 
        dplyr::summarize(capture_m3d = sum(depletion_m3d))
      SP_outliers <- c(SP_outliers, capture_modflow_full$SP[which(capture_modflow_full$capture_m3d/Qw_max < -0.01)])
      
      # identify where to stop analysis
      SP_end <- min(c(Inf, SP_outliers))  # will use entire timeseries if no outliers found
      
      ## compile depletion estimates
      w_modflow <- 
        w_modflow_full %>% 
        subset(SP >= SP_start & SP < SP_end) %>% 
        tidyr::replace_na(list("depletion_m3d" = 0)) %>% 
        dplyr::select(SP, SegNum, depletion_m3d)
      
      w_ADF <- 
        ADF_df %>% 
        subset(WellNum == w) %>% 
        dplyr::right_join(RRCA12p_time, by = c("time_days" = "day_end")) %>% 
        subset(SP >= SP_start & SP < SP_end & is.finite(SegNum)) %>% 
        dplyr::select(SP, SegNum, depletion_m3d)
      
      w_depletion <- 
        dplyr::full_join(w_ADF[ , c("SP", "SegNum", "depletion_m3d")], w_modflow[ , c("SP", "SegNum", "depletion_m3d")],
                         by = c("SP", "SegNum"), suffix = c("_ADF", "_modflow")) %>% 
        tidyr::replace_na(list("depletion_m3d_ADF" = 0, 
                               "depletion_m3d_modflow" = 0)) %>% 
        dplyr::mutate(WellNum = w)
      
      ## compile capture estimates
      w_modflow_capture <- 
        w_modflow %>% 
        dplyr::group_by(SP) %>% 
        dplyr::summarize(capture_m3d = sum(depletion_m3d)) %>% 
        dplyr::ungroup()
      
      w_ADF_capture <- 
        w_ADF %>% 
        dplyr::group_by(SP) %>% 
        dplyr::summarize(capture_m3d = sum(depletion_m3d)) %>% 
        dplyr::ungroup()
      
      w_capture <-
        dplyr::full_join(w_ADF_capture[ , c("SP", "capture_m3d")], w_modflow_capture[ , c("SP", "capture_m3d")],
                         by = "SP", suffix = c("_ADF", "_modflow")) %>% 
        tidyr::replace_na(list("capture_m3d_ADF" = 0, 
                               "capture_m3d_modflow" = 0)) %>% 
        dplyr::mutate(WellNum = w)
      
      ## compile most affected estimates
      w_modflow_mostAffected <- 
        w_modflow %>% 
        dplyr::group_by(SP) %>% 
        dplyr::filter(depletion_m3d == max(depletion_m3d))
      
      w_ADF_mostAffected <- 
        w_ADF %>% 
        dplyr::group_by(SP) %>% 
        dplyr::filter(depletion_m3d == max(depletion_m3d))
      
      w_mostAffected <- 
        w_modflow_mostAffected %>% 
        # add SegNUm of most affected ADF segment
        dplyr::left_join(w_ADF_mostAffected[,c("SP", "SegNum")],
                         by = "SP", suffix = c("_modflow", "_ADF")) %>% 
        # add ADF depletion in MODFLOW most affected segment
        dplyr::left_join(w_ADF, by = c("SP", "SegNum_modflow" = "SegNum"),
                         suffix = c("_modflow", "_ADF")) %>% 
        tidyr::replace_na(list("SegNum_ADF" = 0,
                               "depletion_m3d_ADF" = 0)) %>% 
        dplyr::mutate(WellNum = w)
      
      ## combine into single data frame
      if (i == 1){
        capture_all <- w_capture
        depletion_all <- w_depletion
        mostAffected_all <- w_mostAffected
        outliers_all <- tibble::tibble(WellNum = w, 
                                       SP = SP_outliers)
      } else {
        capture_all <- dplyr::bind_rows(capture_all, w_capture)
        depletion_all <- dplyr::bind_rows(depletion_all, w_depletion)
        mostAffected_all <- dplyr::bind_rows(mostAffected_all, w_mostAffected)
        outliers_all <- dplyr::bind_rows(outliers_all, 
                                         tibble::tibble(WellNum = w, 
                                                        SP = SP_outliers))
      }
      
      ## status update
      #print(paste0(i, " of ", length(wells_all), " complete, ", Sys.time()))
    }
    
    ## how many outliers?
    length(unique(outliers_all$WellNum))
    
    ## save output
    capture_all %>% 
      readr::write_csv(file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Capture_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")))
    
    depletion_all %>% 
      readr::write_csv(file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_Depletion_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")))
    
    mostAffected_all %>% 
      readr::write_csv(file.path("results", paste0("RRCA12p_08_CombineMODFLOW-ADF_MostAffected_", analytical_model, "_", storage, "_", prox, "_", apportionment_eq, "_", paste(str_BCs, collapse = "-"), ".csv")))
    
    print(paste0(apportionment_eq, " ", prox, " complete"))
    
  }
}
