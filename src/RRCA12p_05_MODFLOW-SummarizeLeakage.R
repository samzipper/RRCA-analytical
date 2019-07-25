## RRCA12p_05_MODFLOW-SummarizeLeakage.R
# This script is intended to postprocess MODFLOW output and
# summarize fluxes for each surface water boundary condition.

source(file.path("src", "paths+packages.R"))

## get surface water boundary condition info including SegNum
surfwat_df <- 
  file.path("results", "RRCA12p_03_Surfwat.csv") %>% 
  readr::read_csv(col_types = cols())

## postprocess baseline
# STR - already summarized by segment in python
out_str_baseline_bySeg <- 
  file.path(model_ws_simple, "RRCA12p_STR_Leakage.csv") %>% 
  readr::read_csv(col_types = cols()) %>% 
  dplyr::mutate(leakage_m3d_baseline = leakage*0.3048*0.3048*0.3048*86400) %>% 
  dplyr::mutate(BC = "STR") %>% 
  dplyr::select(SegNum, kstpkper, leakage_m3d_baseline, BC) %>% 
  dplyr::ungroup()

# DRN
out_drn_baseline <- 
  file.path(model_ws_simple, "RRCA12p_DRN_Leakage.csv") %>% 
  readr::read_csv(col_types = cols())
out_drn_baseline[ ,c("row", "col")] <- out_drn_baseline[ ,c("row", "col")] + 1
out_drn_baseline_bySeg <- 
  out_drn_baseline %>% 
  dplyr::left_join(subset(surfwat_df, BC == "DRN"), by = c("row", "col")) %>% 
  dplyr::group_by(SegNum, kstpkper) %>% 
  dplyr::summarize(leakage_m3d_baseline = sum(leakage)*0.3048*0.3048*0.3048*86400) %>% 
  dplyr::mutate(BC = "DRN") %>% 
  dplyr::ungroup()

# CHB
out_chb_baseline <- 
  file.path(model_ws_simple, "RRCA12p_CHB_Leakage.csv") %>% 
  readr::read_csv(col_types = cols())
out_chb_baseline[ ,c("row", "col")] <- out_chb_baseline[ ,c("row", "col")] + 1
out_chb_baseline_bySeg <- 
  out_chb_baseline %>% 
  dplyr::left_join(subset(surfwat_df, BC == "CHB"), by = c("row", "col")) %>% 
  dplyr::group_by(SegNum, kstpkper) %>% 
  dplyr::summarize(leakage_m3d_baseline = sum(leakage)*0.3048*0.3048*0.3048*86400) %>% 
  dplyr::mutate(BC = "CHB") %>% 
  dplyr::ungroup()

# combine
out_all_baseline <- 
  dplyr::bind_rows(out_str_baseline_bySeg, out_drn_baseline_bySeg, out_chb_baseline_bySeg)

## baseline - budget
mf_baseline <- 
  file.path(model_ws_simple, "RRCA12p_BudgetFlux.csv") %>% 
  readr::read_csv(col_types = cols()) %>% 
  # calculate net fluxes
  dplyr::mutate(STORAGE_NET = STORAGE_IN - STORAGE_OUT,
                CHB_NET = CONSTANT_HEAD_IN - CONSTANT_HEAD_OUT,
                WEL_NET = WELLS_IN - WELLS_OUT,
                DRN_NET = DRAINS_IN - DRAINS_OUT,
                EVT_NET = ET_IN - ET_OUT,
                RCH_NET = RECHARGE_IN - RECHARGE_OUT,
                STR_NET = STREAM_LEAKAGE_IN - STREAM_LEAKAGE_OUT,
                BALANCE_NET = `IN-OUT`,
                SP = seq(0, (length(kstpkper)-1)))

mf_baseline_melt <- 
  mf_baseline %>% 
  dplyr::select(STORAGE_NET, CHB_NET, WEL_NET, DRN_NET, EVT_NET, RCH_NET, STR_NET, SP, BALANCE_NET) %>% 
  reshape2::melt(id = c("SP"), value.name = "flux_baseline")

## cycle through wells
wells_df <- 
  file.path("results", "RRCA12p_03_WellSample.csv") %>% 
  readr::read_csv(col_types = cols()) %>% 
  subset(sample_lhs)
wells_all <- wells_df$WellNum

for (w in 1:length(wells_all)){
  ## get well info
  wel <- wells_all[w]
  
  ## load and process output
  # STR - already summarized by segment in python
  out_str_wel_bySeg <- 
    file.path(model_ws_wells, paste0("RRCA12p_Well", wel, "_STR_Leakage.csv")) %>% 
    readr::read_csv(col_types = cols()) %>% 
    dplyr::mutate(leakage_m3d = leakage*0.3048*0.3048*0.3048*86400) %>% 
    dplyr::mutate(BC = "STR",
                  WellNum = wel) %>% 
    dplyr::select(SegNum, kstpkper, leakage_m3d, BC, WellNum) %>% 
    dplyr::ungroup()
  
  # DRN
  out_drn_wel <- 
    file.path(model_ws_wells, paste0("RRCA12p_Well", wel, "_DRN_Leakage.csv")) %>% 
    readr::read_csv(col_types = cols())
  out_drn_wel[ ,c("row", "col")] <- out_drn_wel[ ,c("row", "col")] + 1
  out_drn_wel_bySeg <- 
    out_drn_wel %>% 
    dplyr::left_join(subset(surfwat_df, BC == "DRN"), by = c("row", "col")) %>% 
    dplyr::group_by(SegNum, kstpkper) %>% 
    dplyr::summarize(leakage_m3d = sum(leakage)*0.3048*0.3048*0.3048*86400) %>% 
    dplyr::mutate(BC = "DRN",
                  WellNum = wel) %>% 
    dplyr::ungroup()
  
  # CHB
  out_chb_wel <- 
    file.path(model_ws_wells, paste0("RRCA12p_Well", wel, "_CHB_Leakage.csv")) %>% 
    readr::read_csv(col_types = cols())
  out_chb_wel[ ,c("row", "col")] <- out_chb_wel[ ,c("row", "col")] + 1
  out_chb_wel_bySeg <- 
    out_chb_wel %>% 
    dplyr::left_join(subset(surfwat_df, BC == "CHB"), by = c("row", "col")) %>% 
    dplyr::group_by(SegNum, kstpkper) %>% 
    dplyr::summarize(leakage_m3d = sum(leakage)*0.3048*0.3048*0.3048*86400) %>% 
    dplyr::mutate(BC = "CHB",
                  WellNum = wel) %>% 
    dplyr::ungroup()
  
  # combine
  out_wel_baseline <- 
    dplyr::bind_rows(out_str_wel_bySeg, out_drn_wel_bySeg, out_chb_wel_bySeg) %>% 
    dplyr::left_join(out_all_baseline, by = c("SegNum", "kstpkper", "BC")) %>% 
    dplyr::mutate(leakage_change_m3d = (leakage_m3d_baseline - leakage_m3d))
  
  ## well - budget
  mf_well <- 
    file.path(model_ws_wells, paste0("RRCA12p_Well", wel, "_BudgetFlux.csv")) %>% 
    readr::read_csv(col_types = cols()) %>% 
    # calculate net fluxes
    dplyr::mutate(STORAGE_NET = STORAGE_IN - STORAGE_OUT,
                  CHB_NET = CONSTANT_HEAD_IN - CONSTANT_HEAD_OUT,
                  WEL_NET = WELLS_IN - WELLS_OUT,
                  DRN_NET = DRAINS_IN - DRAINS_OUT,
                  EVT_NET = ET_IN - ET_OUT,
                  RCH_NET = RECHARGE_IN - RECHARGE_OUT,
                  STR_NET = STREAM_LEAKAGE_IN - STREAM_LEAKAGE_OUT,
                  BALANCE_NET = `IN-OUT`,
                  SP = seq(0, (length(kstpkper)-1)),
                  WellNum = wel)
  
  # join with baseline
  mf_well_melt <- 
    mf_well %>% 
    dplyr::select(STORAGE_NET, CHB_NET, WEL_NET, DRN_NET, EVT_NET, RCH_NET, STR_NET, SP, BALANCE_NET, WellNum) %>% 
    reshape2::melt(id = c("SP", "WellNum"), value.name = "flux_pumped") %>% 
    dplyr::left_join(mf_baseline_melt, by = c("SP", "variable")) %>% 
    dplyr::mutate(flux_change = flux_baseline - flux_pumped)
  
  SP_pump_start <- min(mf_well_melt$SP[mf_well_melt$variable == "WEL_NET" & mf_well_melt$flux_change != 0])
  
  if (w == 1){
    out_all <- subset(out_wel_baseline, abs(leakage_change_m3d) > 1e-3)
    budget_all <- subset(mf_well_melt, SP >= SP_pump_start)
  } else {
    out_all <- dplyr::bind_rows(out_all, subset(out_wel_baseline, abs(leakage_change_m3d) > 1e-3))
    budget_all <- dplyr::bind_rows(budget_all, subset(mf_well_melt, SP >= SP_pump_start))
  }
  
  ## status update
  print(paste0(w, " of ", length(wells_all), " complete, ", Sys.time()))
  
}

## save output
out_all %>% 
  dplyr::left_join(RRCA12p_time[,c("kstpkper", "SP")], by = "kstpkper") %>% 
  dplyr::select(-kstpkper) %>% 
  dfDigits(digits = 3) %>% 
  readr::write_csv(path = file.path("results", "RRCA12p_05_MODFLOW-SummarizeLeakage.csv"))

budget_all %>% 
  dfDigits(digits = 3) %>% 
  readr::write_csv(path = file.path("results", "RRCA12p_05_MODFLOW-SummarizeBudget.csv"))
