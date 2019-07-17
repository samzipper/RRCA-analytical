## RRCA12p_CheckMODFLOWbudgets.R
# This script is intended to load the budget file results of the pumped MODFLOW
# simulations and check to see if the reduction in pumping is correct.

source(file.path("src", "paths+packages.R"))

## load MODFLOW baseline result
mf_baseline <- 
  file.path(model_ws_simple, "RRCA12p_BudgetFlux.csv") %>% 
  readr::read_csv() %>% 
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

# melt to join with pumped wells
mf_baseline_melt <- 
  mf_baseline %>% 
  dplyr::select(STORAGE_NET, CHB_NET, WEL_NET, DRN_NET, EVT_NET, RCH_NET, STR_NET, SP, BALANCE_NET) %>% 
  reshape2::melt(id = c("SP"), value.name = "flux_baseline")

# check math
mf_baseline %>% 
  dplyr::select(STORAGE_NET, CHB_NET, WEL_NET, DRN_NET, EVT_NET, RCH_NET, STR_NET, SP, BALANCE_NET) %>% 
  reshape2::melt(id = c("SP", "BALANCE_NET")) %>% 
  dplyr::group_by(SP, BALANCE_NET) %>% 
  dplyr::summarize(FLUX_SUM = sum(value)) %>% 
  ggplot(aes(x = FLUX_SUM, y = BALANCE_NET)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

## figure out which wells should have been run based on sample
wells_all <- 
  file.path("results", "RRCA12p_03_WellSample.csv") %>% 
  readr::read_csv()
well_sample <- wells_all$WellNum[wells_all$sample_lhs]

## get list of budget output files
well_budget_files <- 
  list.files(model_ws_wells, pattern = "BudgetFlux.csv")

# extract WellNum
well_complete <-
  stringr::str_split_fixed(well_budget_files, "_", 3)[, 2] %>% 
  stringr::str_sub(start = 5) %>% 
  as.numeric()

# see if any wells that should have been sampled are missing
well_sample[(well_sample %in% well_complete)]
well_sample[!(well_sample %in% well_complete)]

## scroll through each pumping test
for (w in 1:length(well_budget_files)){
  # read in budget
  mf_well <- 
    file.path(model_ws_wells, well_budget_files[w]) %>% 
    readr::read_csv(progress = F) %>% 
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
                  WellNum = well_complete[w])
  
  # join with baseline
  mf_well_melt <- 
    mf_well %>% 
    dplyr::select(STORAGE_NET, CHB_NET, WEL_NET, DRN_NET, EVT_NET, RCH_NET, STR_NET, SP, BALANCE_NET, WellNum) %>% 
    reshape2::melt(id = c("SP", "WellNum"), value.name = "flux_pumped") %>% 
    dplyr::left_join(mf_baseline_melt, by = c("SP", "variable")) %>% 
    dplyr::mutate(flux_change = flux_baseline - flux_pumped)
  
  # make overall data frame
  if (w == 1){
    mf_all <- mf_well_melt
  } else {
    mf_all <- rbind(mf_all, mf_well_melt)
  }
  
  # status update
  print(paste0("Sample ", w, " budget read-in complete"))
}

# check how many unique values there are (actualchanges)
mf_all_unique <- 
  mf_all %>% 
  dplyr::select(-WellNum) %>% 
  unique()

## for each well and timestep, extract change in WEL_NET (which should equal pumping rate)
wel_change <- 
  mf_all %>% 
  subset(variable == "WEL_NET") %>% 
  rename(wel_flux_change = flux_change)

## extract net change in all other fluxes 
nonwel_change <- 
  mf_all %>% 
  subset(!(variable %in% c("WEL_NET", "BALANCE_NET"))) %>% 
  dplyr::group_by(SP, WellNum) %>% 
  dplyr::summarize(nonwel_flux_change = sum(flux_change))

## extract net change in mass balance
balance_change <- 
  mf_all %>% 
  subset(variable == "BALANCE_NET") %>% 
  rename(balance_change = flux_change) %>% 
  dplyr::select(SP, WellNum, balance_change)

## join and plot
change_all <-
  dplyr::left_join(wel_change, nonwel_change, by = c("SP", "WellNum")) %>% 
  dplyr::left_join(balance_change, by = c("SP", "WellNum")) %>% 
  subset(wel_flux_change != 0 | nonwel_flux_change != 0 | balance_change != 0) %>% 
  dplyr::mutate(flux_mismatch = nonwel_flux_change + wel_flux_change)

ggplot(change_all, aes(x = wel_flux_change, y = nonwel_flux_change)) +
  geom_point() +
  geom_abline(intercept = 0, slope = -1, color = "red")

ggplot(change_all, aes(x = balance_change, y = flux_mismatch)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(change_all, aes(x = flux_mismatch)) +
  geom_histogram()
