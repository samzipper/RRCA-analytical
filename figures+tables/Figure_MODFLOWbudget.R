## Figure_MODFLOWbudget.R
# Plot MODFLOW water budget by SP

source(file.path("src", "paths+packages.R"))

# load MODFLOW baseline budget
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

# melt to long-form
mf_melt <-
  mf_baseline %>% 
  dplyr::select(SP, STORAGE_NET, CHB_NET, WEL_NET, DRN_NET, EVT_NET, RCH_NET, STR_NET, BALANCE_NET) %>% 
  reshape2::melt(id = c("SP", "BALANCE_NET"))

# plot
ggplot(mf_melt) +
  geom_col(aes(x = SP, y = value*86400*0.3048*0.3048*0.3048, fill = variable)) +
  geom_hline(yintercept = 0, color = col.gray) +
  geom_line(data = mf_baseline, aes(x = SP, y = (BALANCE_NET*86400*0.3048*0.3048*0.3048)), 
            color = "black") +
  scale_x_continuous(name = "MODFLOW Stress Period [monthly, last 5 years]", 
                     limits = c(937, 996), expand = c(0,0)) +
  scale_y_continuous(name = "Net Flux [m\u00b3/d]") +
  scale_fill_discrete(name = "Store", 
                      labels = c("STORAGE_NET" = "Storage",
                                 "CHB_NET" = "CHB",
                                 "WEL_NET" = "WEL",
                                 "DRN_NET" = "DRN",
                                 "EVT_NET" = "EVT",
                                 "RCH_NET" = "RCH",
                                 "STR_NET" = "STR")) +
  theme(legend.position = "bottom") +
  ggsave(file.path("figures+tables", "Figure_MODFLOWbudget.png"),
         width = 190, height = 120, units = "mm")
