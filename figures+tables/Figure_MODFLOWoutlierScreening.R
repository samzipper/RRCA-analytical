## Figure_MODFLOWoutlierScreening.R
# This figure is meant to show how MODFLOW results are screened for outliers

source(file.path("src", "paths+packages.R"))

modflow_budget_df <- 
  file.path("results", "RRCA12p_05_MODFLOW-SummarizeBudget.csv") %>% 
  readr::read_csv()

w <- 2219

SP_start <- min(modflow_budget_df$SP[modflow_budget_df$WellNum == w & modflow_budget_df$variable == "WEL_NET"])  # when pumping starts

df_bal_change <- tibble::tibble(SP = modflow_budget_df$SP[modflow_budget_df$WellNum == w & modflow_budget_df$variable == "BALANCE_NET"],
                                BalChange = modflow_budget_df$flux_change[modflow_budget_df$WellNum == w & modflow_budget_df$variable == "BALANCE_NET"])
SP_outliers <- df_bal_change$SP[tsoutliers::tso(ts(df_bal_change$BalChange), type = "AO", cval = 5)[["outliers"]]$ind]

df_plot <- 
  dplyr::bind_rows(tibble::tibble(SP = seq(1, SP_start-1),
                                  BalChange = 0),
                   df_bal_change)

ggplot() + 
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = SP_start, xmax = min(SP_outliers)-1, fill = col.cat.blu, alpha = 0.25) +
  annotate("text", x = (SP_start + min(SP_outliers))/2, y = max(df_bal_change$BalChange), label = "Period of Comparison", color = col.cat.blu) +
  geom_line(data = df_plot, aes(x = SP, y = BalChange)) +
  geom_point(data = subset(df_bal_change, SP %in% SP_outliers), aes(x = SP, y = BalChange), color = col.cat.red) +
  scale_x_continuous(name = "Stress Period [monthly]", expand = c(0,0)) + 
  scale_y_continuous(name = "Change in MODFLOW Mass Balance Error\n(baseline - pump disabled) [ft\u00b3/s]") +
  ggsave(file.path("figures+tables", "Figure_MODFLOWoutlierScreening.png"),
         width = 190, height = 120, units = "mm")
