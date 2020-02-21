## Figure_SamplePumpingSchedule.R

source(file.path("src", "paths+packages.R"))

# stress period data (created using script RRCA12p_03_Load+Simplify+RunBaseline.py)
wel_spd <- 
  file.path(model_ws_simple, "RRCA12p_WEL_StressPeriodData.csv") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(Qw_m3d = abs(0.3048*0.3048*0.3048*86400*Qw))
wel_spd[,c("lay", "row", "col")] <- wel_spd[,c("lay", "row", "col")]+1  # python has 0-based indexing

# get well sample
wells_df <- 
  file.path("results", "RRCA12p_03_WellSample.csv") %>% 
  readr::read_csv() %>% 
  subset(sample_lhs)

# join WellNum and subset to sample
wel_spd_sample <-
  dplyr::left_join(wel_spd, wells_df, by = c("row", "col")) %>% 
  subset(is.finite(WellNum))

# time to plot: last 5 years
SP_end <- max(RRCA12p_time$SP)
SP_start <- SP_end - (12*5) + 1

# subset data to plot period only
time_plot <- subset(RRCA12p_time, SP >= SP_start & SP <= SP_end)

WellNum_sample <- unique(wel_spd_sample$WellNum)
for (w in 1:length(WellNum_sample)){
  w_plot <- 
    wel_spd_sample %>% 
    subset(WellNum == WellNum_sample[w]) %>% 
    dplyr::right_join(RRCA12p_time, by = "kstpkper") %>% 
    subset(SP >= SP_start & SP <= SP_end) %>% 
    tidyr::replace_na(list("Qw_m3d" = 0, "WellNum" = WellNum_sample[w]))
  
  if (w == 1){
    wel_plot <- w_plot
  } else {
    wel_plot <- dplyr::bind_rows(wel_plot, w_plot)
  }
}

## plot
ggplot(wel_plot, aes(x = SP, y = Qw_m3d, group = WellNum)) +
  geom_line(color = col.gray, alpha = 0.5) +
  scale_x_continuous(name = "MODFLOW Stress Period [monthly, last 5 years]", expand = c(0,0)) +
  scale_y_continuous(name = "Pumping Rate [m\u00b3/d]") +
  ggsave(file.path("figures+tables", "Figure_Wells-SamplePumpingSchedule.png"), 
         width = 190, height = 95, units = "mm")
