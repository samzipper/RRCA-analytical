## Figure_SeasonsPumpingWinter.R

source(file.path("src", "paths+packages.R"))

# stress period data (created using script RRCA12p_03_Load+Simplify+RunBaseline.py)
wel_spd <- 
  file.path(model_ws_simple, "RRCA12p_WEL_StressPeriodData.csv") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(Qw_m3d = abs(0.3048*0.3048*0.3048*86400*Qw))

# monthly sum for final year
wel_sum <- 
  wel_spd %>% 
  dplyr::left_join(RRCA12p_time, by = "kstpkper") %>% 
  subset(year == max(RRCA12p_time$year)) %>% 
  dplyr::group_by(month, season) %>% 
  dplyr::summarize(Qw_sum_m3d = sum(Qw_m3d)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Qw_sum_m3mo = Qw_sum_m3d*lubridate::days_in_month(month),
                Qw_sum_km3d = Qw_sum_m3d/1e6)

ggplot(wel_sum, aes(x = factor(month), y = Qw_sum_km3d, fill = season)) +
  geom_col() +
  scale_x_discrete(name = "Month") +
  scale_y_continuous(name = "Groundwater Abstraction [km\u00b3/d]") +
  scale_fill_manual(name = "Season", 
                    values = pal.season) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank()) +
  ggsave(file.path("figures+tables", "Figure_SeasonsPumpingWinter.png"),
         width = 95, height = 95, units = "mm")
