## RRCA_Calibration_CalculateFitStats.R
#' This script calculates some fit statistics for the RRCA model based on 
#' points digitized from Figures S4, S5, and S6 using https://apps.automeris.io/wpd/

source(file.path("src", "paths+packages.R"))

df_S4 <- 
  readr::read_csv(file.path("results", "RRCA_Calibration_PointsFromFigS4.csv"),
                  col_names = F) %>% 
  dplyr::rename(observed = X1, predicted = X2) %>% 
  dplyr::mutate(figure = "S4")

df_S5 <- 
  readr::read_csv(file.path("results", "RRCA_Calibration_PointsFromFigS5.csv"),
                  col_names = F) %>% 
  dplyr::rename(observed = X1, predicted = X2) %>% 
  dplyr::mutate(figure = "S5")

df_S6 <- 
  readr::read_csv(file.path("results", "RRCA_Calibration_PointsFromFigS6.csv"),
                  col_names = F) %>% 
  dplyr::rename(observed = X1, predicted = X2) %>% 
  dplyr::mutate(figure = "S6")

# combine
df_all <- dplyr::bind_rows(df_S4, df_S5, df_S6)

# calculate some fit stats
hydroGOF::NSE(df_all$predicted, df_all$observed)
hydroGOF::NSE(df_S4$predicted, df_S4$observed)
hydroGOF::NSE(df_S5$predicted, df_S5$observed)
hydroGOF::NSE(df_S6$predicted, df_S6$observed)

hydroGOF::rmse(df_all$predicted, df_all$observed)
hydroGOF::rmse(df_S4$predicted, df_S4$observed)
hydroGOF::rmse(df_S5$predicted, df_S5$observed)
hydroGOF::rmse(df_S6$predicted, df_S6$observed)

hydroGOF::nrmse(df_all$predicted, df_all$observed, norm = "maxmin")
hydroGOF::nrmse(df_S4$predicted, df_S4$observed, norm = "maxmin")
hydroGOF::nrmse(df_S5$predicted, df_S5$observed, norm = "maxmin")
hydroGOF::nrmse(df_S6$predicted, df_S6$observed, norm = "maxmin")


ggplot(df_upstream, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope  = 1)
