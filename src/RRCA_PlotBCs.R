## RRCA_ModelDescription_WEL.R
#' Plot locations and pumping rates for wells.

source(file.path("src", "paths+packages.R"))

## load stress period data (created using script RRCA12p_Load+Simplify+RunBaseline.py)
wel_spd <- 
  file.path(path_RRCA, "RRCA12p_WEL_StressPeriodData.csv") %>% 
  read.csv() %>% 
  transform(Qw_acreFeetDay = 86400*Qw/43560)
wel_spd[,c("lay", "row", "col")] <- wel_spd[,c("lay", "row", "col")]+1  # python has 0-based indexing

str_spd <- 
  file.path(path_RRCA, "RRCA12p_STR_StressPeriod1.csv") %>% 
  read.csv()
str_spd[,c("lay", "row", "col")] <- str_spd[,c("lay", "row", "col")]+1  # python has 0-based indexing

## calculate year/month/mid-month date for each stress period
sp_first <- 1918
sp_last  <- 2000
df_time <- 
  data.frame(year = rep(seq(sp_first, sp_last), each=12),
             month = rep(seq(1,12), times=(1+sp_last-sp_first))) %>% 
  transform(date_mid = ymd(paste(year, month, round(days_in_month(month)/2), sep="-")),
            sp = seq(1,length(year)))

## calculate total pumping at each timestep
monthly_Qw <- 
  wel_spd %>% 
  left_join(df_time, by="sp") %>% 
  dplyr::group_by(year, month, date_mid, sp) %>% 
  dplyr::summarize(Qw_acreFeetDaySum = sum(Qw_acreFeetDay)) %>% 
  transform(Qw_acreFeetMonth = Qw_acreFeetDaySum*days_in_month(date_mid))

yearly_Qw <- 
  monthly_Qw %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(Qw_acreFeetYear = sum(Qw_acreFeetMonth))

## plot
ggplot(monthly_Qw, aes(x=date_mid, y=-Qw_acreFeetMonth)) +
  geom_line()

ggplot(yearly_Qw, aes(x=year, y=-Qw_acreFeetYear/1000)) +
  geom_line()

# compare to table on page 6 of RRCA report - close but not exactly the same
mean(yearly_Qw$Qw_acreFeetYear[yearly_Qw$year %in% seq(1981,1990)])
mean(yearly_Qw$Qw_acreFeetYear[yearly_Qw$year %in% seq(1991,2000)])

## locations of wells and stream
wel_locations <- unique(wel_spd[,c("row", "col")])

ggplot() +
  geom_point(data=wel_locations, aes(x=col, y=row)) +
  geom_raster(data=str_spd, aes(x=col, y=row, fill=SegNum))

## number of stream reaches per segment
str_spd %>% 
  group_by(SegNum) %>% 
  summarize(nreach = max(ReachNum)) %>% 
  ggplot(aes(x=nreach)) +
  geom_histogram(binwidth=1)
