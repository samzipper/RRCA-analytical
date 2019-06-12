## RRCA12p_02_PlotBCs+SelectWellSample.R
#' Plot locations and pumping rates for wells and select a sample of wells for analysis.

source(file.path("src", "paths+packages.R"))

## load stress period data (created using script RRCA12p_Load+Simplify+RunBaseline.py)
wel_spd <- 
  file.path(path_RRCA, "RRCA12p_WEL_StressPeriodData.csv") %>% 
  readr::read_csv() %>% 
  transform(Qw_acreFeetDay = 86400*Qw/43560)
wel_spd[,c("lay", "row", "col")] <- wel_spd[,c("lay", "row", "col")]+1  # python has 0-based indexing

str_spd <- 
  file.path(path_RRCA, "RRCA12p_STR_StressPeriod1.csv") %>% 
  readr::read_csv() %>% 
  transform(cond_proportion = cond/cond_total)
str_spd[,c("lay", "row", "col")] <- str_spd[,c("lay", "row", "col")]+1  # python has 0-based indexing

## calculate year/month/mid-month date for each stress period
# get stress periods from kstpkper
kstpkper <- unique(wel_spd$kstpkper)
sp <- 
  strsplit(kstpkper, split="[[:punct:]]+") %>% 
  lapply(., function(l) l[[3]]) %>% 
  unlist(.) %>% 
  as.numeric()
sp_data <- tibble::tibble(kstpkper = kstpkper,
                          sp = sp)

# translate to real-world time
sp_first <- 1918
sp_last  <- 2000
df_time <- 
  tibble::tibble(year = rep(seq(sp_first, sp_last), each=12),
                 month = rep(seq(1,12), times=(1+sp_last-sp_first))) %>% 
  transform(date_mid = lubridate::ymd(paste(year, month, round(lubridate::days_in_month(month)/2), sep="-")),
            sp = seq(1,length(year))) %>% 
  dplyr::left_join(sp_data, by="sp")

## calculate total pumping at each timestep
monthly_Qw <- 
  wel_spd %>% 
  dplyr::left_join(df_time, by="kstpkper") %>% 
  dplyr::group_by(year, month, date_mid, sp) %>% 
  dplyr::summarize(Qw_acreFeetDaySum = sum(Qw_acreFeetDay)) %>% 
  transform(Qw_acreFeetMonth = Qw_acreFeetDaySum*lubridate::days_in_month(date_mid))

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
wells <-
  wel_spd %>% 
  dplyr::left_join(df_time, by="kstpkper") %>% 
  dplyr::group_by(row, col) %>% 
  dplyr::summarize(Qw_acreFeetDay_mean = mean(Qw_acreFeetDay),
                   yr_pump_start = min(year),
                   yr_pump_end = max(year),
                   yr_pump_length = length(unique(year))) %>% 
  transform(WellNum = seq(1, length(yr_pump_end)))

# calculate distance to closest stream segment for each well
wells_sf <- 
  wells %>% 
  sf::st_as_sf(., coords=c("col", "row"), crs="+init=epsg:26714")
str_sf <- 
  str_spd %>% 
  sf::st_as_sf(., coords=c("col", "row"), crs="+init=epsg:26714")

dist_well_str <- 
  sf::st_distance(x=wells_sf, y=str_sf)
df_well_str <- 
  tibble::tibble(
    WellNum = rep(wells_sf$WellNum, times = dim(dist_well_str)[2]),
    SegNum = rep(str_sf$SegNum, each = dim(dist_well_str)[1]),
    distToStream_m = as.numeric(dist_well_str)
  ) %>% 
  dplyr::group_by(WellNum) %>% 
  dplyr::summarize(distToClosestStream_m = min(distToStream_m))

## add to data frame:
#    transmissivity at well
#    conductance of closest stream
#    number of stream cells within (some distance threshold)





wells <- dplyr::left_join(wells, df_well_str, by="WellNum")

# random sample of wells considering location (row/col), pumping rate, pumping duration, and distance to stream
n_wells <- 100
set.seed(n_wells)
wells_sample <-
  wells %>% 
  dplyr::tbl_df() %>% 
  dplyr::sample_n(n_wells)






ggplot() +
  geom_point(data=wells, aes(x=col, y=row), shape=21, alpha=0.25, fill=NA) +
  geom_point(data=wells_sample, aes(x=col, y=row), color="red") +
  geom_raster(data=str_spd, aes(x=col, y=row, fill=leakage)) +
  scale_fill_gradient2()

ggplot(wells, aes(x=Qw_acreFeetDay_mean)) +
  geom_histogram(binwidth=1)

## number of stream reaches per segment
str_spd %>% 
  dplyr::group_by(SegNum) %>% 
  dplyr::summarize(nreach = max(ReachNum)) %>% 
  ggplot(aes(x=nreach)) +
  geom_histogram(binwidth=1)
