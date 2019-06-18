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
  transform(cond_proportion = cond/cond_total,
            BC = "STR")
str_spd[,c("lay", "row", "col")] <- str_spd[,c("lay", "row", "col")]+1  # python has 0-based indexing

budget_spd <- 
  file.path(path_RRCA, "RRCA12p_BudgetFlux.csv") %>% 
  readr::read_csv()

## extract constant head boundaries from ibound
ibound_df <-  
  file.path("modflow", "baseline_simple", "RRCA12p_BAS6-ibound.txt") %>% 
  scan %>% 
  tibble::tibble(row = rep(seq(1, RRCA12p_nrow), each = RRCA12p_ncol),
                 col = rep(seq(1, RRCA12p_ncol), time = RRCA12p_nrow),
                 ibound = .)

# split CHB into segments
# script needs to be run in this specific order
CHB_SegNum_start <- max(str_spd$SegNum)

ibound_df$SegNum <- 0
ibound_df$SegNum[ibound_df$SegNum == 0 & ibound_df$row > 150] <- CHB_SegNum_start + 1
ibound_df$SegNum[ibound_df$SegNum == 0 & ibound_df$col == max(subset(ibound_df, ibound == -1)$col)] <- CHB_SegNum_start + 3
ibound_df$SegNum[ibound_df$SegNum == 0 & ibound_df$row > 80] <- CHB_SegNum_start + 2
ibound_df$SegNum[ibound_df$SegNum == 0 & ibound_df$col > 290] <- CHB_SegNum_start + 4
ibound_df$SegNum[ibound_df$SegNum == 0 & ibound_df$col > 275] <- CHB_SegNum_start + 5
ibound_df$SegNum[ibound_df$SegNum == 0 & ibound_df$col == 275] <- CHB_SegNum_start + 6
ibound_df$SegNum[ibound_df$SegNum == 0 & ibound_df$col > 225] <- CHB_SegNum_start + 7
ibound_df$SegNum[ibound_df$SegNum == 0 & ibound_df$col > 173] <- CHB_SegNum_start + 8
ibound_df$SegNum[ibound_df$SegNum == 0 & ibound_df$col > 100] <- CHB_SegNum_start + 9
ggplot(subset(ibound_df, ibound == -1), aes(x=col, y=row, color=factor(SegNum))) + geom_point()

# add some other columns to match up with str_spd
ibound_df <- 
  ibound_df %>% 
  subset(ibound == -1) %>% 
  dplyr::select(-ibound) %>% 
  transform(lay = 1,
            ReachNum = 0,
            flow = 0,
            stage = 0,
            cond = 0,
            sbot = 0,
            stop = 0,
            width = 0,
            slope = 0,
            rough = 0,
            cond_total = 0,
            leakage = 0,
            cond_proportion = 0,
            BC = "CHB")

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

## monthly budget
monthly_budget <- 
  budget_spd %>% 
  left_join(df_time, by="kstpkper") %>% 
  dplyr::select(-PERCENT_DISCREPANCY, -kstpkper, -TOTAL_IN, -TOTAL_OUT, -`IN-OUT`) %>% 
  reshape2::melt(id=c("year", "month", "date_mid", "sp"), value.name="flux_ft3s", variable.name="store") %>% 
  subset(is.finite(year)) %>% 
  transform(flux_acreFeetMonth = 86400*days_in_month(month)*flux_ft3s/43560)

yearly_budget <- 
  monthly_budget %>% 
  dplyr::group_by(year, store) %>% 
  dplyr::summarize(flux_acreFeetYear = sum(flux_acreFeetMonth))

## decadal budget: should approximately match table on pages 5-6 of RRCA report
yearly_budget$decade <- cut(yearly_budget$year, breaks=seq(1920,2000,10))

decade_budget <-
  yearly_budget %>% 
  subset(is.finite(decade)) %>% 
  dplyr::group_by(decade, store) %>% 
  dplyr::summarize(meanFlux_acreFeetYear = mean(flux_acreFeetYear)) %>% 
  dcast(decade ~ store)

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
  rbind(str_spd, ibound_df) %>% 
  sf::st_as_sf(., coords=c("col", "row"), crs="+init=epsg:26714")

well_str_dist <- 
  sf::st_distance(x=wells_sf, y=str_sf)
well_str_df <- 
  tibble::tibble(
    WellNum = rep(wells_sf$WellNum, times = dim(well_str_dist)[2]),
    distToStream_m = as.numeric(well_str_dist)
  ) %>% 
  # get distance to closest stream
  dplyr::group_by(WellNum) %>% 
  dplyr::summarize(distToClosestStream_m = min(distToStream_m)) %>% 
  # add SegNum and other information about closest stream
  dplyr::left_join(tibble::tibble(
    WellNum = rep(wells_sf$WellNum, times = dim(well_str_dist)[2]),
    distToStream_m = as.numeric(well_str_dist)), 
    SegNum = rep(str_sf$SegNum, each = dim(well_str_dist)[1]),
    ReachNum = rep(str_sf$ReachNum, each = dim(well_str_dist)[1]),
    BC = rep(str_sf$BC, each = dim(well_str_dist)[1]),
    cond = rep(str_sf$cond, each = dim(well_str_dist)[1]),
    leakage = rep(str_sf$leakage, each = dim(well_str_dist)[1]),
    by = c("WellNum", "distToClosestStream_m" = "distToStream_m"))

## load some model characteristics
vars_load <- c("LPF-hk", "LPF-ss", "LPF-sy", "DIS-top", "DIS-botm", "BAS6-strt", "BAS6-ibound", "RCH-rech")
model_df <- tibble::tibble(row = rep(seq(1, RRCA12p_nrow), each = RRCA12p_ncol),
                           col = rep(seq(1, RRCA12p_ncol), time = RRCA12p_nrow))

for (var in vars_load){
  # extract variable name
  var_name <- stringr::str_split(var, pattern = "-", n = 2)[[1]][2]
  
  # load python output
  var_vec <-   
    file.path("modflow", "baseline_simple", paste0("RRCA12p_", var, ".txt")) %>% 
    scan
  
  # add to data frame
  model_df <- tibble::add_column(model_df, !!(var_name) := var_vec)  # !! and := to name column using variable
  
}

# load ground surface data - this is not part of MODFLOW model
ground <- 
  file.path(path_RRCA, "ground", "ground.avg.64") %>%
  scan(skip=1)
model_df$ground <- ground

# load head at start and end
head_SS <- 
  file.path("modflow", "baseline_simple", "RRCA12p_Head-SS.txt") %>% 
  scan()
model_df$head_SS <- head_SS

head_end <- 
  file.path("modflow", "baseline_simple", "RRCA12p_Head-End.txt") %>% 
  scan
model_df$head_end <- head_end

# calculate transmissivity
model_df$transmissivity_ft2s <- (model_df$top - model_df$botm)*model_df$hk

# add to well data frame
wells_all <- 
  wells %>% 
  dplyr::left_join(well_str_df, by = "WellNum") %>% 
  dplyr::left_join(model_df, by = c("row", "col"))

ggplot() +
  geom_raster(data = subset(model_df, ibound != 0), aes(x = col, y = row, fill = hk)) +
  geom_point(data = wells_all, aes(x = col, y = row, color = distToClosestStream_m), shape = 21) +
  viridis::scale_fill_viridis(trans = "log10") +
  NULL


## inspect model
# land surface - should match http://www.republicanrivercompact.org/v12p/html/ground.html
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=ground)) + 
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradientn(colors = c("purple", "deeppink", "blue", "cyan", "green", "yellow", "red"),
                       breaks = seq(1000,6000,1000))

# thickness - should match http://www.republicanrivercompact.org/v12p/html/dz.html
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=(top-botm))) + 
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradientn(colors = c("purple", "deeppink", "blue", "cyan", "green", "yellow", "red"),
                       breaks = seq(0,1000,200), limits=c(0,800))

# specific yield - should match http://www.republicanrivercompact.org/v12p/html/sy12p.html
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=factor(sy))) + 
  geom_raster() +
  scale_y_reverse()

# hydraulic conductivity - should match http://www.republicanrivercompact.org/v12p/html/k12p.html
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=hk*86400)) + 
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradientn(trans="log10", colors = c("purple", "deeppink", "blue", "cyan", "green", "yellow", "red"))

# saturated thickness - should match http://www.republicanrivercompact.org/v12p/html/st12p.html
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=(top-botm))) + 
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradientn(colors = c("brown", "purple", "deeppink", "blue", "cyan", "green", "yellow", "red"))

# transmissivity - should match http://www.republicanrivercompact.org/v12p/html/t12p.html
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=transmissivity_ft2s*86400)) + 
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradientn(trans="log10", colors = c("purple", "deeppink", "blue", "cyan", "green", "yellow", "red"))

# recharge - should match http://www.republicanrivercompact.org/v12p/html/rch/001.html
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=rech*86400)) + 
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradientn(trans="log10", colors = c("purple", "deeppink", "blue", "cyan", "green", "yellow", "red"))

# SS WTD - should match http://www.republicanrivercompact.org/v12p/html/dtw12p-1.html
model_df$wtd_SS <- cut((model_df$ground-model_df$head_SS), breaks=c(-500,-50,-30,-20,-10,-1,1,50,100,150,200,300,500))
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=wtd_SS)) + 
  geom_raster() +
  scale_y_reverse() +
  scale_fill_manual(values=rev(c("grey25", "red","orange","yellow","limegreen","forestgreen","grey50",
                                 "cyan","cornflowerblue","blue","deeppink2","purple")))

# 2000 wtd - should match http://www.republicanrivercompact.org/v12p/html/dtw12p-997.html
model_df$wtd_end <- cut((model_df$ground-model_df$head_end), breaks=c(-500,-50,-30,-20,-10,-1,1,50,100,150,200,300,500))
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=wtd_end)) + 
  geom_raster() +
  scale_y_reverse() +
  scale_fill_manual(values=rev(c("grey25", "red","orange","yellow","limegreen","forestgreen","grey50",
                                 "cyan","cornflowerblue","blue","deeppink2","purple")))

# drawdown to 2000 - should approximately match http://www.republicanrivercompact.org/v12p/html/dh12p-997.html
model_df$ddn <- cut((model_df$head_SS-model_df$head_end), breaks=c(-500,-50,-20,-10,-5,-2,-1,1,2,5,10,20,50,500))
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=ddn)) + 
  geom_raster() +
  scale_y_reverse() +
  scale_fill_manual(values=rev(c("black", "red","orange","yellow","limegreen","forestgreen","grey50",
                             "cyan","cornflowerblue","blue","deeppink2","purple","brown")))






wells <- dplyr::left_join(wells, well_str_df, by="WellNum")

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
