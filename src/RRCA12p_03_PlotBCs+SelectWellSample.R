## RRCA12p_02_PlotBCs+SelectWellSample.R
#' Plot locations and pumping rates for wells and select a sample of wells for analysis.

source(file.path("src", "paths+packages.R"))

## load stress period data (created using script RRCA12p_Load+Simplify+RunBaseline.py)
wel_spd <- 
  file.path(model_ws_simple, "RRCA12p_WEL_StressPeriodData.csv") %>% 
  readr::read_csv() %>% 
  transform(Qw_acreFeetDay = 86400*Qw/43560)
wel_spd[,c("lay", "row", "col")] <- wel_spd[,c("lay", "row", "col")]+1  # python has 0-based indexing

str_spd <- 
  file.path(model_ws_simple, "RRCA12p_STR_StressPeriod1.csv") %>% 
  readr::read_csv() %>% 
  transform(cond_proportion = cond/cond_total,
            BC = "STR")
str_spd[,c("lay", "row", "col")] <- str_spd[,c("lay", "row", "col")]+1  # python has 0-based indexing

budget_spd <- 
  file.path(model_ws_simple, "RRCA12p_BudgetFlux.csv") %>% 
  readr::read_csv()

## extract constant head boundaries from ibound
ibound_df <-  
  file.path(model_ws_simple, "RRCA12p_BAS6-ibound.txt") %>% 
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
    distToStream_cells = as.numeric(well_str_dist)
  ) %>% 
  # get distance to closest stream
  dplyr::group_by(WellNum) %>% 
  dplyr::summarize(distToClosestStream_cells = min(distToStream_cells)) %>% 
  # add SegNum and other information about closest stream
  dplyr::left_join(tibble::tibble(
    WellNum = rep(wells_sf$WellNum, times = dim(well_str_dist)[2]),
    distToStream_cells = as.numeric(well_str_dist), 
    SegNum = rep(str_sf$SegNum, each = dim(well_str_dist)[1]),
    ReachNum = rep(str_sf$ReachNum, each = dim(well_str_dist)[1]),
    BC = rep(str_sf$BC, each = dim(well_str_dist)[1]),
    cond = rep(str_sf$cond, each = dim(well_str_dist)[1]),
    leakage = rep(str_sf$leakage, each = dim(well_str_dist)[1])),
    by = c("WellNum", "distToClosestStream_cells" = "distToStream_cells")) %>% 
  # some well-stream pairs have same distance; choose whichever has higher conductance
  group_by(WellNum) %>% 
  filter(cond == max(cond))

# there are still a few wells with multiple points (for example because conductance is identical); just choose one
well_str_df <- well_str_df[!duplicated(well_str_df$WellNum), ]

## load some model characteristics
vars_load <- c("LPF-hk", "LPF-ss", "LPF-sy", "DIS-top", "DIS-botm", "BAS6-strt", "BAS6-ibound", "RCH-rech")
model_df <- tibble::tibble(row = rep(seq(1, RRCA12p_nrow), each = RRCA12p_ncol),
                           col = rep(seq(1, RRCA12p_ncol), time = RRCA12p_nrow))

for (var in vars_load){
  # extract variable name
  var_name <- stringr::str_split(var, pattern = "-", n = 2)[[1]][2]
  
  # load python output
  var_vec <-   
    file.path(model_ws_simple, paste0("RRCA12p_", var, ".txt")) %>% 
    scan
  
  # add to data frame
  model_df <- tibble::add_column(model_df, !!(var_name) := var_vec)  # !! and := to name column using variable
  
}

# load ground surface data - this is not part of MODFLOW model
ground <- 
  file.path(onedrive_ws, "ground", "ground.avg.64") %>%
  scan(skip=1)
model_df$ground <- ground

# load head at start and end
head_SS <- 
  file.path(model_ws_simple, "RRCA12p_Head-SS.txt") %>% 
  scan()
model_df$head_SS <- head_SS

head_end <- 
  file.path(model_ws_simple, "RRCA12p_Head-End.txt") %>% 
  scan
model_df$head_end <- head_end

# calculate transmissivity, water table depth
model_df$sat_thickness <- model_df$top - model_df$botm
model_df$transmissivity_ft2s <- model_df$sat_thickness*model_df$hk
model_df$WTD_SS <- model_df$ground - model_df$head_SS
model_df$WTD_end <- model_df$ground - model_df$head_end

# add to well data frame
wells_all <- 
  wells %>% 
  dplyr::left_join(well_str_df, by = "WellNum") %>% 
  dplyr::left_join(model_df, by = c("row", "col")) %>% 
  transform(logTransmissivity_ft2s = log10(transmissivity_ft2s),
            logHk = log10(hk)) %>% 
  subset(ibound != 0) %>%   # remove 6 wells in inactive cells
  subset(distToClosestStream_cells > 0)  # remove wells in same cell as a BC feature (STR or CHB)

## select random sample of wells for pumping tests
# want to select wells to test based on:
#  - pumping rate (Qw_acreFeetDay_mean)
#  - log transmissivity (logTransmissivity_ft2s)
#  - storativity (ss)
#  - distance to stream (distToClosestStream_cells)
#  - water table depth (WTD_SS)
#  - log hydraulic conductivity (logHk)
#  - saturated thickness (sat_thickness)

# calculate 1st and 99th percentile for each variable
Qw_range <- quantile(wells_all$Qw_acreFeetDay_mean, c(0.01, 0.99))
logT_range <- quantile(wells_all$logTransmissivity_ft2s, c(0.01, 0.99))
ss_range <- quantile(wells_all$ss, c(0.01, 0.99))
dist_range <- quantile(wells_all$distToClosestStream_cells, c(0.01, 0.99))
WTD_range <- quantile(wells_all$WTD_SS, c(0.01, 0.99))
logHk_range <- quantile(wells_all$logHk, c(0.01, 0.99))
b_range <- quantile(wells_all$sat_thickness, c(0.01, 0.99))

# normalization function
fnorm <- function(x, range){
  (x - min(range))/(max(range) - min(range))
}

# make a data frame with everything normalized to 1st and 99th percentiles
wells_all_norm <- 
  data.frame(WellNum = wells_all$WellNum,
             Qw_acreFeetDay_mean = fnorm(wells_all$Qw_acreFeetDay_mean, Qw_range),
             logTransmissivity_ft2s = fnorm(wells_all$logTransmissivity_ft2s, logT_range),
             ss = fnorm(wells_all$ss, ss_range),
             distToClosestStream_cells = fnorm(wells_all$distToClosestStream_cells, dist_range),
             WTD_SS = fnorm(wells_all$WTD_SS, WTD_range),
             logHk = fnorm(wells_all$logHk, logHk_range),
             sat_thickness = fnorm(wells_all$sat_thickness, b_range)) %>% 
  subset(Qw_acreFeetDay_mean >= 0 & Qw_acreFeetDay_mean <= 1 &
           logTransmissivity_ft2s >= 0 & logTransmissivity_ft2s <= 1 &
           ss >= 0 & ss <= 1 &
           distToClosestStream_cells >= 0 & distToClosestStream_cells <= 1 &
           WTD_SS >= 0 & WTD_SS <= 1 &
           logHk >= 0 & logHk <= 1 &
           sat_thickness >= 0 & sat_thickness <= 1)

# subset wells_all based on percentile
wells_all <- subset(wells_all, WellNum %in% wells_all_norm$WellNum)

# random sample of wells
n_wells <- 250
set.seed(n_wells)
i_sample <- base::sample(seq(1, length(wells_all$WellNum)), 
                         size = n_wells)
wells_all$sample_random <- FALSE
wells_all$sample_random[i_sample] <- TRUE

# plot random sample distribution of all variables
wells_all %>% 
  dplyr::select(WellNum, sample_random, Qw_acreFeetDay_mean, logTransmissivity_ft2s, ss, distToClosestStream_cells, WTD_SS, logHk, sat_thickness) %>% 
  reshape2::melt(id = c("WellNum", "sample_random")) %>%
  ggplot() +
  geom_histogram(aes(x = value, fill = sample_random)) +
  facet_wrap(~ variable, scales = "free", nrow = 2) +
  scale_y_continuous(name = "Number of Wells") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))

## sample using latin hypercube approach
set.seed(n_wells)
lhs_mat <- lhs::randomLHS(n = n_wells, k = 7)  # 7 parameters

wells_mat <- 
  wells_all_norm %>% 
  dplyr::select(-WellNum) %>% 
  as.matrix()

# for each row in sample, find closest row in wells_mat
euc_dist_func <- function(p, q){
  # p and q are vectors of equal length
  (sum(((p - q)^2))^0.5)
}
well_sample_lhs <- integer(0)
replace <- T  # can wells be matched multiple times (T), or should they be removed once matched (F)? 
# T will give a closer approximation to uniform distribution, but result in fewer unique wells than n_wells
for (s in 1:n_wells){
  if (replace){
    euc_dist <- apply(wells_mat, MARGIN=1, FUN=euc_dist_func, lhs_mat[s,])
    well_min_dist <- wells_all_norm$WellNum[which.min(euc_dist)]
  } else {
    # figure out what wells haven't been selected yet
    WellNums_remaining <- wells_all_norm$WellNum[!(wells_all_norm$WellNum %in% well_sample_lhs)]
    i_remaining <- which(wells_all_norm$WellNum %in% WellNums_remaining)
    
    # subset normalized value matrix, calculate distance
    wells_mat_remaining <- wells_mat[i_remaining, ]
    
    euc_dist <- apply(wells_mat_remaining, MARGIN=1, FUN=euc_dist_func, lhs_mat[s,])
    well_min_dist <- wells_all_norm$WellNum[i_remaining][which.min(euc_dist)]
  }
  
  well_sample_lhs <- c(well_sample_lhs, well_min_dist)
  
  
  print(paste0(s, " complete"))
}

wells_all$sample_lhs <- FALSE
wells_all$sample_lhs[wells_all$WellNum %in% well_sample_lhs] <- TRUE

# plot LHS sample distribution of all variables
wells_all %>% 
  dplyr::select(WellNum, sample_lhs, Qw_acreFeetDay_mean, logTransmissivity_ft2s, ss, distToClosestStream_cells, WTD_SS, logHk, sat_thickness) %>% 
  reshape2::melt(id = c("WellNum", "sample_lhs")) %>%
  ggplot() +
  geom_histogram(aes(x = value, fill = sample_lhs)) +
  facet_wrap(~ variable, scales = "free", nrow = 2) +
  scale_y_continuous(name = "Number of Wells") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))

## save summary of wells
wells_all %>% 
  # convert to metric units
  transform(hk_ms = hk*0.3048,
            distToClosestStream_m = distToClosestStream_cells*5280*0.3048,
            top_m = top*0.3048,
            botm_m = botm*0.3048,
            strt_m = strt*0.3048,
            rech_ms = rech*0.3048,
            ground_m = ground*0.3048,
            head_SS_m = head_SS*0.3048,
            head_end_m = head_end*0.3048,
            Qw_m3d_mean = Qw_acreFeetDay_mean*1233.48) %>% 
  dplyr::select(WellNum, row, col, yr_pump_start, yr_pump_end, Qw_m3d_mean, hk_ms,
                ss, sy, distToClosestStream_m, ground_m, top_m, botm_m, strt_m, rech_ms,
                head_SS_m, head_end_m, sample_lhs) %>% 
  readr::write_csv(file.path("results", "RRCA12p_03_WellSample.csv"))

## inspect model via comparison with documentation
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
model_df$WTD_SS_cut <- cut(model_df$WTD_SS, breaks=c(-500,-50,-30,-20,-10,-1,1,50,100,150,200,300,500))
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=WTD_SS_cut)) + 
  geom_raster() +
  scale_y_reverse() +
  scale_fill_manual(values=rev(c("grey25", "red","orange","yellow","limegreen","forestgreen","grey50",
                                 "cyan","cornflowerblue","blue","deeppink2","purple")))

# 2000 wtd - should match http://www.republicanrivercompact.org/v12p/html/dtw12p-997.html
model_df$WTD_end_cut <- cut(model_df$WTD_end, breaks=c(-500,-50,-30,-20,-10,-1,1,50,100,150,200,300,500))
ggplot(subset(model_df, ibound != 0), aes(x=col, y=row, fill=WTD_end_cut)) + 
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