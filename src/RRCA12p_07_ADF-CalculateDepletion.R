## RRCA12p_07_ADF-CalculateDepletion.R
#' Do all ADF calculations: proximity criteria, depletion apportionment, analytical model

source(file.path("src", "paths+packages.R"))
require(streamDepletr)

## some parameters controlling ADF calculations
analytical_model <- "glover"  # analytical model to use: "hunt" or "glover"
str_BCs <- c("STR", "DRN", "CHB")  # surface water BCs to consider: c("STR", "DRN", "CHB")
apportionment <- "WebSq"  # depletion apportionment equation: "Web" or "WebSq"
storage <- "sy"   # "ss" or "sy"

## load pumping wells
# well locations and characteristics
wells_df <- 
  file.path("results", "RRCA12p_03_WellSample.csv") %>% 
  readr::read_csv() %>% 
  subset(sample_lhs)

# well-stream pairs
well_str_df <- 
  file.path("results", "RRCA12p_06_ADF-CalculateWellStreamPairs.csv") %>% 
  readr::read_csv() %>% 
  subset(stream_BC %in% str_BCs)
colnames(well_str_df)[colnames(well_str_df) == paste0(storage, "_bulk")] <- "S_bulk"

# stress period data (created using script RRCA12p_03_Load+Simplify+RunBaseline.py)
wel_spd <- 
  file.path(model_ws_simple, "RRCA12p_WEL_StressPeriodData.csv") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(Qw_m3d = abs(0.3048*0.3048*0.3048*86400*Qw))  # original units: ft3/s
wel_spd[,c("lay", "row", "col")] <- wel_spd[,c("lay", "row", "col")]+1  # python has 0-based indexing

## find distance from each well to each surface water
# surface water
surfwat_df <- 
  file.path("results", "RRCA12p_03_Surfwat.csv") %>% 
  readr::read_csv() %>% 
  subset(BC %in% str_BCs)

# calculate distance to closest stream segment for each well
wells_sf <- 
  wells_df %>% 
  sf::st_as_sf(., coords=c("col", "row"), crs="+init=epsg:26714")
surfwat_sf <- 
  surfwat_df %>% 
  sf::st_as_sf(., coords=c("col", "row"), crs="+init=epsg:26714")

## get distances
well_surfwat_dist <- 
  sf::st_distance(x=surfwat_sf, y=wells_sf)
dist_df <- 
  data.frame(
    SegNum = rep(surfwat_sf$SegNum, times = dim(well_surfwat_dist)[2]),
    WellNum = rep(wells_df$WellNum, each = dim(well_surfwat_dist)[1]),
    dist_wellToStreamPoints_m = as.numeric(well_surfwat_dist)*5280*0.3048  # convert ft2/s to m2/d
  )

## list of wells to analyze
wells_all <- unique(well_str_df$WellNum)
for (w in 1:length(wells_all)){
  ## get well info
  wel <- wells_all[w]
  wel_row <- wells_df$row[wells_df$WellNum == wel]
  wel_col <- wells_df$col[wells_df$WellNum == wel]
  
  ## extract stress period data
  spd <- 
    wel_spd %>% 
    subset(row == wel_row & col == wel_col) %>% 
    dplyr::right_join(RRCA12p_time, by = "kstpkper") %>% 
    tidyr::replace_na(list("Qw_m3d" = 0))
  spd$day_start <- c(1, spd$day_end[1:length(spd$day_end)-1]+1)
  wel_pump_start <- min(spd$day_start[spd$Qw_m3d > 0])
  
  ## compress identical pumping stress periods for streamDepletr
  i.starts <- which(c(1, diff(spd$Qw_m3d)) != 0)
  i.ends <- c((i.starts-1)[2:length(i.starts)], i.starts[length(i.starts)])
  pump_schedule <-
    data.frame(StartOfMonthDays = spd$day_start[i.starts],
               EndOfMonthDays = spd$day_end[i.ends],
               Qw_m3d = spd$Qw_m3d[i.ends])
  
  ## get well-stream pairs
  str_df <- subset(well_str_df, WellNum == wel)
  
  ## stream proximity criteria
  # find adjacent catchments using thiessen polygon
  wel_apportion_poly <- 
    str_df %>% 
    dplyr::select(SegNum, dist_wellToStream_m, stream_col, stream_row) %>% 
    streamDepletr::apportion_polygon(., 
                                     wel_lon = wel_col, 
                                     wel_lat = wel_row,
                                     crs = sp::CRS("+init=epsg:26714"),
                                     reach_name = "SegNum",
                                     dist_name = "dist_wellToStream_m",
                                     lon_name = "stream_col",
                                     lat_name = "stream_row") %>% 
    magrittr::set_colnames(c("SegNum", "frac_depletion"))
  
  # figure out mean pumping rate for all months after the start of pumping
  Qw_m3d_mean <- mean(spd$Qw_m3d[spd$day_start >= wel_pump_start])
  
  # find maximum distance, based on maximum observed S, Tr, lmda (inclusive estimate)
  min_frac <- 0.01
  dist_inc_m <- 5280*0.3048  # increment of 1 model cell (1 mile)
  max_dist_prev <- max(c(min(str_df$dist_wellToStream_m), dist_inc_m))
  
  spd$max_dist_m <- NaN
  wel_start_flag <- T
  for (sp in spd$SP[spd$day_end > wel_pump_start]){
    
    # distance threshold
    time_days <- spd$day_end[spd$SP == sp]
    time_since_pump_start <- time_days - wel_pump_start
    if (analytical_model == "glover") {
      max_dist <- streamDepletr::depletion_max_distance(Qf_thres = min_frac,
                                                        d_interval = dist_inc_m,
                                                        d_min = max_dist_prev,
                                                        d_max = max(str_df$dist_wellToStream_m),
                                                        method = "glover",
                                                        t = time_since_pump_start,
                                                        S = min(str_df$S_bulk),
                                                        Tr = max(str_df$Tr_bulk_m2d))
    } else if (analytical_model == "hunt") {
      max_dist <- streamDepletr::depletion_max_distance(Qf_thres = min_frac,
                                                        d_interval = dist_inc_m,
                                                        d_min = max_dist_prev,
                                                        d_max = max(str_df$dist_wellToStream_m),
                                                        method = "hunt",
                                                        t = time_since_pump_start,
                                                        S = min(str_df$S_bulk),
                                                        Tr = max(str_df$Tr_bulk_m2d),
                                                        lmda = max(str_df$lmda_m2d))
    } else {
      stop("Analytical model not defined correctly; choose glover or hunt")
    }
    spd$max_dist_m[sp] <- max_dist
    if (max_dist > max_dist_prev) max_dist_prev <- max_dist
    
    ## depletion apportionment
    if (apportionment == "Web") { 
      web_exp <- 1 
    } else if (apportionment == "WebSq") { 
      web_exp <- 2 
    } else { 
      stop("Depletion apportionment not defined correctly; choose Web or WebSq") 
    } 
    
    # distance to all point on each segment
    str_all_df <- 
      dist_df %>% 
      subset(WellNum == wel)
    
    wel_apportion_web <- 
      str_all_df %>% 
      dplyr::select(SegNum, dist_wellToStreamPoints_m) %>% 
      subset((dist_wellToStreamPoints_m <= max_dist) | (SegNum %in% wel_apportion_poly$SegNum)) %>% 
      streamDepletr::apportion_web(., 
                                   w = web_exp,
                                   min_frac = min_frac,
                                   reach_name = "SegNum",
                                   dist_name = "dist_wellToStreamPoints_m") %>% 
      magrittr::set_colnames(c("SegNum", "frac_depletion")) %>% 
      dplyr::mutate(WellNum = wel,
                    time_days = time_days,
                    time_since_pump_start_days = time_since_pump_start,
                    SP = sp)
    
    ## combine output
    if (wel_start_flag){
      apportion_wel <- wel_apportion_web
      wel_start_flag <- F
    } else {
      apportion_wel <- rbind(apportion_wel, wel_apportion_web)
    }
    
  }
  
  ## now: run analytical model
  # join with input data and figure out all unique well-stream combinations
  wel_str_input <-
    dplyr::left_join(apportion_wel, well_str_df, by = c("SegNum", "WellNum"))
  wel_str_combos <-
    wel_str_input %>% 
    dplyr::select(SegNum, WellNum, dist_wellToStream_m, S_bulk, Tr_bulk_m2d, lmda_m2d) %>% 
    unique()
  
  # calculate depletion through time for each combo
  for (i in 1:dim(wel_str_combos)[1]){
    # identify well-seg combo
    seg <- wel_str_combos$SegNum[i]
    
    # get times
    output_t_days <- wel_str_input$time_days[wel_str_input$SegNum == seg & wel_str_input$WellNum == wel]
    output_frac <- wel_str_input$frac_depletion[wel_str_input$SegNum == seg & wel_str_input$WellNum == wel]
    # pump
    if (analytical_model == "glover") {
      Qs <- intermittent_pumping(t = output_t_days,
                                 starts = pump_schedule$StartOfMonthDays,
                                 stops  = pump_schedule$EndOfMonthDays,
                                 rates  = pump_schedule$Qw_m3d,
                                 method = "glover",
                                 d = wel_str_combos$dist_wellToStream_m[i],
                                 S = wel_str_combos$S_bulk[i],
                                 Tr = wel_str_combos$Tr_bulk_m2d[i])
      
    } else if (analytical_model == "hunt") {
      Qa <- intermittent_pumping(t = output_t_days,
                                 starts = pump_schedule$StartOfMonthDays,
                                 stops  = pump_schedule$EndOfMonthDays,
                                 rates  = pump_schedule$Qw_m3d,
                                 method = "hunt",
                                 d = wel_str_combos$dist_wellToStream_m[i],
                                 S = wel_str_combos$S_bulk[i],
                                 Tr = wel_str_combos$Tr_bulk_m2d[i],
                                 lmda = wel_str_combos$lmda_m2d[i])
    } else {
      stop("Analytical model not defined correctly; choose glover or hunt")
    }
    
    # compile output
    combo_Qa <- data.frame(SegNum = seg,
                           WellNum = wel,
                           time_days = output_t_days,
                           Qa = Qs)
    
    if (i == 1){
      wel_Qa <- combo_Qa
    } else {
      wel_Qa <- rbind(wel_Qa, combo_Qa)
    }
  }
  
  wel_out <- 
    dplyr::left_join(apportion_wel, wel_Qa, by = c("SegNum", "WellNum", "time_days")) %>% 
    dplyr::left_join(spd[,c("day_end", "Qw_m3d")], by = c("time_days" = "day_end")) %>% 
    dplyr::mutate(depletion_m3d = Qa*frac_depletion) %>% 
    subset(depletion_m3d > 1e-3)
  
  if (w == 1){
    depletion_all <- wel_out
  } else {
    depletion_all <- rbind(depletion_all, wel_out)
  }
  
  ## status update
  print(paste0(w, " of ", length(wells_all), " complete, ", Sys.time()))
}

## save output
fname <- paste0("RRCA12p_07_ADF-CalculationDepletion_", analytical_model, "_", storage, "_", apportionment, "_", paste(str_BCs, collapse = "-"), ".csv")
depletion_all %>% 
  dplyr::select(WellNum, SegNum, time_days, Qw_m3d, frac_depletion, Qa, depletion_m3d) %>% 
  dfDigits(digits = 3) %>% 
  readr::write_csv(path = file.path("results", fname))

## look at output
depletion_all %>% 
  subset(WellNum == 11233) %>% 
  dplyr::group_by(WellNum, time_days) %>% 
  dplyr::summarize(Qw = mean(Qw_m3d),
                   Qs = sum(depletion_m3d)) %>% 
  ggplot(aes(x = time_days)) +
  geom_line(aes(y = Qs), color = "red") + 
  geom_line(aes(y = Qw), color = "blue")
