## RRCA12p_07a_AnalyticalOnly-CalculateDepletion.R
#' Calculate depletion using closest stream reach only (no proximity criteria or depletion apportionment)

source(file.path("src", "paths+packages.R"))
require(streamDepletr)

## some parameters controlling ADF calculations
analytical_model <- "glover"  # analytical model to use: "hunt" or "glover"
str_BCs <- c("STR", "DRN", "CHB")  # surface water BCs to consider: c("STR", "DRN", "CHB")
storage <- "ss_bulk_m"   # "ss_bulk_m", "ss_well_m", "sy_bulk", or "sy_well"

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
colnames(well_str_df)[colnames(well_str_df) == storage] <- "S_bulk"

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
start_flag <- T
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
  str_df <- 
    subset(well_str_df, WellNum == wel) %>% 
    top_n(-1, dist_wellToStream_m) %>% # - value to select smallest distances for each WellNum
    top_n(1, Tr_bulk_m2d) %>%  # for equidistant streams, larger transmissivity
    top_n(-1, S_bulk) %>%      # for equal Trans, smaller storage
    top_n(1, lmda_m2d)         # for equal storage, larger lmda
  seg <- str_df$SegNum
  
  output_t_days <- spd$day_end[spd$day_end > wel_pump_start]
  output_frac <- rep(1, times = length(output_t_days))
  
  # pump
  if (analytical_model == "glover") {
    Qs <- intermittent_pumping(t = output_t_days,
                               starts = pump_schedule$StartOfMonthDays,
                               stops  = pump_schedule$EndOfMonthDays,
                               rates  = pump_schedule$Qw_m3d,
                               method = "glover",
                               d = str_df$dist_wellToStream_m,
                               S = str_df$S_bulk,
                               Tr = str_df$Tr_bulk_m2d)
    
  } else if (analytical_model == "hunt") {
    Qs <- intermittent_pumping(t = output_t_days,
                               starts = pump_schedule$StartOfMonthDays,
                               stops  = pump_schedule$EndOfMonthDays,
                               rates  = pump_schedule$Qw_m3d,
                               method = "hunt",
                               d = str_df$dist_wellToStream_m,
                               S = str_df$S_bulk,
                               Tr = str_df$Tr_bulk_m2d,
                               lmda = str_df$lmda_m2d)
  } else {
    stop("Analytical model not defined correctly; choose glover or hunt")
  }
  
  # compile output
  wel_Qa <- data.frame(SegNum = seg,
                       WellNum = wel,
                       time_days = output_t_days,
                       depletion_m3d = Qs)
  
  wel_out <- 
    dplyr::left_join(wel_Qa, spd[,c("day_end", "Qw_m3d")], by = c("time_days" = "day_end")) %>% 
    subset(depletion_m3d > 1e-3)
  
  if (start_flag){
    depletion_all <- wel_out
    start_flag <- F
  } else {
    depletion_all <- dplyr::bind_rows(depletion_all, wel_out)
  }
  
  ## status update
  print(paste0(w, " of ", length(wells_all), " complete, ", Sys.time()))
  
}

## save output
fname <- paste0("RRCA12p_07a_ADF-CalculationDepletion_", analytical_model, "_", storage, "_AnalyticalOnly_", paste(str_BCs, collapse = "-"), ".csv")
depletion_all %>% 
  dplyr::select(WellNum, SegNum, time_days, Qw_m3d, depletion_m3d) %>% 
  dfDigits(digits = 3) %>% 
  readr::write_csv(path = file.path(onedrive_ws, "results", fname))

## look at output
depletion_all %>% 
  subset(WellNum == 11233) %>% 
  dplyr::group_by(WellNum, time_days) %>% 
  dplyr::summarize(Qw = mean(Qw_m3d),
                   Qs = sum(depletion_m3d)) %>% 
  ggplot(aes(x = time_days)) +
  geom_line(aes(y = Qs), color = "red") + 
  geom_line(aes(y = Qw), color = "blue")
