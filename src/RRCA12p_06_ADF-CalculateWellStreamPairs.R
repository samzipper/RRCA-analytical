## RRCA12p_06_ADF-CalculateWellStreamPairs.R
#' Calculate well-stream pairs for use in analytical depletion functions.

source(file.path("src", "paths+packages.R"))

## load data
# surface water
surfwat_df <- 
  file.path("results", "RRCA12p_03_Surfwat.csv") %>% 
  readr::read_csv()

# model data frame
model_df <- 
  file.path("results", "RRCA12p_03_ModelData.csv") %>% 
  readr::read_csv() %>% 
  dplyr::mutate(sat_thickness_m = top_m - botm_m,
                WTD_SS_m = ground_m - head_SS_m,
                WTD_end_m = ground_m - head_end_m,
                transmissivity_m2s = hk_ms*sat_thickness_m)

# well locations and characteristics
wells_df <- 
  file.path("results", "RRCA12p_03_WellSample.csv") %>% 
  readr::read_csv() %>% 
  subset(sample_lhs) %>% 
  dplyr::mutate(sat_thickness_m = top_m - botm_m,
                WTD_SS_m = ground_m - head_SS_m,
                WTD_end_m = ground_m - head_end_m,
                transmissivity_m2s = hk_ms*sat_thickness_m,
                logTransmissivity_m2s = log10(transmissivity_m2s),
                logHk_ms = log10(hk_ms),
                Qw_m3d_abs = abs(Qw_m3d_mean))

## create sfs
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
    stream_width_m = rep(surfwat_sf$width, times = dim(well_surfwat_dist)[2]),
    stream_row = rep(surfwat_df$row, times = dim(well_surfwat_dist)[2]),
    stream_col = rep(surfwat_df$col, times = dim(well_surfwat_dist)[2]),
    stream_BC = rep(surfwat_sf$BC, times = dim(well_surfwat_dist)[2]),
    WellNum = rep(wells_df$WellNum, each = dim(well_surfwat_dist)[1]),
    well_row = rep(wells_df$row, each = dim(well_surfwat_dist)[1]),
    well_col = rep(wells_df$col, each = dim(well_surfwat_dist)[1]),
    well_elev_m = rep(wells_df$ground_m, each = dim(well_surfwat_dist)[1]),
    well_screenTopDepth_m = rep((wells_df$ground_m - wells_df$top_m), each = dim(well_surfwat_dist)[1]),
    well_wtd_m = rep(wells_df$WTD_SS_m, each = dim(well_surfwat_dist)[1]),
    dist_wellToStreamPoints_m = as.numeric(well_surfwat_dist)*5280*0.3048,  # convert cells to m
    Tr_well_m2d = rep(wells_df$transmissivity_m2s*86400, each = dim(well_surfwat_dist)[1]),  # convert m2/s to m2/d
    Tr_bulk_m2d = NaN,
    ss_well_m = rep(wells_df$ss_m, each = dim(well_surfwat_dist)[1]),
    ss_bulk_m = NaN,
    sy_well = rep(wells_df$sy, each = dim(well_surfwat_dist)[1]),
    sy_bulk = NaN,
    lmda_m2d = rep(surfwat_sf$cond*0.3048*0.3048*86400, times = dim(well_surfwat_dist)[2])  # convert ft2/s to m2/d
  )

dist_singlePt_df <- 
  dist_df %>% 
  # get closest point per well-stream combination
  dplyr::group_by(WellNum, SegNum) %>% 
  dplyr::summarize(dist_wellToStream_m = min(dist_wellToStreamPoints_m)) %>% 
  # add SegNum and other information about closest stream
  dplyr::left_join(
    dist_df, by = c("WellNum", "SegNum", "dist_wellToStream_m"="dist_wellToStreamPoints_m")) %>% 
  # some well-stream pairs have same multiple points with same distance; choose whichever has higher conductance
  dplyr::group_by(WellNum, SegNum) %>% 
  filter(lmda_m2d == max(lmda_m2d))

# there are still two wells with multiple points (for example because conductance is identical); just choose one
dist_singlePt_df <- dist_singlePt_df[!duplicated(dist_singlePt_df[, c("WellNum", "SegNum", "stream_BC")]), ]

# max number of segments to consider as potential options for each well
max.well.segs <- 15
df_all <-
  dist_singlePt_df %>% 
  group_by(WellNum) %>% 
  top_n(-max.well.segs, dist_wellToStream_m)  # - value to select smallest distances for each WellNum

## calculate bulk transmissivity
r_trans_m2s <-
  model_df %>% 
  dplyr::select(col, row, transmissivity_m2s) %>% 
  magrittr::set_colnames(c("x", "y", "transmissivity_m2s")) %>% 
  raster::rasterFromXYZ(crs="+init=epsg:26714")
r_trans_m2s[r_trans_m2s <= 0] <- NA

r_trans_ss_m <- 
  model_df %>% 
  dplyr::select(col, row, ss_m) %>% 
  magrittr::set_colnames(c("x", "y", "ss")) %>% 
  raster::rasterFromXYZ(crs="+init=epsg:26714")
r_trans_ss_m[r_trans_ss_m <= 0] <- NA

r_trans_sy <- 
  model_df %>% 
  dplyr::select(col, row, sy) %>% 
  magrittr::set_colnames(c("x", "y", "sy")) %>% 
  raster::rasterFromXYZ(crs="+init=epsg:26714")
r_trans_sy[r_trans_sy <= 0] <- NA

## for each well-stream pair...
start.time <- Sys.time()
for (i in 1:dim(df_all)[1]){
  # calculate based on all points intersecting a line
  # make a line connecting well to closest point on stream segment
  sf.connector <- 
    sf::st_nearest_points(subset(wells_sf, WellNum==dist_df$WellNum[i]), 
                          subset(surfwat_sf, SegNum==dist_df$SegNum[i])) %>% 
    sf::st_sf() %>% 
    dplyr::mutate(length = as.numeric(sf::st_length(.))) %>% 
    dplyr::filter(length == min(length))
  
  sp.connector <-
    sf.connector[1,] %>%  # just in case two lines have same length
    as(., "Spatial") %>% 
    as(., "SpatialLines")
  
  # at each point along segment extract data
  cellnums <- raster::extract(r_trans_m2s, sp.connector, cellnumbers = T)[[1]]
  df.connector <- 
    data.frame(
      transmissivity_m2d = cellnums[,"transmissivity_m2s"]*86400,
      ss_m = raster::extract(r_trans_ss_m, cellnums[,"cell"]),
      sy = raster::extract(r_trans_sy, cellnums[,"cell"])
    )
  df.connector <- df.connector[complete.cases(df.connector), ]  # remove NAs - happens if connector cuts across an area outside domain
  
  # calculate properties
  cell.size <- 5280*0.3048  # cell size in m
  df_all$Tr_bulk_m2d[i] <- (cell.size*dim(df.connector)[1])/sum(cell.size/df.connector$transmissivity_m2d)
  df_all$ss_bulk_m[i] <- mean(df.connector$ss_m, na.rm = T)
  df_all$sy_bulk[i] <- mean(df.connector$sy, na.rm = T)
  
  # status update
  print(paste0(i, " of ", dim(df_all)[1], " complete, ", round(difftime(Sys.time(), start.time, units="min"), 2), " min"))

}

## save output
df_all %>% 
  write.csv(file.path("results", "RRCA12p_06_ADF-CalculateWellStreamPairs.csv"),
            row.names=F)
