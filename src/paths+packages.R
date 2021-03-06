## paths+packages.R

# load packages
library(tidyverse)
library(reshape2)
library(sf)
library(lubridate)
library(multcompView)
library(patchwork)

# path to folder on computer where large RRCA model input and output files live
#  these are version RRCA12p downloaded from http://www.republicanrivercompact.org/v12p/html/ch00.html
if (Sys.info()["nodename"] == "GSAS-PC5"){
  onedrive_ws <- "C:/Users/gsas/OneDrive - The University of Kansas/Research/StreamflowDepletion/RRCA"
} else if (Sys.info()["nodename"] %in% c("GHPC118", "GHPC117")) {
  onedrive_ws <- "C:/Users/samzipper/OneDrive - The University of Kansas/Research/StreamflowDepletion/RRCA"
} else {
  onedrive_ws <- "C:/Users/Sam/OneDrive - The University of Kansas/Research/StreamflowDepletion/RRCA"
}
model_ws_simple <- file.path(onedrive_ws, "baseline_simple")
model_ws_wells  <- file.path(onedrive_ws, "wells")

# some useful info about the RRCA12p model
RRCA12p_nrow <- 165
RRCA12p_ncol <- 326

# some info about time
RRCA12p_time <- tibble::tibble(
  kstpkper = paste0("(1, ", seq(1,996), ")"),
  SP = seq(1, 996),
  year = rep(seq(1918, 2000), each = 12),
  month = rep(seq(1, 12), times = 996/12),
  season = rep(c("Non-Pumping", "Non-Pumping", "Non-Pumping", "Non-Pumping", "Non-Pumping", "Pumping", 
                 "Pumping", "Pumping", "Pumping", "Non-Pumping", "Non-Pumping", "Non-Pumping"),
               times = 996/12)
) %>% 
  dplyr::mutate(date_mid = lubridate::ymd(paste0(year, "-", month, "-", round(lubridate::days_in_month(month)/2))),
                day_end = cumsum(lubridate::days_in_month(date_mid)))

## color palettes
# categorical color palette from https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

pal.season <- c("All" = "black", "Pumping"= col.cat.org, "Non-Pumping" = col.cat.blu)

## labels
labs_wellProperties <- c("Qw_m3d_abs" = "Pumping Rate [m\u00b3/d]",
                         "logTransmissivity_m2s" = "log(Trans) [m\u00b2/s]",
                         "ss_m" = "Specific Storage [m\u207b\u00b9]",
                         "distToClosestSurfwat_km" = "Distance to Water [km]",
                         "distToClosestEVT_km" = "Distance to ET [km]",
                         "WTD_SS_m" = "Water Table Depth [m]")

labs_ADF <- c("Adjacent_InvDist" = "Adjacent\nInverse Dist.",
              "Adjacent_InvDistSq" = "Adjacent\nInverse Dist. Squared",
              "Adjacent+Expanding_InvDist" = "Adjacent+Expanding\nInverse Dist.",
              "Adjacent+Expanding_InvDistSq" = "Adjacent+Expanding\nInverse Dist. Squared",
              "Adjacent_Web" = "Adjacent\nWeb",
              "Adjacent_WebSq" = "Adjacent\nWeb Squared",
              "Adjacent+Expanding_Web" = "Adjacent+Expanding\nWeb",
              "Adjacent+Expanding_WebSq" = "Adjacent+Expanding\nWeb Squared",
              "AnalyticalOnly_AnalyticalOnly" = "Analytical Only",
              "MODFLOW_MODFLOW" = "MODFLOW")

## ggplot theme
windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw(base_size=10, base_family="Arial") + 
    theme(
      text=element_text(color="black"),
      plot.title=element_text(face="bold", size=rel(1)),
      axis.title=element_text(face="bold", size=rel(1)),
      axis.text=element_text(size=rel(1)),
      strip.text=element_text(size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      legend.text=element_text(size=rel(1)),
      panel.grid=element_blank(),
      plot.margin=unit(c(1,1,1,1), "mm"),
      strip.background=element_blank())
}

theme_set(theme_scz())

## functions
# round all columns in data frame to reduce output file size
# from: https://stackoverflow.com/questions/29352659/r-write-csv-with-fixed-precision
dfDigits <- function(x, digits = 3) {
  ## x is a data.frame
  for (col in colnames(x)[sapply(x, class) == 'numeric'])
    x[,col] <- round(x[,col], digits = digits)
  x
}

# r squared
R2 <- function(sim, obs) {
  if (length(sim) != length(obs)) stop("vectors not the same size")
  return((sum((obs-mean(obs))*(sim-mean(sim)))/
            ((sum((obs-mean(obs))^2)^0.5)*(sum((sim-mean(sim))^2)^0.5)))^2)
}

## statistically significant differences between classes
generate_label_df <- function(TUKEY, variable){
  # function from: http://www.r-graph-gallery.com/84-tukey-test/
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}
