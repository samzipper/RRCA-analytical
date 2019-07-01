## paths+packages.R

# load packages
library(tidyverse)
library(reshape2)
library(sf)
library(lubridate)

# path to folder on computer where large RRCA model input and output files live
#  these are version RRCA12p downloaded from http://www.republicanrivercompact.org/v12p/html/ch00.html
onedrive_ws <- "C:/Users/Sam/OneDrive - The University of Kansas/Research/StreamflowDepletion/RRCA"
model_ws_simple = file.path(onedrive_ws, "baseline_simple")
output_simple = file.path("modflow", "baseline_simple")

# some useful info about the RRCA12p model
RRCA12p_nrow <- 165
RRCA12p_ncol <- 326
