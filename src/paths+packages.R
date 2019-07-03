## paths+packages.R

# load packages
library(tidyverse)
library(reshape2)
library(sf)
library(lubridate)

# path to folder on computer where large RRCA model input and output files live
#  these are version RRCA12p downloaded from http://www.republicanrivercompact.org/v12p/html/ch00.html
onedrive_ws <- "C:/Users/Sam/OneDrive - The University of Kansas/Research/StreamflowDepletion/RRCA"
model_ws_simple <- file.path(onedrive_ws, "baseline_simple")

# some useful info about the RRCA12p model
RRCA12p_nrow <- 165
RRCA12p_ncol <- 326

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

