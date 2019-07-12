## paths+packages.R

# load packages
library(tidyverse)
library(reshape2)
library(sf)
library(lubridate)

# path to folder on computer where large RRCA model input and output files live
#  these are version RRCA12p downloaded from http://www.republicanrivercompact.org/v12p/html/ch00.html
if (Sys.info()["nodename"] == "GSAS-PC5"){
  onedrive_ws <- "C:/Users/gsas/OneDrive - The University of Kansas/Research/StreamflowDepletion/RRCA"
} else {
  onedrive_ws <- "C:/Users/Sam/OneDrive - The University of Kansas/Research/StreamflowDepletion/RRCA"
}
model_ws_simple <- file.path(onedrive_ws, "baseline_simple")

# some useful info about the RRCA12p model
RRCA12p_nrow <- 165
RRCA12p_ncol <- 326

## color palettes
# categorical color palette from https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

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

