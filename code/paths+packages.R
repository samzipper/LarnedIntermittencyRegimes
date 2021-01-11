## paths+packages.R
# a place to keep variables, functions, etc. relevant across numerous scripts

# load packages
library(zoo)
library(rnoaa)
library(tidyverse)
library(patchwork)
library(dataRetrieval)
library(lubridate)
options(dplyr.summarise.inform=F)   # suppress summarize info
ncores <- (parallel::detectCores() - 1)  # number of cores to use for any parallel stuff

## info about site/analysis
USGS_gage <- "07141220"   # USGS gage ID for Ark @ Larned
last_date <- "2020-12-31" # when analysis should end

## relevant paths
dir_data <- "Z:/Common/Larned Research Site/data"  # location for storing big files

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
