# Install packages 
library(lubridate)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(plyr)
library(gasfluxes)
library(tidyverse)
library(data.table)

#################################
### Import data  25-March-2021  #  AL07   AL06   AL04
#################################

# RAW DATA is in different .txt files 
# load all the files

LGRraw_2021.03.25_1 <-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/LGR_raw_data/2021-03-25_LGR/n2o-co_2021-03-25_f0000.txt", skip="1", header=T, sep=",", nrows=571)