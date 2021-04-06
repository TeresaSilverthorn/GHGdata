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
# Due to the metadata at the end of the files, must use fread() which automatically cuts it out

LGRraw_2021.03.25_1 <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/LGR_raw_data/2021-03-25_LGR/n2o-co_2021-03-25_f0000.txt")
      
