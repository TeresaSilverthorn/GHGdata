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
### Import data  16-March-2021  #  CA02 -  CA01
#################################

# RAW DATA is in different files zipped .txt files
# load all the files

#Set working directory to source file location of raw LGR files
setwd("~/Data/Campaign1_03.2021/LGR_raw_data")

CA02 <- read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/LGR_raw_data/2021-03-16_LGR/n2o-co_2021-03-16_f0001.txt/n2o-co_2021-03-16_f0001.txt", skip=1, header=T, sep=",nrows="10")


