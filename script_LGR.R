##########################################################################
##### Script for importing and processing N2O data from LGR analyzer #####
##########################################################################
#Notes

# You need to calculate the time difference between the start/end times and the LGR time for each campaign-


################################
# Install packages 
library(lubridate)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(plyr)
library(gasfluxes)
library(tidyverse)
library(data.table)
library(stringr)
library(car)
library(dpseg)

#################################
######## Import data  ##########
#################################


# load all the data files from each campaign 
# RAW DATA is in different .txt files 
# Due to the metadata at the end of the files, must use fread() which automatically cuts it out


#Campaign 1 (March 16 - 25, 2021) 

LGR_MAR2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data_MAR2021/LGR_raw_data", pattern='txt', full.names=T, recursive=TRUE), fread ,header=T))
str(LGR_MAR2021) #131258 obs. of 36 vars


#Campaign 3 (June 21, 2021 - July 1, 2021) 
LGR_JUN2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign3_raw_data_JUN2021/LGR_raw_data", pattern='txt', full.names=T, recursive=TRUE), fread ,header=T))
str(LGR_JUN2021) #130263 obs. of  36 vars


#Campaign 5 (September 13 - 21, 2021) 
LGR_SEP2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign5_raw_data_SEP2021/LGR_raw_data", pattern='txt', full.names=T, recursive=TRUE), fread ,header=T))
str(LGR_SEP2021) #57406 obs. of  36 vars 
#A bit suspicious that there are half as many observations, but I guess I made fewer measurements over time. 


#Campaign 7 (November 2021 22 - December 3, 2021) 
LGR_NOV2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign7_raw_data_NOV2021/LGR_raw_data", pattern='txt', full.names=T, recursive=TRUE), fread ,header=T))
str(LGR_NOV2021)
#95818 obs of 36 vars


#############################################################################

##### Combine (vertically) all of the data from each of the campaigns #######

#Maybe you need to correct the times before you combine?

#Combine the campaigns together vertically
LGR_2021 <- rbind(LGR_MAR2021,LGR_JUN2021, LGR_SEP2021, LGR_NOV2021)
str(LGR_2021) #414745 obs. of  36 variables

#Subset just the useful columns (Time, [N2O]d_ppm, AmbT_C)
LGR_2021<- subset(LGR_2021, select = c( "Time", "[N2O]d_ppm", "AmbT_C"))
str(LGR_2021) #414745 obs. of  3 variables

#Rename the N2O column to get rid of the square brackets because they can cause some issues later
names(LGR_2021)[names(LGR_2021) == "[N2O]d_ppm"] <- "N2O_ppm"

# change the time format to "as.POSIXct" time - R's time format
options(digits.secs=3) # this keeps the 3 decimal second fractions
LGR_2021$Time <- as.POSIXct(LGR_2021$Time, format = "%d/%m/%Y %H:%M:%OS", tz="Europe/Paris")


write.csv(LGR_2021, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/LGR_data_processed2021.csv")


################################################################
# Import the ancillary data files with the start and end times #
################################################################

#import the ancillary data files for the aquatic and riparian measurements

ancil_dat_aquatic <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary data/GHG_data_entry_2021-Aquatic_2022-01-14.csv", header=T)
str(ancil_dat_aquatic) #1069 obs. of  31 variables

ancil_dat_riparian <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary data/GHG_data_entry_2021-Riparian_2022-01-14.csv", header=T)
str(ancil_dat_riparian) #1010 obs. of  25 variables


#add an ID column which combines site, point, and campaign

#Could consider adding date or campaign to the ID column, but instead maybe subset by campaign, and run the loop separately for each campaign

ancil_dat_aquatic$ID <- paste(ancil_dat_aquatic$siteID_new,ancil_dat_aquatic$point, ancil_dat_aquatic$campaign, sep="_")

ancil_dat_riparian$ID <- paste(ancil_dat_riparian$siteID_new,ancil_dat_riparian$point, ancil_dat_riparian$campaign, sep="_" )

#In order to merge the riparian and aquatic ancillary data, need to differentiate between awuatic and riparian collar heights, so rename aquatic collar heights to "sed_collarheight1" etc. 

names(ancil_dat_aquatic)[names(ancil_dat_aquatic) == "collar_height1"] <- "sed_collar_height1"
names(ancil_dat_aquatic)[names(ancil_dat_aquatic) == "collar_height2"] <- "sed_collar_height2"
names(ancil_dat_aquatic)[names(ancil_dat_aquatic) == "collar_height3"] <- "sed_collar_height3"
names(ancil_dat_aquatic)[names(ancil_dat_aquatic) == "collar_volume_L"] <- "sed_collar_volume_L"

#combine the riparian and aquatic ancillary data
ancil_dat <- bind_rows(ancil_dat_aquatic, ancil_dat_riparian)

#add the date to the start and end time columns 
ancil_dat$time_start<- as.POSIXct(paste(ancil_dat$date, ancil_dat$time_start), format="%Y-%m-%d %H:%M")
ancil_dat$time_end<- as.POSIXct(paste(ancil_dat$date, ancil_dat$time_end), format="%Y-%m-%d %H:%M")

#make soil temp and VWC average column
ancil_dat$soil_temp <- rowMeans(ancil_dat[,c('soil_temp1', 'soil_temp2', 'soil_temp3')], na.rm=TRUE)
ancil_dat$VWC <- rowMeans(ancil_dat[,c('VWC_1', 'VWC_2', 'VWC_3')], na.rm=TRUE)

#Take the average also of sediment temperature and moisture, as well as collar height for dry fluxes
ancil_dat$sed_temp <- rowMeans(ancil_dat[,c('sed_temp1', 'sed_temp2', 'sed_temp3')], na.rm=TRUE)
ancil_dat$sed_VWC <- rowMeans(ancil_dat[,c('sed_VWC1', 'sed_VWC2', 'sed_VWC3')], na.rm=TRUE)
ancil_dat$sed_collar_height <- rowMeans(ancil_dat[,c('sed_collar_height1', 'sed_collar_height2', 'sed_collar_height3')], na.rm=TRUE)


#replace the resulting NaN's with NA's
ancil_dat$soil_temp[is.nan(ancil_dat$soil_temp)]<-NA
ancil_dat$VWC[is.nan(ancil_dat$VWC)]<-NA
ancil_dat$sed_temp[is.nan(ancil_dat$sed_temp)]<-NA
ancil_dat$sed_VWC[is.nan(ancil_dat$sed_VWC)]<-NA
ancil_dat$collar_height[is.nan(ancil_dat$collar_height)]<-NA

#Take the pool_width1 column (width2 is the length I think, so discard), divide by 100 to get metres and add them to the stream width column
ancil_dat$stream_width_m_2 <- ancil_dat$pool_riffle_width1  /100
ancil_dat <- ancil_dat %>% mutate(stream_width = coalesce(stream_width_m, stream_width_m_2)) 

#Now rename the old stream width column and update the new one to stream_width_m
names(ancil_dat)[names(ancil_dat) == "stream_width_m"] <- "stream_width_m_old"
names(ancil_dat)[names(ancil_dat) == "stream_width"] <- "stream_width_m"


#Subset the ancillary data for only LGR data
ancil_dat<-ancil_dat[ancil_dat$Picarro_LGR=="LGR",]

#Subset just the useful columns: Date, ID, drying_regime, soil_temp, VWC, Picarro_LGR, total_volume_L, chamber_area_m2, flowing_state, habitat_type, pool_riffle_depth_cm, time_start, time_end 

ancil_dat <- ancil_dat %>% select( "ID_unique", "campaign", "date", "siteID_new", "ID", "time_start", "time_end", "drying_regime", "soil_temp", "VWC", "Picarro_LGR", "total_volume_L", "chamber_area_m2", "flow_state", "habitat_type", "stream_width_m", "pool_riffle_depth_cm", "sed_temp", "sed_VWC")

#rename Site column
names(ancil_dat)[names(ancil_dat) == "siteID_new"] <- "Site"

str(ancil_dat) #707 obs of 19 vars

write.csv(ancil_dat, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/LGR_ancil_dat.csv")


###################################################
#### Ancillary Data Cleaning: Long intervals #####
##################################################

#data cleaning : check if there are any super long or super short intervals (negative) (potentially typos) 

ancil_dat$int <- difftime(ancil_dat$time_end, ancil_dat$time_start, unit = "mins")
#ancil_dat <- ancil_dat %>% mutate(long_int=ifelse(int>=6,T,F))

#manual edit in the google doc file, and re download

# Make an if else statement modifying the end time to 5 minutes after the start time if the interval is greater than 5 minutes
#ancil_dat$time_end <-data.table::fifelse(ancil_dat$long_int=="TRUE", ancil_dat$time_start+300, ancil_dat$time_end)

#The first few, and last few seconds are often wonky (remove)

ancil_dat$time_start <- ancil_dat$time_start +30
ancil_dat$time_end <- ancil_dat$time_end -30

head(ancil_dat)


########### DO NOT RUN THIS CODE IF YOU WANT "UNTIDY" DATA EG. FOR THE BREAKPOINT ANALYSIS ##################################################################################

# In certain cases, visual inspection shows you need to cut even more

#Modifying the start time:

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL01_R1"), ancil_dat$time_start +100 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL02_A1"), ancil_dat$time_start +30, ancil_dat$time_start)

#AL02_A1 <- N2O_dat[ which(N2O_dat$ID=='AL02_A1'), ]
#plot(AL02_A1$Time, AL02_A1$N2O_ppm)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL02_C1"), ancil_dat$time_start +45 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL02_R5"), ancil_dat$time_start +50 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL03_B1"), ancil_dat$time_start +60 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL04_A1"), ancil_dat$time_start +110 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL04_D1"), ancil_dat$time_start +70 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL05_A1"), ancil_dat$time_start +110 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL05_A2"), ancil_dat$time_start +92 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL06_A1"), ancil_dat$time_start +70 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL06_F1"), ancil_dat$time_start +125 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "AL07_R1"), ancil_dat$time_start +10 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "BR01_A1"), ancil_dat$time_start +30 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "BR01_R3"), ancil_dat$time_start +30 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "BR01_R4"), ancil_dat$time_start +50 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "BR02_A1"), ancil_dat$time_start + 20 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "BR02_R4"), ancil_dat$time_start +40 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "BR02_R5"), ancil_dat$time_start +10 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "BU01_B1"), ancil_dat$time_start +9 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "BU01_D1"), ancil_dat$time_start +35 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "BU01_R1"), ancil_dat$time_start +5 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "BU02_A1"), ancil_dat$time_start +90 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "CA01_A1"), ancil_dat$time_start +15 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "CO01_A1"), ancil_dat$time_start +90 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "CO01_C1"), ancil_dat$time_start +90 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "ME01_A1"), ancil_dat$time_start +50 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "ME01_C1"), ancil_dat$time_start +30 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "ME01_E1"), ancil_dat$time_start +30 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "RA01_R1"), ancil_dat$time_start +20 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "RA01_R5"), ancil_dat$time_start +80 , ancil_dat$time_start)

ancil_dat$time_start <- data.table::fifelse((ancil_dat$ID == "VI01_R3"), ancil_dat$time_start +20 , ancil_dat$time_start)

#############################################
#Modifying the end time:

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL01_D1"), ancil_dat$time_end - 6 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL01_E1"), ancil_dat$time_end - 50 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL01_R2"), ancil_dat$time_end - 115 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL01_R5"), ancil_dat$time_end - 20 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL01_R6"), ancil_dat$time_end - 40 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL02_B1"), ancil_dat$time_end - 60 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL02_E1"), ancil_dat$time_end - 60 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL02_E1"), ancil_dat$time_end - 58 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL02_R1"), ancil_dat$time_end - 25 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL02_R2"), ancil_dat$time_end - 60 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL02_R6"), ancil_dat$time_end - 55 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL03_C1"), ancil_dat$time_end - 20 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL03_E1"), ancil_dat$time_end - 95 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL03_E2"), ancil_dat$time_end - 40 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL03_R3"), ancil_dat$time_end - 30 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL03_R2"), ancil_dat$time_end - 52 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL03_R4"), ancil_dat$time_end - 90 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL03_R5"), ancil_dat$time_end - 36 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL04_B1"), ancil_dat$time_end - 46 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL04_C1"), ancil_dat$time_end - 40 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL04_R2"), ancil_dat$time_end - 145 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL05_A1"), ancil_dat$time_end - 40 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL05_A2"), ancil_dat$time_end - 45 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL05_B1"), ancil_dat$time_end - 40 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL05_C1"), ancil_dat$time_end - 15 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL05_D1"), ancil_dat$time_end - 50 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL05_R1"), ancil_dat$time_end - 46 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL05_R2"), ancil_dat$time_end - 33 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL05_R3"), ancil_dat$time_end - 60 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL06_A1"), ancil_dat$time_end - 30 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL06_C1"), ancil_dat$time_end - 30 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL06_E1"), ancil_dat$time_end - 34 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL06_R2"), ancil_dat$time_end - 42 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL06_R4"), ancil_dat$time_end - 160 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL06_R5"), ancil_dat$time_end - 40 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL07_A1"), ancil_dat$time_end - 122 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL07_C1"), ancil_dat$time_end - 40 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL07_E1"), ancil_dat$time_end - 42 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL07_R2"), ancil_dat$time_end - 30 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL07_R4"), ancil_dat$time_end - 41 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "AL07_R6"), ancil_dat$time_end - 15 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BR01_R3"), ancil_dat$time_end - 5 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BR02_A1"), ancil_dat$time_end - 10 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BR02_E1"), ancil_dat$time_end - 60 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BR02_R1"), ancil_dat$time_end - 170 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BR02_R2"), ancil_dat$time_end - 30 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BR02_R3"), ancil_dat$time_end - 105 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BR02_R4"), ancil_dat$time_end - 15 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BU01_A1"), ancil_dat$time_end - 52 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BU01_B1"), ancil_dat$time_end - 20 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BU01_D1"), ancil_dat$time_end - 50 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BU01_R2"), ancil_dat$time_end - 22 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BU01_R4"), ancil_dat$time_end - 100 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BU02_C1"), ancil_dat$time_end - 110 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BU02_D1"), ancil_dat$time_end - 56 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BU02_R1"), ancil_dat$time_end - 70 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "BU02_R6"), ancil_dat$time_end - 30 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA01_B1"), ancil_dat$time_end - 47 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA01_C1"), ancil_dat$time_end - 135 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA01_E1"), ancil_dat$time_end +90 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA01_R1"), ancil_dat$time_end - 32 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA01_R2"), ancil_dat$time_end - 15 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA01_R3"), ancil_dat$time_end - 102 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA01_R4"), ancil_dat$time_end - 80 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA01_R5"), ancil_dat$time_end - 92 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA02_A1"), ancil_dat$time_end - 85 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA02_B1"), ancil_dat$time_end - 102 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA02_C1"), ancil_dat$time_end - 160 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA02_E1"), ancil_dat$time_end - 100 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA02_F1"), ancil_dat$time_end - 40 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CA02_R2"), ancil_dat$time_end - 15 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CO01_E1"), ancil_dat$time_end - 38 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CO01_R3"), ancil_dat$time_end - 125 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "CO01_R5"), ancil_dat$time_end - 10 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "GR01_B1"), ancil_dat$time_end - 10 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "GR01_R1"), ancil_dat$time_end - 65 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "GR01_R2"), ancil_dat$time_end - 57 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "GR01_R3"), ancil_dat$time_end - 63 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "GR01_R4"), ancil_dat$time_end - 50 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "GR01_R6"), ancil_dat$time_end - 15 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "ME01_A1"), ancil_dat$time_end - 60 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "ME01_C1"), ancil_dat$time_end - 50 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "ME01_D1"), ancil_dat$time_end - 60 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "ME01_E1"), ancil_dat$time_end - 30 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "ME01_R1"), ancil_dat$time_end - 30 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "ME01_R3"), ancil_dat$time_end - 15 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "ME01_R5"), ancil_dat$time_end - 30 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "RA01_A1"), ancil_dat$time_end - 60 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "RO01_A1"), ancil_dat$time_end - 35 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "RA01_D1"), ancil_dat$time_end - 100 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "RO01_A1"), ancil_dat$time_end - 60 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "RO01_D1"), ancil_dat$time_end - 55 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "VI01_C1"), ancil_dat$time_end - 75 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "VI01_D1"), ancil_dat$time_end - 120 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "VI01_R3"), ancil_dat$time_end - 100 , ancil_dat$time_end)

ancil_dat$time_end <- data.table::fifelse((ancil_dat$ID == "VI01_R4"), ancil_dat$time_end - 100 , ancil_dat$time_end)


str(ancil_dat) #223 obs. 

########### More data cleaning - removing points ############
# Some data points are such crap that we need to completely exclude the entire point
#Or in some cases the data set is too short (perhaps because the N2O wasn't warmed up yet and we started mesuring)

ancil_dat <- ancil_dat %>%
  filter(!ID == "AL01_B1")%>%
  filter(!ID == "AL02_B1")%>%
  filter(!ID == "AL02_R3")%>%
  filter(!ID == "AL02_R4")%>%
  filter(!ID == "AL04_E1")%>%
  filter(!ID == "AL05_R2")%>%
  filter(!ID == "AL05_R3")%>%
  filter(!ID == "AL06_A1")%>%
  filter(!ID == "AL06_R4")%>%
  filter(!ID == "BR01_B1")%>%
  filter(!ID == "BR01_C1")%>%
  filter(!ID == "BR01_R2")%>%
  filter(!ID == "BR02_A1")%>%
  filter(!ID == "BU01_R6")%>%
  filter(!ID == "BU02_E1")%>%
  filter(!ID == "CA01_E1")%>%
  filter(!ID == "CA02_A1")%>%
  filter(!ID == "CA02_C1")%>%
  filter(!ID == "CA02_E1")%>%
  filter(!ID == "CA02_R1")%>%
  filter(!ID == "CA02_R5")%>% #too short
  filter(!ID == "CA02_R6")%>%
  filter(!ID == "CO01_C1")%>%
  filter(!ID == "CO01_F1")%>%
  filter(!ID == "GR01_A1")%>%
  filter(!ID == "GR01_C1")%>%
  filter(!ID == "GR01_D1")%>%
  filter(!ID == "GR01_E1")%>%
  filter(!ID == "GR01_R4")%>%
  filter(!ID == "JO01_R6")%>%
  filter(!ID == "JO01_R7")%>%
  filter(!ID == "RO01_R1")%>%
  filter(!ID == "RO01_R4")%>%
  filter(!ID == "RO01_R6")%>%
  filter(!ID == "VI01_B1")%>%
  filter(!ID == "VI01_C1")%>%
  filter(!ID == "VI01_R1")%>% #too short
  filter(!ID == "VI01_R2")%>%
  filter(!ID == "VI01_R3")%>%
  filter(!ID == "VI01_R4")%>%
  filter(!ID == "VI01_R5")


str(ancil_dat) #195

#Excluded for:
# "V" or other weird distribution, unclear which if there is influx or efflux, and can't determine based on the other points at the site:
#AL01_B1
#AL02_R3
#AL02_R4
#AL04_E1
#AL05_R3
#AL06_A1
#BR01_B1
#BR01_C1
#BR01_R2
#BR02_A1
#BU01_D1
#BU01_R6
#BU02_A1
#BU02_E1
#CA01_E1
#CA02_E1
#CA02_R1
#CA02_R6
#CO01_F1
#GR01_A1
#GR01_C1
#GR01_D1
#GR01_E1
#GR01_R4
#JO01_R6
#RO01_R1
#RO01_R4
#VI01_B1
#VI01_R2
#VI01_R3
#VI01_R4
#VI01_R5




###########################################################
######### Cut the N2O ppm data by start and end time ######
###########################################################

#Now, cut the concentration based on the Start and End times from the ancillary data sheet

####### OPTION 1 #########################################
### You can do this manually following Naiara's script ###




#Using AL01 2020.03.25 as an example

#RIPARIAN
AL04_R1<-LGR_March2021[Time >= "2021-03-25 14:47:00" & Time <= "2021-03-25 14:52:00"]
AL04_R2<-LGR_March2021[Time >= "2021-03-25 14:56:00" & Time <= "2021-03-25 15:01:00"]
AL04_R3<-LGR_March2021[Time >= "2021-03-25 15:27:00" & Time <= "2021-03-25 15:32:00"]
AL04_R5<-LGR_March2021[Time >= "2021-03-25 15:56:00" & Time <= "2021-03-25 16:01:00"]
AL04_R6<-LGR_March2021[Time >= "2021-03-25 16:18:00" & Time <= "2021-03-25 16:23:00"]

#AQUATIC
AL04_A1<-LGR_March2021[Time >= "2021-03-25 14:40:00" & Time <= "2021-03-25 14:45:00"]
AL04_B1<-LGR_March2021[Time >= "2021-03-25 15:03:00" & Time <= "2021-03-25 15:08:00"]
AL04_C1<-LGR_March2021[Time >= "2021-03-25 15:21:00" & Time <= "2021-03-25 15:26:00"]
AL04_D1<-LGR_March2021[Time >= "2021-03-25 15:50:00" & Time <= "2021-03-25 15:55:00"]
AL04_E1<-LGR_March2021[Time >= "2021-03-25 16:12:00" & Time <= "2021-03-25 16:17:00"]


#create an ID column for each measure
AL04_R1$ID <- rep("AL04_R1",nrow(AL04_R1)) 
AL04_R2$ID <- rep("AL04_R2",nrow(AL04_R2))
AL04_R3$ID <- rep("AL04_R3",nrow(AL04_R3))
AL04_R5$ID <- rep("AL04_R5",nrow(AL04_R5))
AL04_R6$ID <- rep("AL04_R6",nrow(AL04_R6))

AL04_A1$ID <- rep("AL04_A1",nrow(AL04_A1)) 
AL04_B1$ID <- rep("AL04_B1",nrow(AL04_B1)) 
AL04_C1$ID <- rep("AL04_C1",nrow(AL04_C1)) 
AL04_D1$ID <- rep("AL04_D1",nrow(AL04_D1)) 
AL04_E1$ID <- rep("AL04_E1",nrow(AL04_E1)) 

#plot
par(mfrow=c(2,5), mai = c(.1, .3, .5, .1))
plot(N2O_ppm~ Time, data=AL04_R1)
plot(N2O_ppm~ Time, data=AL04_R2)
plot(N2O_ppm~ Time, data=AL04_R3)
plot(N2O_ppm~ Time, data=AL04_R5)
plot(N2O_ppm~ Time, data=AL04_R6)
plot(N2O_ppm~ Time, data=AL04_A1)
plot(N2O_ppm~ Time, data=AL04_B1)
plot(N2O_ppm~ Time, data=AL04_C1)
plot(N2O_ppm~ Time, data=AL04_D1)
plot(N2O_ppm~ Time, data=AL04_E1)

 #don't run this if running OPTION 2 below ##
####### OPTION 2 #############
### You can automate this process using a loop
##############################

# It would be faster to automate these steps 
# Write a loop to clip the [N2O] by start time and end time


ID <- ancil_dat$ID
startT<-ancil_dat$time_start #start times
endT<-ancil_dat$time_end  # end times 

for(i in 1:length(startT)){
  st<-startT[i]
  se<-endT[i]
  id<-ID[i]
  data<-LGR_March2021[LGR_March2021$Time >= st & LGR_March2021$Time <= se,]
  data$ID<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
}

LGR_dat<-get(paste("data",length(startT),sep="_"))
LGR_dat
str(LGR_dat) # now 69952 obs. of  4 #51429 obs. #48965 obs

####### DATA CLEANING ##################### DONE ##############

#it seems that we WERE missing some points at some sites.... e.g. BR01
#Investigate which ID's are in the ancillary data but not the N2O data and correct
IDstring <-unique(LGR_dat$ID) 
IDstring <- as.data.frame(IDstring) #218 unique IDs
names(IDstring)[names(IDstring) == "IDstring"] <- "ID"
#now compare this to the ancil data and see what we are missing
ancildatID <- ancil_dat$ID
ancildatID <- as.data.frame(ancildatID)
names(ancildatID)[names(ancildatID) == "ancildatID"] <- "ID"

missing <- dplyr::setdiff( ancildatID, IDstring)

print(missing) #    

# the time was input incorrectly for 1-4 so I went back into the raw data files to correct them 
# but JO01_R6 only 2 minutes long, and the first one at the site, so maybe we did not capture the flux... 
#       ID
#1 RA01_E1, #2 AL02_F1, #3 GR01_R5, #4 BU01_R6, #5 JO01_R6

#Re-checked, now  ID: BR01_R1 JO01_R6. Which are the first measurement of the day, therefore maybe we didn't capture the flux?
#Indeed the LGR started measuring at 9:12 on March 18, BU01_R6 started at 9:05
#JO01_R6 start time is 9:42 on March 23, but started measuring at 9:53


##############################################################################
###### Calculate the GAS FLUX RATE ###########################################
##############################################################################

## Now convert the data to ug-N/L, get the slope of each relationship of gas concentration over time, and correct by area/volume of chamber ###

# Basically, once you convert the ppm to ug-N/L, you can input into the gasflux package if you set the initial time of each measurement to 0

#In order to set the initial time of each measurement to 0: 
#start by adding a column for epoch time (expressed as seconds since Jan 1, 1970)
LGR_dat$epoch_time <- as.integer(as.POSIXct(LGR_dat$Time), tz="Europe/Paris")

str(LGR_dat) #69952 obs. of  5 vars.

#there are duplicated time rows in the data, delete them
dups <- LGR_dat[duplicated(epoch_time)]
str(dups)  # 331 obs. 

LGR_dat <- LGR_dat %>% 
   # Base the removal on the "epoch_time" column
  distinct(epoch_time, .keep_all = TRUE)

str(LGR_dat) #69490 obs, therefore 462 obs. were removed
#48634-48965 therefore 331 removed


#then set  the initial time of each measure to 0h 
#use Naiara's function to rest the min time to each time

rescale <- function(x) (x-min(x))

#apply this function to all epoch_time (seconds) of each measure, 
#and divide by 36000 (hours)
LGR_dat <- setDT(LGR_dat)[,c("flux_time"):=.(rescale(epoch_time/3600)),by=.(ID)]


#Convert LGR_dat concentration from ppm to ug-N/L using the ideal gas law (PV=nRT) for input to gasfluxes package. Note that ppm = uL/L

# ug-N/L = ((LGR_dat concentration in ppm  * molecular mass of nitrogen *1 atm )) / (0.08206	L·atm/K·mol * temperature in K )

LGR_dat$N2O_ug_L <- ((LGR_dat$N2O_ppm  * 28.0134 *1 )) / (0.082*(LGR_dat$AmbT_C + 273.15))

#Use 28 for molar mass bc 2*14.0067
#Use pressure values from LGR if they exist?

#Now turn to the ancillary date to prepare it to merge with the N2O data

#Subset just the useful columns: Date, ID, drying_regime, soil_temp, VWC, Picarro_LGR, total_volume_L, chamber_area_m2, flowing_state, habitat_type, pool_riffle_depth_cm,  
str(ancil_dat)
ancil_dat <- ancil_dat %>% select("date","Site" , "ID", "drying_regime", "soil_temp", "VWC", "Picarro_LGR", "total_volume_L", "chamber_area_m2", "flow_state", "habitat_type", "pool_riffle_depth_cm")


#Merge ancil_dat with N2O
N2O_dat <- merge(ancil_dat, LGR_dat, by="ID")

str(N2O_dat) # 69490 obs.

#some values for N2O are < 0 , delete them
N2O_dat <-N2O_dat[N2O_dat$N2O_ug_L >= 0,]

str(N2O_dat) #69483 obs., therefore 7 obs. removed

#Check the units
#V = L
#A = m2
# flux time = h
# concentration of N2O = mg/L

#[f0] = mg/m^2/h

mean(N2O_dat$total_volume_L) # 4.5... L
mean(N2O_dat$chamber_area_m2) # 0.045... m2
mean(N2O_dat$N2O_ug_L) # 0.2 ug/L
mean(N2O_dat$N2O_ppm) #0.3480787
mean(N2O_dat$flux_time) # 0.05h = 3 minutes

#If getting an error "Error: flux_time not sorted in flux ID 2021-03-24_AL01_A1." need to investigate
#flux time needs to be ordered within each ID
N2O_dat_order <- data.table(N2O_dat, key = c("ID", "flux_time"))

#Run the package to calculate the gas flux rate
N2O.results <- gasfluxes(N2O_dat_order, .id = "ID", .V = "total_volume_L", .A = "chamber_area_m2",.times = "flux_time", .C = "N2O_ug_L",method = c("linear"), plot = F) #can turn plot to FALSE if the number of plots was getting out of hand
N2O.results
str(N2O.results) #221 obs. --should be 223-- because 1 BR01_R1 and JO01_R6 are missing



#fluxes1 <- setNames(fluxes1, c("ID", "CO2_mg_m2_h"))

#CO2_CH4_fluxes <- CO2_CH4_fluxes  %>%
  separate(ID, c("site", "point"), "_")

#CO2_CH4_fluxes$ID<-paste(CO2_CH4_fluxes$site,CO2_CH4_fluxes$point,sep="_")

# attach the ancillary data to it





# attach the ancillary data to it
ancil_dat_sub<- subset(ancil_dat, select = c("ID", "date", "Site", "drying_regime", "soil_temp", "VWC", "flow_state", "habitat_type", "pool_riffle_depth_cm"))

N2O_fluxes <- merge (N2O.results, ancil_dat_sub, by= "ID")

write.csv (N2O_fluxes, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/N2O_flux.results.csv")

dev.off()

boxplot(linear.f0 ~ habitat_type, data= subset(N2O_fluxes, flowing_state="flowing"))

boxplot(linear.f0 ~ drying_regime, data= subset(N2O_fluxes, flowing_state="flowing"))

boxplot(linear.f0 ~ Site, data=N2O_fluxes)


