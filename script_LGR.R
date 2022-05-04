##########################################################################
##### Script for importing and processing N2O data from LGR analyzer #####
##########################################################################

#Notes

# You need to calculate the time difference between the start/end times and the LGR time for each campaign-
# The atmospheric concentration of N2O (2018) was 331.1 parts per billion, which is 0.3311 ppm. Therefore the LGR output files are in ppm. 

# Redownload ancillary dat files because I made some changes

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
library(scales)

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

#Combine the campaigns together vertically
LGR_2021 <- rbind(LGR_MAR2021,LGR_JUN2021, LGR_SEP2021, LGR_NOV2021)
str(LGR_2021) #414745 obs. of  36 variables

#Subset just the useful columns (Time, [N2O]d_ppm, AmbT_C)
LGR_2021<- subset(LGR_2021, select = c( "Time", "[N2O]d_ppm", "AmbT_C"))
str(LGR_2021) #414745 obs. of  3 variables

#Rename the N2O (dry) column to get rid of the square brackets because they can cause some issues later
names(LGR_2021)[names(LGR_2021) == "[N2O]d_ppm"] <- "N2O_ppm"

# change the time format to "as.POSIXct" time - R's time format
options(digits.secs=3) # this keeps the 3 decimal second fractions
LGR_2021$Time <- as.POSIXct(LGR_2021$Time, format = "%d/%m/%Y %H:%M:%OS", tz="Europe/Paris")

str(LGR_2021) #414745 obs. of  3 variables

write.csv(LGR_2021, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/LGR_data_processed2021.csv")


################################################################
# Import the ancillary data files with the start and end times #
################################################################

#import the ancillary data files for the aquatic and riparian measurements


ancil_dat_aquatic <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary data/GHG_data_entry_2021 - Aquatic_2022-04-25.csv", header=T)
str(ancil_dat_aquatic) #1069 obs. of  31 variables

ancil_dat_riparian <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary data/GHG_data_entry_2021 - Riparian_2022-04-22.csv", header=T)
str(ancil_dat_riparian) #1023 obs. of  25 variables


#add an ID column which combines site, point, and campaign

#Could consider adding date or campaign to the ID column, but instead maybe subset by campaign, and run the loop separately for each campaign

ancil_dat_aquatic$ID <- paste(ancil_dat_aquatic$siteID_new,ancil_dat_aquatic$point, ancil_dat_aquatic$campaign, sep="_")

ancil_dat_riparian$ID <- paste(ancil_dat_riparian$siteID_new,ancil_dat_riparian$point, ancil_dat_riparian$campaign, sep="_" )

#In order to merge the riparian and aquatic ancillary data, need to differentiate between aquatic and riparian collar heights, so rename aquatic collar heights to "sed_collarheight1" etc. 

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
ancil_dat$sed_collar_height[is.nan(ancil_dat$sed_collar_height)]<-NA

#Take the pool_width1 column (width2 is the length I think, so discard), divide by 100 to get metres and add them to the stream width column
ancil_dat$stream_width_m_2 <- ancil_dat$pool_riffle_width1  /100
ancil_dat <- ancil_dat %>% mutate(stream_width = coalesce(stream_width_m, stream_width_m_2)) 

#Now rename the old stream width column and update the new one to stream_width_m
names(ancil_dat)[names(ancil_dat) == "stream_width_m"] <- "stream_width_m_old"
names(ancil_dat)[names(ancil_dat) == "stream_width"] <- "stream_width_m"


#Subset just the useful columns: Date, ID, drying_regime, soil_temp, VWC, Picarro_LGR, total_volume_L, chamber_area_m2, flowing_state, habitat_type, pool_riffle_depth_cm, time_start, time_end 

ancil_dat <- ancil_dat %>% select( "ID_unique", "campaign", "date", "siteID_new", "ID", "time_start", "time_end", "drying_regime", "soil_temp", "VWC", "Picarro_LGR", "total_volume_L", "chamber_area_m2", "flow_state", "habitat_type", "stream_width_m", "pool_riffle_depth_cm", "sed_temp", "sed_VWC")

#rename Site column
names(ancil_dat)[names(ancil_dat) == "siteID_new"] <- "Site"

str(ancil_dat) # 2092 obs


###################################################

####### To the ancillary data we now need to add the temperature and pressure data in order to be able to properly calculate the GHG flux ############

air.temp1 <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/ibutton_air_temp.csv") #For C1 and C2 #ignore warning, column headers are OK
colnames(air.temp1) <- c("ID_unique", "Air_temp")

air.temp2 <-fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/HOBO_air_temp.csv") #for other campaigns
colnames(air.temp2) <- c("ID_unique", "Air_temp")

# Now rbind the two temperature files together vertically 
air.temp <- rbind (air.temp1, air.temp2)
str(air.temp) #2092 obs.

#Save as a csv
write.csv(air.temp, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/air_temp_full.csv")

#Now merge air.temp with ancil_dat horizontally by ID_unique
str(ancil_dat) #2092 obs. of  19 variables 
str(air.temp) #2092 obs. of  2 variables 

#check if they match
air.temp$ID_unique[!(air.temp$ID_unique %in% ancil_dat$ID_unique)]

#put all data frames into list
df_list <- list(air.temp, ancil_dat)     
  
#merge all data frames together
ancil_dat <-  df_list %>% reduce(full_join, by='ID_unique')

str(ancil_dat) #2092 obs. of  20 variables


##################### Now add the air pressure data from Naiara / Metociel by site #############

#load in the file

pressures <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/Pressure data/Air_pressures_calculated_TS.csv")

#Select the relevant columns: campaign, Site, pressure_calc_atm

pressures <- pressures[, c("Site", "campaign", "pressure_calc_atm")]

str(pressures) #140 obs. of  3 variables
str(ancil_dat) #2092 obs. of  20 variables


ancil_dat <- merge(ancil_dat, pressures, by=c("Site","campaign"))

str(ancil_dat) #2092 obs. of  21 variables

#######################################################
### Subset LGR data ###################################

#Subset the ancillary data for only LGR data
ancil_dat<-ancil_dat[ancil_dat$Picarro_LGR=="LGR",]

str(ancil_dat) #713 obs


###################################################
#### Ancillary Data Cleaning: Long intervals #####
##################################################

#data cleaning : check if there are any super long or super short intervals (negative) (potentially typos) 

#ancil_dat$int <- difftime(ancil_dat$time_end, ancil_dat$time_start, unit = "mins")
#ancil_dat <- ancil_dat %>% mutate(long_int=ifelse(int>=6,T,F))

#manual edit in the google doc file, and re download

# Make an if else statement modifying the end time to 5 minutes after the start time if the interval is greater than 5 minutes
#ancil_dat$time_end <-data.table::fifelse(ancil_dat$long_int=="TRUE", ancil_dat$time_start+300, ancil_dat$time_end)

#The first few, and last few seconds are often wonky (remove)

ancil_dat$time_start <- ancil_dat$time_start +30
ancil_dat$time_end <- ancil_dat$time_end -30

head(ancil_dat)

################################### Timing cleaning ########################
## For each campaign take an example site and make sure the start and end times match the peak

all <- ggplot(data=LGR_2021,aes(Time, N2O_ppm))+ geom_point() #+ scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")
all #try BR02 here

#Campaign 1 LGR time is on average 6.1166 minutes ahead of the recorded start end time

MAR252021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-03-25 11:00" & LGR_2021$Time>"2021-03-25 9:00"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")
MAR252021 #AL07 #On average, the LGR time is 6 minutes ahead of the recorded start/end time. 


MAR2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-03-23 14:30" & LGR_2021$Time>"2021-03-23 13:55"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M") + ylim(0.334, 0.37)
MAR2021 #CO01 #AL07 #On average, the LGR time is 6 minutes ahead of the recorded start/end time. 


MAR2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-03-18 12:30" & LGR_2021$Time>"2021-03-18 11:14"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")
MAR2021 #BR02 # On average, the LGR time is 6.35 minutes ahead of the recorded start/end time. 

MAR2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-03-17 12:00" & LGR_2021$Time>"2021-03-17 9:30"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("5 min"), date_labels = "%I:%M")
MAR2021 #GR01


#Campaign 3: On average the LGR time is 8.93888 minutes ahead

#Campaign 3 (June 21, 2021 - July 1, 2021) 

JUN2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-06-28 15:50" & LGR_2021$Time>"2021-06-28 14:40"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")  + ylim(0.335, 0.42)
JUN2021 #BU02  # On average the LGR time is 8.85 minutes ahead of the recorded start and end times

JUN2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-06-25 15:00" & LGR_2021$Time>"2021-06-25 14:10"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M") + ylim(0.335, 0.42)
JUN2021 #BR01 # On average the times are 9.16666 minutes ahead


JUN2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-06-25 11:45" & LGR_2021$Time>"2021-06-25 10:30"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M") + ylim(0.345, 0.43)
JUN2021 #BR02 #On average the actual times are 8.8 minutes ahead

JUN2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-06-22 16:00" & LGR_2021$Time>"2021-06-22 14:20"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("5 min"), date_labels = "%I:%M") 
JUN2021


#Campaign 5 : On average the LGR is 11.1943 minutes ahead. 

SEP152021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-09-15 11:30" & LGR_2021$Time>"2021-09-15 10:30"),],aes(Time, N2O_ppm))+ geom_point() + ylim(0.3, 0.45) + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M") 
SEP152021 # AL04 # On average the actual times are 11.333 minutes ahead


SEP152021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-09-15 14:10" & LGR_2021$Time>"2021-09-15 13:30"),],aes(Time, N2O_ppm))+ geom_point() + ylim(0.3, 0.45) + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M") 
SEP152021 #AL05 # On average the actual times are 11.25 minutes ahead


SEP162021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-09-16 12:45" & LGR_2021$Time>"2021-09-16 12:00"),],aes(Time, N2O_ppm))+ geom_point() + ylim(0.3, 0.45) + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M") 
SEP162021 #CA01 # On average the actual times are 11 minutes ahead



#Campaign 7: is on average 13.4 minutes ahead of the recorded times

NOV232021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-11-23 11:30" & LGR_2021$Time>"2021-11-23 9:00"),],aes(Time, N2O_ppm)) + geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")  
#+ theme(axis.text.x = element_text(angle = 30))
NOV232021 #CA02 # On average the actual times are 13.333 minutes ahead

NOV252021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-11-25 14:00" & LGR_2021$Time>"2021-11-25 12:30"),],aes(Time, N2O_ppm)) + geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")  + ylim(0.339, 0.365)
NOV252021  #AL02 #on average 13.5 minutes ahead

DEC32021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-12-03 15:50" & LGR_2021$Time>"2021-12-03 13:00"),],aes(Time, N2O_ppm)) + geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")  + ylim(0.325, 0.390)
DEC22021  #BU02 # on average 13.5 minutes ahead

########################################################################
########## Adjust start and end times based on LGR time drift #########

#Now, based on the above information, adjust the start and end times in ancil_dat by campagin

#Campaign 1 : 6.1166 minutes or 366.996 seconds
#Campaign 3 : 8.93888 minutes or 536.333 s
#Campaign 5 : 11.1943 minutes or 671.658 s
#Campaign 7: 13.4 minutes or 804 s



ancil_dat <- ancil_dat %>%
mutate(time_start = if_else(campaign == 1, as.POSIXct(time_start) + 366.996, time_start)) %>%
  mutate(time_end = if_else(campaign == 1, as.POSIXct(time_end) + 366.996, time_end))  %>%
    mutate(time_start = if_else(campaign == 3, as.POSIXct(time_start) + 536.333, time_start)) %>%
      mutate(time_end = if_else(campaign == 3, as.POSIXct(time_end) + 536.333, time_end)) %>%
       mutate(time_start = if_else(campaign == 5, as.POSIXct(time_start) + 671.658, time_start)) %>%
        mutate(time_end = if_else(campaign == 5, as.POSIXct(time_end) + 671.658, time_end)) %>%
         mutate(time_start = if_else(campaign == 7, as.POSIXct(time_start) + 804, time_start)) %>%
          mutate(time_end = if_else(campaign == 7, as.POSIXct(time_end) + 804, time_end))


#found a missing time fo RA01 F2 09/15 FIX

#Save the new start and end times to use to cut the temperature loggers

# Add a column for the start and end time in Epoch Time format for cleaning? Or find this info somewhere


write.csv(ancil_dat, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/LGR_ancil_dat.csv")

###########################################################
######### Cut the N2O ppm data by start and end time ######
###########################################################

#Now, cut the concentration based on the Start and End times from the ancillary data sheet

#Because you have replicate measurements from the same sites over time, you need to use the "ID_unique"

## You can automate this process using a loop

ID <- ancil_dat$ID_unique
startT<-ancil_dat$time_start #start times
endT<-ancil_dat$time_end  # end times 

for(i in 1:length(startT)){
  st<-startT[i]
  se<-endT[i]
  id<-ID[i]
  data<-LGR_2021[LGR_2021$Time >= st & LGR_2021$Time <= se,]
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
str(LGR_dat) # 172205 obs. of  4 variables now 168587 obs. of  4 variables

####### DATA CLEANING ##################### DONE ##############

#it seems that we WERE missing some points at some sites.... e.g. BR01
#Investigate which ID's are in the ancillary data but not the N2O data and correct
#IDstring <-unique(LGR_dat$ID) 
#IDstring <- as.data.frame(IDstring) #218 unique IDs
#names(IDstring)[names(IDstring) == "IDstring"] <- "ID"
#now compare this to the ancil data and see what we are missing
#ancildatID <- ancil_dat$ID
#ancildatID <- as.data.frame(ancildatID)
#names(ancildatID)[names(ancildatID) == "ancildatID"] <- "ID"

#missing <- dplyr::setdiff( ancildatID, IDstring)

#print(missing) #    

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

str(LGR_dat) #172205 obs. of  5 variables

#there are duplicated time rows in the data, delete them
dups <- LGR_dat[duplicated(epoch_time)]
str(dups)  # 3299 obs. of  5 variables

LGR_dat <- LGR_dat %>% 
   # Base the removal on the "epoch_time" column
  distinct(epoch_time, .keep_all = TRUE)

str(LGR_dat) #168906 obs. of  5 variables, therefore 462 obs. were removed
#172205-168906 therefore 3299 removed (or 1.9%, so should hopefully not pose a problem)


#then set  the initial time of each measure to 0h 
#use Naiara's function to rest the min time to each time

rescale <- function(x) (x-min(x))

#apply this function to all epoch_time (seconds) of each measure, 
#and divide by 36000 (hours)
LGR_dat <- setDT(LGR_dat)[,c("flux_time"):=.(rescale(epoch_time/3600)),by=.(ID)]


###############################################################################
##### Use DPSEG to select the linear portion of the flux ######################

#nest the data by ID_unique
LGR_dat_nested <- LGR_dat %>% 
  group_by(ID) %>% 
  nest()

# EXPLANATION OF DPSEG FUNCTION PARAMETERS from Naiara:
# I have specified a minimum segment length because if I do it, the weird seconds (<1min) at the end or start will remain
# jumps = FALSE --> continuous measure, no jumps between segments
# P = indicator of goodness of the fit; higher P allows longer segments
# function to estimate p based on the data (some plots are more disperse than others)
# I am using EPOCH_TIME to have unique numerical values for each time

# functions to obtain the segments the N2O flux fits

N2O_seg <- function(x) { (dpseg(x=x$epoch_time,
                                y=x$N2O_ppm,  #Naiara used the value in ug/L here, not sure what is the advantage?
                                jumps = FALSE, 
                                P = estimateP(x=x$epoch_time, y=x$N2O_ppm),
))$segments }

# apply functions to nested data
# store the results in new vectors called N2O_segments

LGR_dat_nested_segs <- LGR_dat_nested %>%  
  mutate(N2O_segments  = map(data, N2O_seg)) 

####################################
# Now we want to plot those segments

#function to select values for plotting N2O segments (X and Y)

N2O_predX <- function(x) { predict(dpseg(x=x$epoch_time,
                                         y=x$N2O_ppm,
                                         jumps = FALSE, 
                                         P = estimateP(x=x$epoch_time, y=x$N2O_ppm),
))$x }

N2O_predY <- function(x) { predict(dpseg(x=x$epoch_time,
                                         y=x$N2O_ppm,
                                         jumps = FALSE, 
                                         P = estimateP(x=x$epoch_time, y=x$N2O_ppm),
))$y }


# apply functions to nested data

LGR_dat_nested_plots<- LGR_dat_nested %>%  
  mutate(N2O_PredX  = map(data, N2O_predX)) %>% 
  mutate(N2O_PredY = map(data, N2O_predY))

LGR_dat_nested_plots

# join the different data vectors in the same tibble
# there is a WARNING MESSAGE but doesn?t matter

LGR_dat_nested_plots <- LGR_dat_nested_plots %>% unnest() %>% group_by(ID) %>% nest()
LGR_dat_nested_plots


# create N2O concentration~time plot 
# adding the predicted segments (geom_line) 

# EXPLANATION
#when using map2() instead of map(), you specify two elements, and then the function.
# here: data and ID
# inside the function, you refer to the first element (data) with .x
# and to the second (ID) with .y

LGR_dat_plots <- 
  LGR_dat_nested_plots %>% 
  mutate(N2O_plot = map2(data, ID, ~ ggplot(data = .x, aes(x =epoch_time, y = N2O_ppm)) +
                           geom_point (size=1.5) + ggtitle(paste0(unique(.y),"_N2O")) +
                           geom_line(aes(x= N2O_PredX , y= N2O_PredY), size=1.5, linetype=1, col="red"))) 

#Now export N2O plots in .png

#file_names_N2O <- paste0(LGR_dat_plots$ID,"_N2O", ".png")
#setwd("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Flux_figures/N2O_raw/")
#walk2(file_names_N2O, LGR_dat_plots$N2O_plot, ggsave) #Note that this takes a while because there are 700+ files so don't run every time (# in front)


#From these plots, check if there are any major mistakes, if there is the right number. Maybe do QAQC for a random selection of a few sites. Then create a new csv file with new start and end time based on manual oberservations. 


#ancildat has 713, and we only have 704 images output--because of NAs for start/end times? (3 NAs)


######### Re clipping based on new start and end times ####################

#Read in the corrected ancil_dat

ancil_dat <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/LGR_ancil_dat.csv")
str(ancil_dat) #713 obs. of  22 variables:

ancil_dat$time_start <- as.POSIXct(ancil_dat$time_start, format = "%Y-%m-%d %H:%M:%OS", tz="Europe/Paris")
ancil_dat$time_end <- as.POSIXct(ancil_dat$time_end, format = "%Y-%m-%d %H:%M:%OS", tz="Europe/Paris")

#Load new start and end times 
newtimes <- read.csv ('C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/N2O_new_flux_times.csv')
#convert the epoch time to regular time
newtimes$new_start <- as.POSIXct(newtimes$new_start, origin="1970-01-01")
newtimes$new_end <- as.POSIXct(newtimes$new_end, origin="1970-01-01")

#save as a new file and read in (to get rid of the epoch times)
write.csv(newtimes, 'C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/N2O_new_flux_times_formatted.csv')

newtimes <-read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/N2O_new_flux_times_formatted.csv")
newtimes$new_start <-  as.POSIXct(newtimes$new_start, format = "%Y-%m-%d %H:%M:%OS", tz="Europe/Paris")
newtimes$new_end <- as.POSIXct(newtimes$new_end, format = "%Y-%m-%d %H:%M:%OS", tz="Europe/Paris")


#Select just the new start and end times and merge with ancil dat

#check if they match or if any are missing
ancil_dat$ID_unique[!(ancil_dat$ID_unique %in% newtimes$ID_unique)] #in fact 32 unique IDs are missing from unique IDs newtimes. Sometimes this was because the LGR wasnt working (16-17/09), forgot to do the LGR etc. so they are legitimate missing data. 

# # [1] "2021-09-17_AL01_F1_LGR" "2021-09-17_AL01_F2_LGR" "2021-09-17_AL01_F3_LGR"
# [4] "2021-09-17_AL01_R1_LGR" "2021-09-17_AL01_R2_LGR" "2021-09-17_AL01_R3_LGR"
# [7] "2021-11-25_AL02_D1_LGR" "2021-11-26_AL03_F2_LGR" "2021-09-21_BR02_F1_LGR"
# [10] "2021-09-21_BR02_F2_LGR" "2021-09-21_BR02_F3_LGR" "2021-09-21_BR02_F4_LGR"
# [13] "2021-09-21_BR02_R1_LGR" "2021-09-21_BR02_R2_LGR" "2021-09-21_BR02_R3_LGR"
# [16] "2021-06-25_BU01_F2_LGR" "2021-11-22_CA01_R1_LGR" "2021-06-21_JO01_F5_LGR"
# [19] "2021-11-25_JO01_P2_LGR" "2021-11-26_ME01_F1_LGR" "2021-11-26_ME01_F2_LGR"
# [22] "2021-06-22_RA01_R1_LGR" "2021-09-15_RA01_F2_LGR" "2021-11-22_RA01_F1_LGR"
# [25] "2021-11-22_RA01_F2_LGR" "2021-11-22_RA01_F3_LGR" "2021-11-22_RA01_F4_LGR"
# [28] "2021-11-22_RA01_F5_LGR" "2021-11-22_RA01_R1_LGR" "2021-11-22_RA01_R2_LGR"
# [31] "2021-11-22_RA01_R3_LGR" "2021-11-22_RA01_R4_LGR"

newtimes <-   subset(newtimes, select = c( "ID_unique", "new_start", "new_end" ))

#put all data frames into list
df_list <- list(newtimes, ancil_dat)

str(ancil_dat) #713
str(newtimes) #681

#merge all data frames together #with this type of merge you don't lose any data
ancil_dat <-  df_list %>% reduce(full_join, by='ID_unique')

str(ancil_dat) #713 obs. of  33 variables

#Now any NA's in the new times mean we can use the original times, so fill the NA's with the original times

ancil_dat$new_start <- ifelse(is.na(ancil_dat$new_start), ancil_dat$time_start, ancil_dat$new_start) #changes all to epoch time
ancil_dat$new_start <- as.POSIXct(ancil_dat$new_start, origin="1970-01-01")

ancil_dat$new_end <- ifelse(is.na(ancil_dat$new_end), ancil_dat$time_end, ancil_dat$new_end) #changes all to epoch time
ancil_dat$new_end <- as.POSIXct(ancil_dat$new_end, origin="1970-01-01")


######################################################




ID <- ancil_dat$ID_unique
startT<-ancil_dat$time_start #start times
endT<-ancil_dat$time_end  # end times 

for(i in 1:length(startT)){
  st<-startT[i]
  se<-endT[i]
  id<-ID[i]
  data<-LGR_2021[LGR_2021$Time >= st & LGR_2021$Time <= se,]
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
str(LGR_dat)













#############################################################################

#Use 28 for molar mass bc 2*14.0067

#Use unique pressure and temperature values for each measurement 

#Now turn to the ancillary date to prepare it to merge with the N2O data

#Subset just the useful columns: Date, ID, drying_regime, soil_temp, VWC, Picarro_LGR, total_volume_L, chamber_area_m2, flowing_state, habitat_type, pool_riffle_depth_cm,  

#merge ancil dat with temperatures and pressures with LGR dat to include unique temperature and pressure values
#subset just the temp and pressure columns and ID_unique

ancil_dat_temp_press <-   subset(ancil_dat_temp_press, select = c( "ID_unique", "Air_temp", "pressure_calc_atm", 
 "date","Site" , "ID", "drying_regime", "soil_temp", "VWC", "Picarro_LGR", "total_volume_L", "chamber_area_m2", "flow_state", "habitat_type", "pool_riffle_depth_cm"))

str(ancil_dat_temp_press)  #713 obs. of  15 variables:

#Merge ancil_dat with N2O #Need to use a unique ID e.g. "2021-12-03_BU02_R4_LGR"
#Need to rename tthe ID column in LGR_dat to "ID_unique"
names(LGR_dat)[names(LGR_dat) == "ID"] <- "ID_unique"

str(LGR_dat) #212832 obs. of  6 variables

N2O_dat <- merge(ancil_dat_temp_press, LGR_dat, by="ID_unique")

str(N2O_dat) # 166750 obs. of  19 variables #71825 obs. of  20 variables why so few now?

#some values for N2O are < 0 , delete them #Why are we getting negative N2O readings? Will it cause issues deleting them like this????
N2O_dat <-N2O_dat[N2O_dat$N2O_ug_L >= 0,]

str(N2O_dat) #163309 obs., therefore 4699 obs. removed (2.7% of observations...)

#Check the units
#V = L
#A = m2
# flux time = h
# concentration of N2O = ug/L 

#[f0] = ug/m^2/h

mean(N2O_dat$total_volume_L) # 5.00079... L
mean(N2O_dat$chamber_area_m2) # 0.04523893... m2
mean(N2O_dat$N2O_ug_L) # 0.4093119 ug/L
mean(N2O_dat$N2O_ppm) # 0.3486239 ppm
mean(N2O_dat$flux_time) # 0.05724947= 3.43 minutes


#Convert LGR_dat concentration from ppm to ug-N/L using the ideal gas law (PV=nRT) for input to gasfluxes package. Note that ppm = uL/L

# ug-N/L = ((LGR_dat concentration in ppm  * molecular mass of nitrogen *1 atm )) / (0.08206	L·atm/K·mol * temperature in K )


LGR_dat$N2O_ug_L <- ((LGR_dat$N2O_ppm  * 28.0134 *1 )) / (0.082*(LGR_dat$AmbT_C + 273.15))





#If getting an error "Error: flux_time not sorted in flux ID 2021-03-24_AL01_A1." need to investigate
#flux time needs to be ordered within each ID (use ID unique for all of the data)
N2O_dat_order <- data.table(N2O_dat, key = c("ID_unique", "flux_time"))

#Run the package to calculate the gas flux rate
N2O.results <- gasfluxes(N2O_dat_order, .id = "ID_unique", .V = "total_volume_L", .A = "chamber_area_m2",.times = "flux_time", .C = "N2O_ug_L",method = c("linear"), plot = F) #can turn plot to FALSE if the number of plots was getting out of hand

N2O.results

str(N2O.results) #681 obs. #so we are missing some...

write.csv (N2O.results, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/N2O_flux.results.csv")

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


