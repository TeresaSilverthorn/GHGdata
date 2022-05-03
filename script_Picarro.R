#################
# Load packages 
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

#########################################


#########################################
### load raw data and metadata
### put the date-time in the same format for all files
##########################################

# In terms of converting from GMT time (Picarro) to France time. GMT +1 is from January to March 28, 2021 and GMT +2 from March 28 to October 31, then again GMT +1 from October 31 to December 31, 2021

#load raw data of each campaign
# Now with the 2nd campaign, ID isn't unique, thus we need to add date to ID

## CAMPAIGN 1 - MARCH 2021 ##

Picarro_March2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data_MAR2021/Picarro_raw_data", pattern='dat', full.names=T, recursive=TRUE), fread ,header=T))
#to include sub-directories, change the recursive T
str(Picarro_March2021) # 144316 obs. of  22 variables

# create a column merging date and time
time<-as.POSIXct(paste(Picarro_March2021$DATE, Picarro_March2021$TIME), format="%Y-%m-%d %H:%M:%S")
Picarro_March2021<-cbind(Picarro_March2021,time)
str(Picarro_March2021) #144316 obs

#Since the Picarro data is in GMT time, we need to add one hour to correspond to the actual time (GMT+1 time) for campaign 1 
Picarro_March2021$time<- as.POSIXlt(Picarro_March2021$time) +3600



## CAMPAIGN 2 - MAY 2021 ##

Picarro_May2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Campaign2_raw_data_MAY2021/Picarro_raw_data/", pattern='dat', full.names=T, recursive=TRUE), fread ,header=T))
#to include sub-directories, change the recursive T
str(Picarro_May2021) #106260 obs.

# create a column merging date and time
time<-as.POSIXct(paste(Picarro_May2021$DATE, Picarro_May2021$TIME), format="%Y-%m-%d %H:%M:%S")
Picarro_May2021<-cbind(Picarro_May2021,time)
str(Picarro_May2021)

#Since the Picarro data is in GMT time, we need to add two hours to correspond to the actual time (GMT+2 time) for campaign 2
Picarro_May2021$time<- as.POSIXlt(Picarro_May2021$time) +7200

#Combine the campaigns together vertically
Picarro_2021 <- rbind(Picarro_March2021, Picarro_May2021)
str(Picarro_2021) #250576


## CAMPAIGN 3 - JUNE 2021 ##

Picarro_June2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Campaign3_raw_data_JUN2021/Picarro_raw_data/", pattern='dat', full.names=T, recursive=TRUE), fread ,header=T))
#to include sub-directories, change the recursive T
str(Picarro_June2021) # obs.

# create a column merging date and time
time<-as.POSIXct(paste(Picarro_June2021$DATE, Picarro_June2021$TIME), format="%Y-%m-%d %H:%M:%S")
Picarro_June2021<-cbind(Picarro_June2021,time)
str(Picarro_June2021)

#Since the Picarro data is in GMT time, we need to add two hours to correspond to the actual time (GMT+2 time) for campaign 2
Picarro_June2021$time<- as.POSIXlt(Picarro_June2021$time) +7200



## CAMPAIGN 4 - JULY 2021 ##

Picarro_July2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Campaign3_raw_data_JUL2021/Picarro_raw_data/", pattern='dat', full.names=T, recursive=TRUE), fread ,header=T))
#to include sub-directories, change the recursive T
str(Picarro_July2021) # obs.

# create a column merging date and time
time<-as.POSIXct(paste(Picarro_July2021$DATE, Picarro_July2021$TIME), format="%Y-%m-%d %H:%M:%S")
Picarro_July2021<-cbind(Picarro_July2021,time)
str(Picarro_July2021)

#Since the Picarro data is in GMT time, we need to add two hours to correspond to the actual time (GMT+2 time) for campaign 2
Picarro_July2021$time<- as.POSIXlt(Picarro_July2021$time) +7200


## CAMPAIGN 5 -  SEPTEMBER 2021 ##

Picarro_September2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Campaign5_raw_data_SEP2021/Picarro_raw_data/", pattern='dat', full.names=T, recursive=TRUE), fread ,header=T))
#to include sub-directories, change the recursive T
str(Picarro_September2021) # obs.

# create a column merging date and time
time<-as.POSIXct(paste(Picarro_September2021$DATE, Picarro_September2021$TIME), format="%Y-%m-%d %H:%M:%S")
Picarro_September2021<-cbind(Picarro_September2021,time)
str(Picarro_September2021)

#Since the Picarro data is in GMT time, we need to add two hours to correspond to the actual time (GMT+2 time) for campaign 2
Picarro_September2021$time<- as.POSIXlt(Picarro_September2021$time) +7200


## CAMPAIGN 6 -  OCTOBER 2021 ##

Picarro_October2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Campaign6_raw_data_OCT2021/Picarro_raw_data/", pattern='dat', full.names=T, recursive=TRUE), fread ,header=T))
#to include sub-directories, change the recursive T
str(Picarro_October2021) # obs.

# create a column merging date and time
time<-as.POSIXct(paste(Picarro_October2021$DATE, Picarro_October2021$TIME), format="%Y-%m-%d %H:%M:%S")
Picarro_October2021<-cbind(Picarro_October2021,time)
str(Picarro_October2021)

#Since the Picarro data is in GMT time, we need to add two hours to correspond to the actual time (GMT+2 time) for campaign 2
Picarro_October2021$time<- as.POSIXlt(Picarro_October2021$time) +7200

## CAMPAIGN 7 -  November 2021 ##

Picarro_November2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Campaign7_raw_data_NOV2021/Picarro_raw_data/", pattern='dat', full.names=T, recursive=TRUE), fread ,header=T))
#to include sub-directories, change the recursive T
str(Picarro_November2021) # obs.

# create a column merging date and time
time<-as.POSIXct(paste(Picarro_November2021$DATE, Picarro_November2021$TIME), format="%Y-%m-%d %H:%M:%S")
Picarro_November2021<-cbind(Picarro_November2021,time)
str(Picarro_November2021)

#Since the Picarro data is in GMT time, we need to add one hour to correspond to the actual time (GMT+1 time) for campaign 7
Picarro_November2021$time<- as.POSIXlt(Picarro_November2021$time) +3600


###############################################

### Combine the campaigns together vertically ###
Picarro_2021 <- rbind(Picarro_March2021, Picarro_July2021)
str(Picarro_2021) #













#write.csv(Picarro_March2021,"C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Picarro_raw_March2021.csv")

#write.csv(Picarro_May2021,"C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Picarro_raw_May2021.csv")

#write.csv(Picarro_2021,"C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Picarro_raw_2021.csv")


##########################################################
#load the ancillary data
##########################################################

#Import ancillary data #Update as necessary to most recent file in the Ancillary data folder in case you make any corrections to the Google Drive sheet

ancil_dat_aquatic <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary data/GHG_data_entry_2021 - Aquatic_2022-04-22.csv", header=T)
str(ancil_dat_aquatic) #1069 obs. of  31 variables

ancil_dat_riparian <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary data/GHG_data_entry_2021 - Riparian_2022-04-22.csv", header=T)
str(ancil_dat_riparian) #1023 obs. of  25 variables

#add an ID column  #Could consider adding date to the ID column, but instead maybe subset by campaign, and run the loop separately for each campaign

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

#add the date to the start and end time columns for 
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
ancil_dat$sed_collar_height[is.nan(ancil_dat$sed_collar_height)]<-NA

#Take the pool_width1 column (width2 is the length I think, so discard), divide by 100 to get metres and add them to the stream width column
ancil_dat$stream_width_m_2 <- ancil_dat$pool_riffle_width1  /100
ancil_dat <- ancil_dat %>% mutate(stream_width = coalesce(stream_width_m, stream_width_m_2)) 

#Now rename the old stream width column and update the new one to stream_width_m
names(ancil_dat)[names(ancil_dat) == "stream_width_m"] <- "stream_width_m_old"
names(ancil_dat)[names(ancil_dat) == "stream_width"] <- "stream_width_m"

#Subset the ancillary data for only picarro data
ancil_dat<-ancil_dat[ancil_dat$Picarro_LGR=="Picarro",]

str(ancil_dat) # 1372 obs.

#Subset just the useful columns: Date, ID, drying_regime, soil_temp, VWC, Picarro_LGR, total_volume_L, chamber_area_m2, flowing_state, habitat_type, pool_riffle_depth_cm, time_start, time_end 
ancil_dat <- ancil_dat %>% select("ID_unique",  "campaign", "date", "siteID_new", "ID", "time_start", "time_end", "drying_regime", "soil_temp", "VWC", "Picarro_LGR", "total_volume_L", "chamber_area_m2", "flow_state", "habitat_type", "stream_width_m", "pool_riffle_depth_cm", "sed_temp", "sed_VWC")

str(ancil_dat) #1372 obs


#Since the Picarro failed on 2021-03-23 at CO01, you can delete those rows
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_A1"),]
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_B1"),]
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_C1"),]
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_D1"),]
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_E1"),]
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_F1"),]
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_R1"),]
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_R2"),]
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_R3"),]
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_R4"),]
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_R5"),]
ancil_dat<-ancil_dat[!(ancil_dat$ID=="2021-03-23_CO01_R6"),]

#rename Site column
names(ancil_dat)[names(ancil_dat) == "siteID_new"] <- "Site"

str(ancil_dat) #1372 obs

###################################################
#### Ancillary Data Cleaning: Long intervals #####
##################################################

#data cleaning : check if there are any super long or super short intervals (potentially typos) 

#ancil_dat$int <- difftime(ancil_dat$time_end, ancil_dat$time_start, unit = "mins")
#ancil_dat <- ancil_dat %>% mutate(long_int=ifelse(int>=6,T,F))

# Make an if else statement modifying the end time to 5 minutes after the start time if the interval is greater than 5 minutes

#$time_end <-data.table::fifelse(ancil_dat$long_int=="TRUE", ancil_dat$time_start+300, ancil_dat$time_end)

#The first few, and last few seconds are often wonky (remove)
ancil_dat$time_start <- ancil_dat$time_start +30
ancil_dat$time_end <- ancil_dat$time_end -30

head(ancil_dat)


write.csv(ancil_dat, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Picarro_ancil_dat.csv")

#########################################################################
#### Clip the data by start and end times ####
###########################################################

###   CAMPAIGN 1 ######
#Subset the first campaign
ancil_dat_1 <- ancil_dat[ which(ancil_dat$campaign==1), ]

ID <- ancil_dat_1$ID
startT<-ancil_dat_1$time_start #start times
endT<-ancil_dat_1$time_end  # end times 

for(i in 1:length(startT)){
  st<-startT[i]
  se<-endT[i]
  id<-ID[i]
  data<-Picarro_2021[Picarro_2021$time >= st & Picarro_2021$time <= se,]
  data$ID<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
}

Picarro_dat_camp1<-get(paste("data",length(startT),sep="_"))
str(Picarro_dat_camp1) # 54975 obs.



###   CAMPAIGN 2 ######
#Subset the first campaign
ancil_dat_2 <- ancil_dat[ which(ancil_dat$campaign==2), ]

ID <- ancil_dat_2$ID
startT<-ancil_dat_2$time_start #start times
endT<-ancil_dat_2$time_end  # end times 

for(i in 1:length(startT)){
  st<-startT[i]
  se<-endT[i]
  id<-ID[i]
  data<-Picarro_2021[Picarro_2021$time >= st & Picarro_2021$time <= se,]
  data$ID<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
}

Picarro_dat_camp2<-get(paste("data",length(startT),sep="_"))
str(Picarro_dat_camp2) # 55279 obs.



#################################################
######### DATA CLEANING #########################
#################################################

### there are duplicated time rows in the data, delete them

# For campaign 1:
Picarro_dat_camp1 <- Picarro_dat_camp1  %>% 
  # Base the removal on the "epoch_time" column
  distinct(EPOCH_TIME, .keep_all = TRUE)

str(Picarro_dat_camp1) # 54930 obs. #45 observations removed

#For campaign 2: 
Picarro_dat_camp2 <- Picarro_dat_camp2  %>% 
  # Base the removal on the "epoch_time" column
  distinct(EPOCH_TIME, .keep_all = TRUE)

str(Picarro_dat_camp2) #55277  obs. # 2 observations removed


################################################################################
#I also may have input some times incorrectly into the ancillary data sheet, so to check this, see which  ID's are missing to see if it might be due to a data inputting error

#for campaign 1: 

#it seems that we are missing some points at some sites.... e.g. Al05 for campaign 1
#Investigate which ID's are in the ancillary data but not the Pic data and correct
IDstring <-unique(Picarro_dat_camp1$ID) 
IDstring <- as.data.frame(IDstring) #205 unique IDs (vs 218 for N2O, why?) #322 now #207 now
names(IDstring)[names(IDstring) == "IDstring"] <- "ID"
#now compare this to the ancil data and see what we are missing
ancildatID <- ancil_dat_1$ID
ancildatID <- as.data.frame(ancildatID) #431 
names(ancildatID)[names(ancildatID) == "ancildatID"] <- "ID" 
#221 obs #233 now

missing_camp1 <- dplyr::setdiff(ancildatID, IDstring)
print(missing_camp1) # 16 missing 

# AL05 -- What is going on here? This was after the unexpected shut-down where we had really weird values (flat lines, like steps)
# CO01 we had to redo, and we only did 5 when we went back
# BU02_A1 and BU02_B1 the Picarro wasn't measuring properly, not enough data points
# GR01_R4, GR01_R5 and GR01_R6 was when the Picarro battery died, so R4 doesn't have enough points--we need to remove it. And R5 and R6 we didn't redo so they don't exist

############################
#for campaign 2: 

#it seems that we are missing some points at some sites.... e.g. Al05 for campaign 1
#Investigate which ID's are in the ancillary data but not the Pic data and correct
IDstring <-unique(Picarro_dat_camp2$ID) 
IDstring <- as.data.frame(IDstring) #209 unique IDs 
names(IDstring)[names(IDstring) == "IDstring"] <- "ID"
#now compare this to the ancil data and see what we are missing
ancildatID <- ancil_dat_2$ID
ancildatID <- as.data.frame(ancildatID)
names(ancildatID)[names(ancildatID) == "ancildatID"] <- "ID" 
#210 obs.

missing_camp2 <- dplyr::setdiff(ancildatID, IDstring)
print(missing_camp2) # JO01_C1 missing, was a type--fixed # now no missing ID's!

###############################################################################

#Further, for campaign 1, 

### Based on the gasflux figures, we can see that BU02A1, BU02_B1, and GR01_R4 don't have enough data, so we will have to exclude them 
Picarro_dat_camp1 <- Picarro_dat_camp1 %>%
  filter(!ID == "BU02_A1")
str(Picarro_dat_camp1) #67732 obs (2 obs removed)

Picarro_dat_camp1 <- Picarro_dat_camp1 %>%
  filter(!ID == "BU02_B1")
str(Picarro_dat_camp1) #67678 obs. (54 obs removed)

Picarro_dat_camp1 <- Picarro_dat_camp1 %>%
  filter(!ID == "GR01_R4")
str(Picarro_dat_camp1) # 67633 obs. (45 obs removed)

# AL01_R1 has a big gap in the middle.... remove
Picarro_dat_camp1 <- Picarro_dat_camp1 %>%
  filter(!ID == "AL01_R1")
str(Picarro_dat_camp1) 

###########################
#### now that we have the data split by start/end times, we can set t0 to 0 hrs ####
##############################

#create function to rest the min time to each time
rescale <- function(x) (x-min(x))

Picarro_dat_camp1 <- setDT(Picarro_dat_camp1)[,c("flux_time"):=.(rescale(EPOCH_TIME/3600)),by=.(ID)]
str(Picarro_dat_camp1) # 54725 obs.

Picarro_dat_camp2 <- setDT(Picarro_dat_camp2)[,c("flux_time"):=.(rescale(EPOCH_TIME/3600)),by=.(ID)]
str(Picarro_dat_camp2) # 55544 obs.

## keep only the desired columns from picarro data
Picarro_dat_camp1 <- subset(Picarro_dat_camp1, select = c( "CavityTemp", "CH4_dry", "CO2_dry", "ID", "flux_time", "EPOCH_TIME"))
str(Picarro_dat_camp1) # 54725 obs.

Picarro_dat_camp2 <- subset(Picarro_dat_camp2, select = c( "CavityTemp", "CH4_dry", "CO2_dry", "ID", "flux_time", "EPOCH_TIME"))
str(Picarro_dat_camp2) #  55544 obs

CO2_CH4_dat_camp1 <- merge (Picarro_dat_camp1, ancil_dat_1 , by="ID", allow.cartesian=TRUE)
CO2_CH4_dat_camp2 <- merge (Picarro_dat_camp2, ancil_dat_2 , by="ID", allow.cartesian=TRUE)

str(CO2_CH4_dat_camp1) # 57934 
str(CO2_CH4_dat_camp2) # 55544

############################################################################
# delete clear outliers (CO2 and CH4 data that is not real)
# this will make the plots more "clean"

str(CO2_CH4_dat_camp1) #57934 obs.
str(CO2_CH4_dat_camp2) #55544 obs.

#For Campaign 1:
#CO2

outliers_CO2_1<- Boxplot(CO2_CH4_dat_camp1$CO2_dry ~ ID , data=CO2_CH4_dat_camp1)
CO2_dat_camp1<- CO2_CH4_dat_camp1 %>%
  filter(!row_number() %in% outliers_CO2_1)
str(CO2_dat_camp1) #57755 obs., therefore 179 obs removed for CO2 for Camp 1 


#CH4
outliers_CH4_1<- Boxplot(CO2_CH4_dat_camp1$CH4_dry ~ ID , data=CO2_CH4_dat_camp1)
CO2_CH4_dat_camp1<- CO2_dat_camp1 %>%
  filter(!row_number() %in% outliers_CH4_1)
str(CO2_CH4_dat_camp1) #57676 obs., therefore 258 obs removed for CH4 for Camp 1   
#warning that this method deletes the entire row for each outlying CO2 or CH4 value

#For Campaign 2:
#CO2

outliers_CO2_2<- Boxplot(CO2_CH4_dat_camp2$CO2_dry ~ ID , data=CO2_CH4_dat_camp2)
CO2_dat_camp2<- CO2_CH4_dat_camp2 %>%
  filter(!row_number() %in% outliers_CO2_2)
str(CO2_dat_camp2) #55342 obs., therefore 202 obs removed for CO2 for Camp 2

#CH4
outliers_CH4_2<- Boxplot(CO2_CH4_dat_camp2$CH4_dry ~ ID , data=CO2_CH4_dat_camp2)
CO2_CH4_dat_camp2<- CO2_dat_camp2 %>%
  filter(!row_number() %in% outliers_CH4_2)
str(CO2_CH4_dat_camp2) # 55169 obs., therefore 375 obs removed for CH4 for Camp 2   

########################################################################
#### Selecting the linear portion of the flux using DPSEG #############
#######################################################################

#nest the data by ID
CO2_CH4_dat_camp1_nested <- CO2_dat_camp1 %>% 
  group_by(ID) %>% 
  nest()

CO2_CH4_dat_camp2_nested <- CO2_dat_camp2 %>% 
  group_by(ID) %>% 
  nest()


# EXPLANATION OF DPSEG FUNCTION PARAMETERS

# I have specified a minimum segment length because if I do it, the weird seconds (<1min) at the end or start will remain
# jumps = FALSE --> continuous measure, no jumps between segments
# P = indicator of goodness of the fit; higher P allows longer segments
# function to estimate p based on the data (some plots are more disperse than others)
# I am using EPOCH_TIME to have unique numerical values for each time

# functions to obtain the segments the CO2 and CH4 fits

CO2_seg <- function(x) { (dpseg(x=x$EPOCH_TIME,
                                y=x$CO2_dry,
                                jumps = FALSE, 
                                P = estimateP(x=x$EPOCH_TIME, y=x$CO2_dry),
))$segments }

CH4_seg <- function(x) { (dpseg(x=x$EPOCH_TIME,
                                y=x$CH4_dry,
                                jumps = FALSE, 
                                P = estimateP(x=x$EPOCH_TIME, y=x$CH4_dry),
))$segments }

# apply functions to nested data
# store the results in new vectors called CO2 and CH4_segments 

#For campaign 1

dataFlux_nested_segs_camp1<- CO2_CH4_dat_camp1_nested %>%  
  mutate(CO2_segments  = map(data, CO2_seg)) %>% 
  mutate(CH4_segments = map(data, CH4_seg))

#For campaign 2

dataFlux_nested_segs_camp2<- CO2_CH4_dat_camp2_nested %>%  
  mutate(CO2_segments  = map(data, CO2_seg)) %>% 
  mutate(CH4_segments = map(data, CH4_seg))

####################################
# Now we want to plot those segments

#function to select values for plotting CO2 segments (X and Y)

CO2_predX <- function(x) { predict(dpseg(x=x$EPOCH_TIME,
                                         y=x$CO2_dry,
                                         jumps = FALSE, 
                                         P = estimateP(x=x$EPOCH_TIME, y=x$CO2_dry),
))$x }

CO2_predY <- function(x) { predict(dpseg(x=x$EPOCH_TIME,
                                         y=x$CO2_dry,
                                         jumps = FALSE, 
                                         P = estimateP(x=x$EPOCH_TIME, y=x$CO2_dry),
))$y }

CH4_predX <- function(x) { predict(dpseg(x=x$EPOCH_TIME,
                                         y=x$CH4_dry,
                                         jumps = FALSE, 
                                         P = estimateP(x=x$EPOCH_TIME, y=x$CH4_dry),
))$x }

CH4_predY <- function(x) { predict(dpseg(x=x$EPOCH_TIME,
                                         y=x$CH4_dry,
                                         jumps = FALSE, 
                                         P = estimateP(x=x$EPOCH_TIME, y=x$CH4_dry),
))$y }


# apply functions to nested data

#For campaign 1

dataFlux_nested_plots_camp1 <- CO2_CH4_dat_camp1_nested %>%  
  mutate(CO2_PredX  = map(data, CO2_predX)) %>% 
  mutate(CO2_PredY = map(data, CO2_predY))%>%
  mutate(CH4_PredX  = map(data, CH4_predX)) %>% 
  mutate(CH4_PredY = map(data, CH4_predY))

dataFlux_nested_plots_camp1

#for campaign 2 

dataFlux_nested_plots_camp2 <- CO2_CH4_dat_camp2_nested %>%  
  mutate(CO2_PredX  = map(data, CO2_predX)) %>% 
  mutate(CO2_PredY = map(data, CO2_predY))%>%
  mutate(CH4_PredX  = map(data, CH4_predX)) %>% 
  mutate(CH4_PredY = map(data, CH4_predY))

dataFlux_nested_plots_camp2


# join the different data vectors in the same tibble
# there is a WARNING MESSAGE but doesn?t matter

dataFlux_nested_plots_camp1 <- dataFlux_nested_plots_camp1 %>% unnest() %>% group_by(ID) %>% nest()
dataFlux_nested_plots_camp1

dataFlux_nested_plots_camp2 <- dataFlux_nested_plots_camp2 %>% unnest() %>% group_by(ID) %>% nest()
dataFlux_nested_plots_camp2

# create CO2 and CH4 concentration~time plot 
# adding the predicted segments (geom_line) 

# EXPLANATION
#when using map2() instead of map(), you specify two elements, and then the function.
# here: data and ID
# inside the function, you refer to the first element (data) with .x
# and to the second (ID) with .y
#######

#For campaign 1
dataFlux_plots_camp1 <- 
  dataFlux_nested_plots_camp1 %>% 
  mutate(CO2_plot = map2(data, ID, ~ ggplot(data = .x, aes(x =EPOCH_TIME, y = CO2_dry)) +
                           geom_point (size=1.5) + ggtitle(paste0(unique(.y),"_CO2")) +
                           geom_line(aes(x= CO2_PredX , y= CO2_PredY), size=1.5, linetype=1, col="red"))) %>%
  mutate(CH4_plot = map2(data, ID, ~ ggplot(data = .x, aes(x = EPOCH_TIME, y = CH4_dry)) +
                           geom_point (size=1.5) + ggtitle(paste0(unique(.y),"_CH4")) +
                           geom_line(aes(x= CH4_PredX , y= CH4_PredY), size=1.5, linetype=1, col="red")))

#For campaign 2
dataFlux_plots_camp2 <- 
  dataFlux_nested_plots_camp2 %>% 
  mutate(CO2_plot = map2(data, ID, ~ ggplot(data = .x, aes(x =EPOCH_TIME, y = CO2_dry)) +
                           geom_point (size=1.5) + ggtitle(paste0(unique(.y),"_CO2")) +
                           geom_line(aes(x= CO2_PredX , y= CO2_PredY), size=1.5, linetype=1, col="red"))) %>%
  mutate(CH4_plot = map2(data, ID, ~ ggplot(data = .x, aes(x = EPOCH_TIME, y = CH4_dry)) +
                           geom_point (size=1.5) + ggtitle(paste0(unique(.y),"_CH4")) +
                           geom_line(aes(x= CH4_PredX , y= CH4_PredY), size=1.5, linetype=1, col="red")))

#########################
#export CO2 plots in .png

file_names_CO2_camp1 <- paste0(dataFlux_plots_camp1$ID,"_CO2", ".png")
setwd("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Flux_figures/Linear_segments/Camp1/CO2")
walk2(file_names_CO2_camp1, dataFlux_plots_camp1$CO2_plot, ggsave)

file_names_CH4 <- paste0(dataFlux_plots$ID,"_CH4", ".png")
setwd("C:/Users/naina/OneDrive/Escritorio/data analysis DRYvER/FRA_March/3_LINEAR SEGMENTS/CH4")
walk2(file_names_CH4, dataFlux_plots$CH4_plot, ggsave)








########################################################
################## Make flux figures ###################
#######################################################
#This takes a few minutes so only run if you need to update figures

#ID <- unique(CO2_CH4_dat$ID)

#for (i in ID) {
#  CO2_plot = ggplot(data=subset(CO2_CH4_dat, ID==i)) + 
#    geom_point(size=1, aes(x=flux_time, y=CO2_dry)) +
#    theme_minimal() +
#    ggtitle(i) }  ### Stop here, then run the next part
  
  
#  ggsave(CO2_plot, path="C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183#)/Documents/Data/R/GHGdata/Flux_figures/2021.04.23", 
#         file=paste0("plot_", i,".pdf"), width = 7, height = 5, units = "cm")

#############################################
### flux calculations                    ####
#############################################

#put CO2 and CH4 concentration (now in ppm) in mg/L
# mg/L = ((ppm  * molecular mass *1 atm )/1000) / (0.082 * 293K )
# ug/L = (ppm  * molecular mass *1 atm ) / (0.082 * 293K )

CO2_CH4_dat$CO2_mg_L <- ((CO2_CH4_dat$CO2_dry  * 12.011 *1 )/1000) / (0.082*(CO2_CH4_dat$CavityTemp + 273.15))

CO2_CH4_dat$CH4_ug_L <- (CO2_CH4_dat$CH4_dry  * 12.011 *1 ) / (0.082*(CO2_CH4_dat$CavityTemp + 273.15))

#some values for CO2 or CH4 are < 0 , delete them

CO2_CH4_dat<-CO2_CH4_dat[!(CO2_CH4_dat$CO2_mg_L<="0")] 
CO2_CH4_dat<-CO2_CH4_dat[!(CO2_CH4_dat$CH4_ug_L<="0")] 

str(CO2_CH4_dat) #67722 obs. #63309 obs.

#write.csv(CO2_CH4_dat,"C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_CH4_dat.csv")

#write.csv(CO2_CH4_dat,"C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_CH4_dat_forbreakpoints.csv")  #to reqrite this file, run the script without the manual cleaning (just with the +/- 30 sec and bad sites removed)

#Check the units
#V = L
#A = m2
# flux time = h
# concentration of CO2 / CH4 = mg/L

#[f0] = mg/m^2/h

mean(CO2_CH4_dat$total_volume_L) # 4.5... L
mean(CO2_CH4_dat$chamber_area_m2) # 0.045... m2
median(CO2_CH4_dat$CO2_mg_L) # ~0.23 mg/L          
median(CO2_CH4_dat$CH4_ug_L) # ~1.53 ug/L         
mean(CO2_CH4_dat$flux_time) # 0.05h = 3 minutes

#Subset just the 2nd campaign data
CO2_CH4_dat_camp2 <- CO2_CH4_dat[ which(CO2_CH4_dat$campaign==2), ]

CO2.results<- gasfluxes(CO2_CH4_dat_camp2, .id = "ID", 
		.V = "total_volume_L", .A = "chamber_area_m2", 
		.times = "flux_time",.C = "CO2_mg_L", 
		methods = c("linear"), plot=T)

#turn plot to F if you don't want plots 

str(CO2.results)

CH4.results <- gasfluxes(CO2_CH4_dat_camp2, .id = "ID", 
		.V = "total_volume_L", .A = "chamber_area_m2",
		.times = "flux_time",.C = "CH4_ug_L", 
		methods = c("linear"), plot=T)

#save model outputs

write.csv (CO2.results, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2.results.csv")

write.csv (CH4.results, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CH4.results.csv")


##################
# save CO2 and CH4 fluxes in new file

fluxes1 <- CO2.results[, c(1,2)]
fluxes1 <- setNames(fluxes1, c("ID", "CO2_mg_m2_h"))

fluxes2 <- CH4.results[, c(1,2)]
fluxes2 <- setNames(fluxes2, c("ID", "CH4_ug_m2_h"))

CO2_CH4_fluxes <- merge (fluxes1, fluxes2, by= "ID")
str(CO2_CH4_fluxes)

CO2_CH4_fluxes <- CO2_CH4_fluxes  %>%
  separate(ID, c("site", "point"), "_")

CO2_CH4_fluxes$ID<-paste(CO2_CH4_fluxes$site,CO2_CH4_fluxes$point,sep="_")

# attach the ancillary data to it

ancil_dat_sub<- subset(ancil_dat, select = c("ID", "date", "drying_regime", "soil_temp", "VWC", "flow_state", "habitat_type", "pool_riffle_depth_cm"))

N2O_fluxes <- merge (N2O.results, ancil_dat_sub, by= "ID")

write.csv (CO2_CH4_fluxes, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_CH4_flux.results.csv")


dev.off()
boxplot(CO2_mg_m2_h ~ habitat_type, data= subset(CO2_CH4_fluxes, flowing_state="flowing"))

boxplot(CO2_mg_m2_h ~ drying_regime, data= subset(CO2_CH4_fluxes, flowing_state="flowing"))
        
boxplot(CH4_ug_m2_h ~ site, data=CO2_CH4_fluxes, ylim=c(-200, 200))



