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


## NEED TO RE DOWNLOAD BC I MADE SOME CHANGES

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

################################### Timing cleaning ########################
## For each campaign take an example site and make sure the start and end times match the peak

all <- ggplot(data=LGR_2021,aes(Time, N2O_ppm))+ geom_point() #+ scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")
all #try BR02 here

#Campaign 1: 

MAR2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-03-18 12:30" & LGR_2021$Time>"2021-03-18 11:00"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")
MAR2021 #using BR02 data (high peaks normally) #more or less on time

MAR2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-03-17 12:40" & LGR_2021$Time>"2021-03-17 9:00"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("5 min"), date_labels = "%I:%M") + ylim(0.334, 0.355)
MAR2021 #Also high peaks at GR01 #difficult to determine a pattern, but seems s/e time is ahead


#Campaign 3: 

JUN2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-06-25 15:00" & LGR_2021$Time>"2021-06-25 14:10"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M") + ylim(0.335, 0.42)
JUN2021 #BR01 # On average the times are 9.16666 minutes ahead


JUN2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-06-25 11:45" & LGR_2021$Time>"2021-06-25 10:30"),],aes(Time, N2O_ppm))+ geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M") + ylim(0.345, 0.43)
JUN2021 #BR02 #On average the actual times are 8.8 minutes ahead


#Campaign 5 :

SEP2021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-09-15 13:30" & LGR_2021$Time>"2021-09-25 12:30"),],aes(Time, N2O_ppm))+ geom_point() #+ scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")
SEP2021 #try BR02 here

#Campaign 7: is on average 13.4 minutes ahead of the recorded times

NOV232021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-11-23 11:30" & LGR_2021$Time>"2021-11-23 9:00"),],aes(Time, N2O_ppm)) + geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")  
#+ theme(axis.text.x = element_text(angle = 30))
NOV232021 #CA02 # On average the actual times are 13.333 minutes ahead

NOV252021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-11-25 14:00" & LGR_2021$Time>"2021-11-25 12:30"),],aes(Time, N2O_ppm)) + geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")  + ylim(0.339, 0.365)
NOV252021  #AL02 #on average 13.5 minutes ahead

DEC32021 <- ggplot(data=LGR_2021[which(LGR_2021$Time<"2021-12-03 15:50" & LGR_2021$Time>"2021-12-03 13:00"),],aes(Time, N2O_ppm)) + geom_point() + scale_x_datetime(breaks=date_breaks("2 min"), date_labels = "%I:%M")  + ylim(0.325, 0.390)
DEC22021  #BU02 # on average 13.5 minutes ahead






###########################################################
######### Cut the N2O ppm data by start and end time ######
###########################################################

#Now, cut the concentration based on the Start and End times from the ancillary data sheet

## You can automate this process using a loop

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


