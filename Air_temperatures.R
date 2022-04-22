#### Script for extracting air temperature data ####
# 
# Load the necessary packages
#load necessary packages
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(survival)
library(scales)
library(tidyr)
#
### Goal:  Extract air temperature data from the ibutton dataloggers as well as Naiara's HOBO dataloggers to get an average air temperature per GHG measurement to correct for in the gasflux calculation ###
#
#
#
# Air water and soil ibuttons can be found here: 
# C:\Users\teresa.silverthorn\Dropbox\My PC (lyp5183)\Documents\Data\Ancillary Data\iButton_data_Dryver\Air_water_soil_temperature_ibuttons #However, not that most of them stopped recording after June 2021
#
#
# Air ibuttons we have for 17 sites, missing #30 BR01 and #53 CA01
#
# Water ibuttons we have 14 sites, are missing #7 CA01, #12 BR01, #22 AL01,  #24 JO01, #50 RO01, and AL04 (no ibutton noted, could it be the unlabeled 91)
#
#
# Soil ibuttons we only have 7 ibuttons. We are missing:
# 2.  BR02
# 3. BU01 apparently I took the data off the ibutton, but can't find the file
# 6.  CA02
# 13. BU02
# 16. VI01
# 18. GR01
# 19. AL05
# 25. AL03
# 28. AL06
#3 2. AL07
# 42. BR01
# 55. RA01
# 60. CA01
#
# Note when missing data, the datalogger from the next closest site was used. NA in the iButton ID column denotes missing datalogger
#
# ibutton information (file name, type, site, ibutton ID) can be found here: 

ibutton_dat <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/iButton_data_Dryver/Air_water_soil_temperature_ibuttons/iButton_master_sheet.csv", header=T)



# Naiara's HOBO logger data master sheet can be found here:

HOBO_dat <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/Naiara_light_logger_master_list.csv", header=T)

#Note that the HOBO data is in different folder by month, way want to merge all the data? per site?





###############################################################################
### Merge iButton data information and ancillary data ############################
###############################################################################


#Read in the ancillary data file which has the start and end times for GHG measurements

ancil_dat_LGR <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/LGR_ancil_dat.csv") # for LGR #Note this is the data cleaned wit the time shifts and +- 30s off start end times

#For Picarro

ancil_dat_Pic <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Picarro_ancil_dat.csv") #likewise this data has +-30s off start and end times

#Merge the LGR and Picarro ancillary data together vertically

ancil_dat<-rbind(ancil_dat_LGR, ancil_dat_Pic)

#Subset, site, date, and start and end time 

ancil_dat <- ancil_dat %>% select("ID_unique", "campaign", "date", "Site",  "time_start", "time_end")

#Subset just the air temperature from the ibutton dataframe
ibutton_dat <- subset(ibutton_dat, Type=="Air",
                  select=File_name:iButton_ID)

#Merge the data logger master sheet (start with ibutton, then do the HOBOs) with the ancillary data
air_temp <- merge(ancil_dat,ibutton_dat,by="Site")

str(air_temp)# 2079 obs. of  9 variables

#The iButtons stopped measuring 06/06/2021, therefore subset the dataframe until then (first two campaigns)

air_temp_ibutton <- subset(air_temp, date < as.Date("2021-06-06") )

str(air_temp_ibutton) #666 obs. of  9 variables

###############################################################################
### Merge HOBO date information and ancillary data ############################
###############################################################################

#Merge the data logger master sheet  with the ancillary data
air_temp2 <- merge(ancil_dat,HOBO_dat,by="Site")

str(air_temp2)# 1849 obs. of  9 variables

#Naiara's HOBO logger started measuring 19/04/2021 except for AL03, BR01, GR01, JO01 which started on 21/06/2021, therefore subset the dataframe after that. 21/06/2021 is when campaign 3 starts, so Naiara's HOBO will cover campaign 3 and later. Check if there are any gaps 

air_temp_HOBO <- subset(air_temp2, date > as.Date("2021-06-20") )
#
#################################################################################################
### Use a loop to select the correct datalogger file and choose the closest air temperature #####


# Start with the ibutton data (air_temp_ibutton)


matTemp<-matrix(rep(NA,nrow(air_temp_ibutton)*1),nrow=nrow(air_temp_ibutton),ncol=1,dimnames=(list(air_temp_ibutton$ID_unique ,c("Temp")))) #creates a matrix with one column for the temperature associated with each unique IF

## Loop for finding nearest temperature to each start time (ibuttons) 

for(i in 1:nrow(air_temp_ibutton)){
  I<-air_temp_ibutton[i,]
  ID<-I$File_name
  Dat <- fread(paste("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/iButton_data_Dryver/Air_water_soil_temperature_ibuttons/Air/",ID,sep=""), header=TRUE)
  colnames(Dat) <- c("DateTime", "Unit", "Temp_C")
  Dat$date <- as.POSIXct(Dat$DateTime, format = "%d/%m/%y %I:%M:%S %p", tz="Europe/Paris", origin = "1970-01-01")
  Dat <- Dat %>% drop_na()
  I$time_start<- as.POSIXct(I$time_start, format="%Y-%m-%d %H:%M", tz="Europe/Paris", origin = "1970-01-01")
  
  setDT(Dat)            ## convert to data.table by reference
  setDT(I)            ## same
  
  I[, date := time_start] 
  setkey(I, time_start)    ## set the column to perform the join on
  setkey(Dat, date)    ## same as above
  
  ans = Dat[I, roll=Inf] ## perform rolling join
  
  matTemp[i,"Temp"]<-ans$Temp_C
  
}


write.table(matTemp,file="C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/ibutton_air_temp.csv") 

################################################################################
##Now for the HOBOs. The tricky thing with the hobos is that we have 4 files for each site, that we should merge into 1 file per site
#There are also some gaps in measurements which may leave out some campaigns: check. 
#HOBOs Measured: April 19 to May 13, July 21 to August 14, September 13 to October 9, October 25 to November 19th
#
#Campaigns: 
# 1. March 16-25 - covered by ibuttons
# 2. May 3-7 - covered by ibuttons
# 3. June 21 to July 1 - No temp data! Will need to use weather station data (see email from Tibo)
# 4.July 19 to 23 - covered by HOBOs
# 5. September 13 to 21 - covered by HOBOs
# 6. October 22 to 28 - covered partly by HOBOs (can use the single HOBO attached to Picarro)
# 7. November 22 to Dec 3 - no temp data! Will need to use weather station data (see email from Tibo)
# 
#At some point we attached an ibutton to the Picarro : September 13 to 14 and October 25 to 28

#Therefore will need to constrain the data from July 19 to Oct 28

#Load and merge all files which contain the site name

#Will need to re download all or just the problem files from the HOBO software. 


#testAL01<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/AL01", pattern='csv', full.names=T,  recursive=TRUE), fread, select = c(2:4), skip=2, header=F, col.names=c("Datetime", "Temp_C", "Light_intensity"))) #OK the workaround means no header but we can add those later. #For one site/folder #next try loop


#testAL01$Datetime <- as.POSIXct(testAL01$Datetime, format = "%m/%d/%y %I:%M:%S %p", tz="Europe/Paris", origin = "1970-01-01")

#plot <- ggplot(testAL01, aes(x=Datetime, y=Temp_C)) +
  #geom_line()  
#plot

#VI01<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/VI01", pattern='csv', full.names=T,  recursive=TRUE), fread, select = c(2:4), skip=2, header=F, col.names=c("Datetime", "Temp_C", "Light_intensity"))) 



#Note light intensity is measured in lum/ft² so will need to convert to lux or metres (whatever is metric)
              

setwd("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/")

filelist <- list.files(path="C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/")


myplot <- function(data, title){
  ggplot(data, aes(x = Datetime,y=Temp_C)) +
    geom_line () + scale_x_datetime(breaks=date_breaks("1 month"), date_labels = "%b") + 
    labs(title = title) # function to make plots in loop
}
  

#Loop to save each csv file in each folder by site


for(i in (filelist)){            
  
  setwd(paste0("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/",i))

  Dat <-do.call(rbind, lapply(list.files(), fread, select = c(2:4), skip=2, header=F, col.names=c("Datetime", "Temp_C", "Light_intensity"))) 
  
  Dat$Datetime <- as.POSIXct(Dat$Datetime, format = "%m/%d/%y %I:%M:%S %p", tz="Europe/Paris", origin = "1970-01-01")
  
assign(i,Dat)

setwd("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/New HOBO files")

write.csv(Dat, paste0("HOBO_temp_", i, ".csv"))

print(myplot(get(i), i)) #loop to plot each site to see if we have any gaps
#
#RA01 stops in September
#ME01 gap September to November
#GR01 gap September to November
#AL03 gap July to September
#AL02 only Dec to Feb #No problem, totally replaced with AL01 in next loop

}

#Next step is to fill the gaps with temperature data from Romain's ibuttons or weather station data

#Identify the gaps:
#AL01: April - Feb
#AL02: Dec - Feb            * Replace completely with AL01, except for Dec-Feb if possible *
#AL03: Jun - Jul, Sep (2021-09-14 15:19:02) - Dec (2021-11-26 13:14:03)  overlaps with all campaign OK
#AL04: April - Dec
#AL05 April - April
#AL06: April - Feb
#AL07: April - Dec
#BR01: Jul - Feb            no problems, covers the June campaign OK
#BR02: April - Feb
#BU01: April - Oct          * ends 2021-10-25 10:21:01 thus doesn't cover November campaign *
#BU02: April - Feb
#CA01: April - Feb
#CA02: April - Feb
#CO01: April - Feb
#GR01: Jun - Sep (2021-09-13 14:02:46), Nov (2021-11-22 08:00:00) - Feb * therefore missing the October campaign *
#JO01: Jun - Feb            
#ME01: May-Sep (2021-09-17 15:17:09), Nov (2021-10-28 10:50:54)-Feb     * therefore missing the October campaign *
#RA01: April - Sep  (2021-09-15 16:41:40)     * therefore missing the October and November campaign  *        
#RO01: April - Oct (2021-10-27 16:20:30)         * therefore missing the November campaign *
#VI01: April - Feb  

## Read-in Romain's leaf pack ibutton metadata:

Romain_dat <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/iButton_data_Dryver/Romain_ibutton_temperatures.csv", sep=";")

#Now check if he has the data for: 
# BU01 November campaign 72_B200000031DB6D21_010422
BU01_11.2021 <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/iButton_data_Dryver/October 2021_leafpacks/72_B200000031DB6D21_010422.csv")
colnames(BU01_11.2021) <- c("Datetime", "Unit", "Temp_C")
BU01_11.2021$Datetime <- as.POSIXct(BU01_11.2021$Datetime, format = "%d/%m/%y %I:%M:%S %p", tz="Europe/Paris", origin = "1970-01-01")

BU01plot<- ggplot(BU01_11.2021, aes(x = Datetime,y=Temp_C)) +
  geom_line () + scale_x_datetime(breaks=date_breaks("1 month"), date_labels = "%b") 
BU01plot

# GR01 October campaign 62_F900000031DAFC21_010422
GR01_10.2021 <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/iButton_data_Dryver/October 2021_leafpacks/62_F900000031DAFC21_010422.csv")
colnames(GR01_10.2021) <- c("Datetime", "Unit", "Temp_C")
GR01_10.2021$Datetime <- as.POSIXct(GR01_10.2021$Datetime, format = "%d/%m/%y %I:%M:%S %p", tz="Europe/Paris", origin = "1970-01-01") #GHG measures on October 26th at 15:30 

GR01plot<- ggplot(GR01_10.2021, aes(x = Datetime,y=Temp_C)) +
  geom_line () + scale_x_datetime(breaks=date_breaks("1 week"), date_labels = "%d/%m") 
GR01plot


# ME01 October campaign 63_0D00000031F9BD21_010422
ME01_10.2021 <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/iButton_data_Dryver/October 2021_leafpacks/63_0D00000031F9BD21_010422.csv")
colnames(ME01_10.2021) <- c("Datetime", "Unit", "Temp_C")
ME01_10.2021$Datetime <- as.POSIXct(ME01_10.2021$Datetime, format = "%d/%m/%y %I:%M:%S %p", tz="Europe/Paris", origin = "1970-01-01") #GHG measures Oct 28 at 10:30

ME01plot<- ggplot(ME01_10.2021, aes(x = Datetime,y=Temp_C)) +
  geom_line () + scale_x_datetime(breaks=date_breaks("1 week"), date_labels = "%d/%m") 
ME01plot

# RA01 October and November campaign 81_4200000031FB2121_010422
RA01_10.2021 <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/iButton_data_Dryver/October 2021_leafpacks/81_4200000031FB2121_010422.csv")
colnames(RA01_10.2021) <- c("Datetime", "Unit", "Temp_C")
RA01_10.2021$Datetime <- as.POSIXct(RA01_10.2021$Datetime, format = "%d/%m/%y %I:%M:%S %p", tz="Europe/Paris", origin = "1970-01-01") #GHG measures at Oct 25th at 12:00

RA01plot<- ggplot(RA01_10.2021, aes(x = Datetime,y=Temp_C)) +
  geom_line () + scale_x_datetime(breaks=date_breaks("1 week"), date_labels = "%d/%m") 
RA01plot


# RO01 November campaign 95_2700000031E3B821_010422
RO01_11.2021 <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/iButton_data_Dryver/October 2021_leafpacks/95_2700000031E3B821_010422.csv")
colnames(RO01_11.2021) <- c("Datetime", "Unit", "Temp_C")
RO01_11.2021$Datetime <- as.POSIXct(RO01_11.2021$Datetime, format = "%d/%m/%y %I:%M:%S %p", tz="Europe/Paris", origin = "1970-01-01")

RO01plot<- ggplot(RO01_11.2021, aes(x = Datetime,y=Temp_C)) +
  geom_line () + scale_x_datetime(breaks=date_breaks("1 week"), date_labels = "%d/%m") 
RO01plot


#################################################################
#Now  for each case, bind data to cover the holes

#BU01
#Remove rows with NA's, clip Romain's data so that it doesn't over lap
BU01 <- BU01 %>% drop_na()
BU01_11.2021 <- subset(BU01_11.2021, Datetime > ("2021-10-25 10:12:48") ) #1831-1711= 120 (dropped 120 obs)
str(BU01) #27237 obs. of  3 variables
BU01 <- rbind(BU01, #2021-04-19 07:00:00 - 2021-10-25 10:12:48 
              BU01_11.2021, fill=TRUE) # 	2021-10-20 10:51:00 - 2022-01-04 15:51:00 #fill=TRUE sets NAs for non-matching columns
str(BU01) #28948 obs. of  4
#Must remove the 4th column (Unit)
BU01 <- subset(BU01, select = -c(Unit) )
plot1<- ggplot(BU01, aes(x = Datetime,y=Temp_C)) +
  geom_line () + scale_x_datetime(breaks=date_breaks("1 month"), date_labels = "%b") 
plot1
write.csv(BU01, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/Filled air temp files/HOBO_temp_BU01.csv")

######################
#GR01
#Remove rows with NA's, clip Romain's data so that it doesn't over lap
GR01 <- GR01 %>% drop_na()
GR01_10.2021 <- subset(GR01_10.2021, Datetime > ("2021-09-13 14:02:46") ) 
GR01_10.2021 <- subset(GR01_10.2021, Datetime < ("2021-11-22 08:00:006") ) 
str(GR01) #23560 obs. of  3 variables
GR01 <- rbind(GR01, # gap 2021-09-13 14:02:46 -2021-11-22 08:00:00
              GR01_10.2021, fill=TRUE) # 2021-10-20 10:05:00 - 2022-01-04 15:05:00
str(GR01) #24351 obs. of  4 variables
GR01 <- subset(GR01, select = -c(Unit) )
plot2<- ggplot(GR01, aes(x = Datetime,y=Temp_C)) +
  geom_line () + scale_x_datetime(breaks=date_breaks("1 month"), date_labels = "%b") 
plot2
write.csv(GR01, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/Filled air temp files/HOBO_temp_GR01.csv")

######################
# ME01 
ME01 <- ME01 %>% drop_na()
ME01_10.2021 <- subset(ME01_10.2021, Datetime > ("2021-09-17 15:17:09") ) 
ME01_10.2021 <- subset(ME01_10.2021, Datetime < ("2021-10-28 10:50:54 ") ) 
str(ME01) #36670 obs. of  3 variables:
ME01 <- rbind(ME01, # gap: 2021-09-17 15:17:09 to 2021-10-28 10:50:54   
              ME01_10.2021, fill=TRUE) # 
str(ME01) #36862 obs. of  4 variables
ME01 <- subset(ME01, select = -c(Unit) )
plot3<- ggplot(ME01, aes(x = Datetime,y=Temp_C)) +
  geom_line () + scale_x_datetime(breaks=date_breaks("1 month"), date_labels = "%b") 
plot3
write.csv(ME01, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/Filled air temp files/HOBO_temp_ME01.csv")

######################
# RA01
RA01 <- RA01 %>% drop_na()
#subset RA01 to remove the 0 in late November
RA01 <- subset(RA01, Datetime < ("2021-09-15 16:41:40") )
RA01_10.2021 <- subset(RA01_10.2021, Datetime > ("2021-09-15 16:41:40") ) #doesn't remove any
str(RA01) #21514 obs. of  3 variables 
RA01 <- rbind(RA01, # stops 2021-09-15 16:41:40  
              RA01_10.2021, fill=TRUE) # 
str(RA01) #23344 obs. of  4 variables
RA01 <- subset(RA01, select = -c(Unit) )
plot4<- ggplot(RA01, aes(x = Datetime,y=Temp_C)) +
  geom_line () + scale_x_datetime(breaks=date_breaks("1 month"), date_labels = "%b") 
plot4
write.csv(RA01, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/Filled air temp files/HOBO_temp_RA01.csv")

######################
# RO01
RO01 <- RO01 %>% drop_na()
RO01_11.2021 <- subset(RO01_11.2021, Datetime > ("2021-10-27 16:20:30") )
str(RO01) #27568 obs. of  3 variables
RO01 <- rbind(RO01, # Stops 2021-10-27 16:20:30
              RO01_11.2021, fill=TRUE) # 
str(RO01) #29224 obs. of  4 variables
RO01 <- subset(RO01, select = -c(Unit) )
plot5<- ggplot(RO01, aes(x = Datetime,y=Temp_C)) +
  geom_line () + scale_x_datetime(breaks=date_breaks("1 month"), date_labels = "%b") 
plot5
write.csv(RO01, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/Filled air temp files/HOBO_temp_RO01.csv")

################################################################################
########## Now run the same loop to select the nearest temperature for the HOBO data ######################
## In the "Filled air temp files" folder you will need to add all of the other files that did not need to be filled, and make this the folder locatio for the loop

air_temp_HOBO <- subset(air_temp2, date > as.Date("2021-06-20") ) #Will need to subset the data to exclude the first two campaigns

matTemp2<-matrix(rep(NA,nrow(air_temp_HOBO)*1),nrow=nrow(air_temp_HOBO),ncol=1,dimnames=(list(air_temp_HOBO$ID_unique ,c("Temp")))) #creates a matrix with one column for the temperature associated with each unique IF

## Loop for finding nearest temperature to each start time (HOBOs) 

for(i in 1:nrow(air_temp_HOBO)){
  I<-air_temp_HOBO[i,]
  ID<-I$File_name
  Dat <- fread(paste("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/Filled air temp files/",ID,sep=""), header=TRUE)
  colnames(Dat) <- c("", "Datetime", "Temp_C", "Light_intensity")
  Dat$Datetime <- as.POSIXct(Dat$Datetime, format = "%Y/%m/%d %I:%M:%S %p", tz="Europe/Paris", origin = "1970-01-01")
  Dat <- Dat %>% drop_na() #added bc problems of NAs in final mattemp file
  
  I$time_start<- as.POSIXct(I$time_start, format="%Y-%m-%d %H:%M", tz="Europe/Paris", origin = "1970-01-01")
  
  setDT(Dat)            ## convert to data.table by reference
  setDT(I)            ## same
  
  I[, date := time_start] 
  setkey(I, time_start)    ## set the column to perform the join on
  setkey(Dat, Datetime)    ## same as above
  
  ans = Dat[I, roll=Inf] ## perform rolling join
  
  matTemp2[i,"Temp"]<-ans$Temp_C
}


write.table(matTemp2,file="C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/HOBO_air_temp.csv")

######################### Save the ibutton and HOBO data into a single file #######

#first you will need to read in the CSV's using fread because of the weird formatting of write.table

air.temp1 <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/ibutton_air_temp.csv") #For C1 and C2 #ignore warning, column headers are OK
colnames(air.temp1) <- c("ID_unique", "Air_temp")

air.temp2 <-fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/HOBO_air_temp.csv") #for other campaigns
colnames(air.temp2) <- c("ID_unique", "Air_temp")

# Now rbind the two temperature files together vertically 

air.temp <- rbind (air.temp1, air.temp2)

#Save as a csv

write.csv(air.temp, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/air_temp_full.csv")


