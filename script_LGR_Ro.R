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
### Import data  
#################################

# RAW DATA is in different .txt files 
# load all the files
# Due to the metadata at the end of the files, must use fread() which automatically cuts it out

#this is how you input one file
LGRraw_2021.03.25_1 <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/LGR_raw_data/2021-03-25_LGR/n2o-co_2021-03-25_f0000.txt")

#now input and combine all the files for a date (then for the entire first campaign)
#first list all of the files in the folder
LGR_25March21 <- list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/LGR_raw_data/2021-03-25_LGR", pattern='txt', full.names=T, recursive=TRUE) 
#to include sub-directories, change the recursive T, else F

LGR_25March21<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/LGR_raw_data/2021-03-25_LGR", pattern='txt', full.names=T, recursive=TRUE), fread ,header=T))

#now input and combine all the files for the entire first campaign
LGR_March2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/LGR_raw_data", pattern='txt', full.names=T, recursive=TRUE), fread ,header=T))

#Subset just the useful columns (Time, [N2O]d_ppm, AmbT_C)
LGR_March2021<- subset(LGR_March2021, select = c( "Time", "[N2O]d_ppm", "AmbT_C"))

#Rename the N2O column to get rid of the square brackets because they can cause some issues later
names(LGR_March2021)[names(LGR_March2021) == "[N2O]d_ppm"] <- "N2O_ppm"

#select the time range of each measure
#start by inputting the ancillary data files
ancil_dat_aquatic <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/Ancillary_data/Campaign1_dataentry - Aquatic 2021.04.02.csv", header=T)
str(ancil_dat_aquatic)

ancil_dat_riparian <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/Ancillary_data/Campaign1_dataentry - Riparian 2021.04.02.csv", header=T)
str(ancil_dat_riparian)

#add the date to the start and end time columns for AQUATIC
ancil_dat_aquatic$time_start<- as.POSIXct(paste(ancil_dat_aquatic$date, ancil_dat_aquatic$time_start), format="%Y-%m-%d %H:%M")
ancil_dat_aquatic$time_end<- as.POSIXct(paste(ancil_dat_aquatic$date, ancil_dat_aquatic$time_end), format="%Y-%m-%d %H:%M")

#add the date to the start and end time columns for RIPARIAN
ancil_dat_riparian$time_start<- as.POSIXct(paste(ancil_dat_riparian$date, ancil_dat_riparian$time_start), format="%Y-%m-%d %H:%M")
ancil_dat_riparian$time_end<- as.POSIXct(paste(ancil_dat_riparian$date, ancil_dat_riparian$time_end), format="%Y-%m-%d %H:%M")


#Cut the concentration based on the Start and End times from the ancillary data sheet
#You can do this manuallym following Naiara's script

# change the time format to "as.POSIXct" time - R's time format.
LGR_March2021$Time <- as.POSIXct(LGR_March2021$Time, format = "%d/%m/%Y %H:%M:%OS")
#Here we lose the decimal fraction of the seconds, but I don't think it is important, since we measure every second...

write.csv(LGR_March2021, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/March2021_LGR_data_processed.csv")

#Write a loop to clip the [N2O] by start time and end time

#start by subsetting just the LGR times
LGR_aq<-ancil_dat_aquatic[ancil_dat_aquatic$Picarro_LGR=="LGR",]
LGR_ri<-ancil_dat_riparian[ancil_dat_riparian$Picarro_LGR=="LGR",]


#make a new ID column with the site and point ID 
ID<-paste(LGR_aq$siteID_new,LGR_aq$point,sep="_")
#ID<-paste(LGR_ri$siteID_new,LGR_ri$point,sep="_")

#Set the start and end times
startT<-LGR_aq$time_start
endT<-LGR_aq$time_end

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

FinalTable<-get(paste("data",length(startT),sep="_"))
FinalTable

#RIPARIAN
CA02_R1<-LGR_March2021[Time >= "2021-03-16 10:09:00" & Time <= "2021-03-16 10:14:00"]
CA02_R2<-LGR_March2021[Time >= "2021-03-16 11:39:00" & Time <= "2021-03-16 11:43:00"]
CA02_R4<-LGR_March2021[Time >= "2021-03-16 12:14:00" & Time <= "2021-03-16 12:19:00"]
CA02_R6<-LGR_March2021[Time >= "2021-03-16 14:12:00" & Time <= "2021-03-16 14:17:00"]

#AQUATIC
CA02_A1<-LGR_March2021[Time >= "2021-03-16 09:53:00" & Time <= "2021-03-16 10:56:00"]
CA02_B1<-LGR_March2021[Time >= "2021-03-16 10:32:00" & Time <= "2021-03-16 10:37:00"]
CA02_C1<-LGR_March2021[Time >= "2021-03-16 10:59:00" & Time <= "2021-03-16 11:05:00"]
CA02_D1<-LGR_March2021[Time >= "2021-03-16 12:26:00" & Time <= "2021-03-16 12:31:00"]
CA02_E1<-LGR_March2021[Time >= "2021-03-16 13:47:00" & Time <= "2021-03-16 13:52:00"]
CA02_F1<-LGR_March2021[Time >= "2021-03-16 14:22:00" & Time <= "2021-03-16 14:27:00"]

#create an ID column for each measure
CA02_R1$ID <- rep("CA02_R1",nrow(CA02_R1)) 
CA02_R2$ID <- rep("CA02_R2",nrow(CA02_R2))
CA02_R4$ID <- rep("CA02_R4",nrow(CA02_R4))
CA02_R6$ID <- rep("CA02_R6",nrow(CA02_R6))

CA02_A1$ID <- rep("CA02_A1",nrow(CA02_A1)) 
CA02_B1$ID <- rep("CA02_B1",nrow(CA02_B1)) 
CA02_C1$ID <- rep("CA02_C1",nrow(CA02_C1)) 
CA02_D1$ID <- rep("CA02_D1",nrow(CA02_D1)) 
CA02_E1$ID <- rep("CA02_E1",nrow(CA02_E1)) 
CA02_F1$ID <- rep("CA02_F1",nrow(CA02_F1)) 


#plot
#par(mfrow=c(2,6))
plot(N2O_ppm~ Time, data=LGR_March2021)
plot(N2O_ppm~ Time, data=CA02_R2)
plot(N2O_ppm~ Time, data=CA02_R4)
plot(N2O_ppm~ Time, data=CA02_R6)


plot(N2O_ppm~ Time, data=CA02_A1)
plot(N2O_ppm~ Time, data=CA02_B1)
plot(N2O_ppm~ Time, data=CA02_C1)
plot(N2O_ppm~ Time, data=CA02_D1)
plot(N2O_ppm~ Time, data=CA02_E1)
plot(N2O_ppm~ Time, data=CA02_F1)


#But it would be faster to automate these steps






###########################


#In order to set the initial time of each measurement to 0: 
#start by adding a column for epoch time (expressed as seconds since Jan 1, 1970)
LGR_March2021$epoch_time <- as.integer(as.POSIXct(LGR_March2021$Time), tz="Europe/Paris")

#then set  the initial time of each measure to 0h 
#use Naiara's function to rest the min time to each time

rescale <- function(x) (x-min(x))

#apply this function to all epoch_time (seconds) of each measure, 
#and divide by 36000 (hours)
LGR_March2021 <- setDT(LGR_March2021)[,c("flux_time"):=.(rescale(epoch_time/3600)),by=.(ID)]


#Convert N2O concentration from ppm to ug-N/L using the ideal gas law (PV=nRT) for input to gasfluxes package. Note that ppm = uL/L
# ug-N/L = ((N2O concentration in ppm  * molecular mass of nitrogen *1 atm )) / (0.08206	L·atm/K·mol * temperature in K )

LGR_March2021$N2O_ug_L <- ((LGR_March2021$N2O_ppm  * 14.0067 *1 )) / (0.082*(LGR_March2021$AmbT_C + 273.15))



#plot to view
plot(LGR_March2021$N2O_ug_L ~ LGR_March2021$Time, type="l")





