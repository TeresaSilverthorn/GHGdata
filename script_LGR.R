##########################################################################
##### Script for importing and processing N2O data from LGR analyzer #####
##########################################################################

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

#################################
######## Import data  ##########
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


################################################################
# Import the ancillary data files with the start and end times #
################################################################

#import the ancillary data files for the aquatic and riparian measurements
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

###########################################################
######### Cut the N2O ppm data by start and end time ######
###########################################################

#Now, cut the concentration based on the Start and End times from the ancillary data sheet

####### OPTION 1 #########################################
### You can do this manually following Naiara's script ###
##########################################################

# change the time format to "as.POSIXct" time - R's time format.
LGR_March2021$Time <- as.POSIXct(LGR_March2021$Time, format = "%d/%m/%Y %H:%M:%OS")
#Here we lose the decimal fraction of the seconds, but I don't think it is important, since we measure every second...

write.csv(LGR_March2021, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/March2021_LGR_data_processed.csv")


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


####### OPTION 2 #############
### You can automate this process using a loop
##############################

# It would be faster to automate these steps 
# Write a loop to clip the [N2O] by start time and end time

#start by subsetting just the LGR times
LGR_aq<-ancil_dat_aquatic[ancil_dat_aquatic$Picarro_LGR=="LGR",]
LGR_ri<-ancil_dat_riparian[ancil_dat_riparian$Picarro_LGR=="LGR",]

#Loop to cut N2O concentrations by the start and end times, and create an ID column with the site and point


#AQUATIC

#make a new ID column with the site and point ID 
ID_aq<-paste(LGR_aq$siteID_new,LGR_aq$point,sep="_")

#Set the start and end times
startT<-LGR_aq$time_start
endT<-LGR_aq$time_end

for(i in 1:length(startT)){
  st<-startT[i]
  se<-endT[i]
  id<-ID_aq[i]
  data<-LGR_March2021[LGR_March2021$Time >= st & LGR_March2021$Time <= se,]
  data$ID_aq<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
  
}

N2O_aquatic<-get(paste("data",length(startT),sep="_"))
N2O_aquatic


#RIPARIAN

#make a new ID column with the site and point ID 
ID_ri<-paste(LGR_ri$siteID_new,LGR_ri$point,sep="_")

#Set the start and end times
startT_ri<-LGR_ri$time_start
endT_ri<-LGR_ri$time_end

for(i in 1:length(startT_ri)){
  st<-startT_ri[i]
  se<-endT_ri[i]
  id<-ID_ri[i]
  N2Odata_rip<-LGR_March2021[LGR_March2021$Time >= st & LGR_March2021$Time <= se,]
  N2Odata_rip$ID_ri<-id
  
  if(i==1){
    assign(paste("N2Odata_rip",i, sep="_"),N2Odata_rip)
  } else {
    assign(paste("N2Odata_rip",i, sep="_"),N2Odata_rip)
    assign(paste("N2Odata_rip",i, sep="_"),rbind(get(paste("N2Odata_rip",i, sep="_")),get(paste("N2Odata_rip",i-1, sep="_"))))
  }
  
}

N2O_riparian<-get(paste("N2Odata_rip",length(startT_ri),sep="_"))
N2O_riparian

##############################################################################
############# Now make a flux rate figure for each point ####################

#Make ggplots in a loop

#Add a column for site, so you can group the figures by site
N2O_riparian$Site <- substr(N2O_riparian$ID_ri, 1, 4)
N2O_aquatic$Site <- substr(N2O_aquatic$ID_aq, 1, 4)

# list of unique ID values to loop over
N2O_ri = unique(N2O_riparian$ID_ri)

# Loop to create and save a plot for each point

for (i in N2O_ri) {
  
  temp_plot = ggplot(data= subset(N2O_riparian, ID_ri == i)) + 
    geom_point(size=3, aes(x=Time, y=N2O_ppm)) +
    ggtitle(i) +
  
  ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

#now figure out a way to save the all of the points at one site on a single page...

for (var in unique(mydata$Variety)) {
  dev.new()
  print( ggplot(mydata[mydata$Variety==var,], aes(Var1, Var2)) + geom_point() )
}




##############################################################################
#### Now convert the data to ug-N/L, get the slope of each relationship of gas concentration over time, and correct by area/volume of chamber###############
#############################################################################


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





