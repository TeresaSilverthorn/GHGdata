#### Script for extracting air temperature data ####
# 
# Load the necessary packages
#load necessary packages
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(survival)
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


#Read in the ancilalry data file which has the start and end times for GHG measurements

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
  colnames(Dat) <- c("DateTime", "Unit", "Temp.C")
  Dat$date <- as.POSIXct(Dat$DateTime, format = "%d/%m/%y %I:%M:%S %p", tz="Europe/Paris", origin = "1970-01-01")
  
  I$time_start<- as.POSIXct(I$time_start, format="%Y-%m-%d %H:%M", tz="Europe/Paris", origin = "1970-01-01")
  
  setDT(Dat)            ## convert to data.table by reference
  setDT(I)            ## same
  
  I[, date := time_start] 
  setkey(I, time_start)    ## set the column to perform the join on
  setkey(Dat, date)    ## same as above
  
  ans = Dat[I, roll=Inf] ## perform rolling join
  
  matTemp[i,"Temp"]<-ans$Temp.C
}


write.table(matTemp,file="ibutton_air_temp.csv")

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
# 6. October 22 to 28 - covered partly by HOBOs
# 7. November 22 to Dec 3 - no temp data! Will need to use weather station data (see email from Tibo)
# 
#At some point we attached an ibutton to the Picarro : September 13 to 14 and October 25 to 28

#Therefore will need to constrain the data from July 19 to Oct 28

#Load and merge all files which contain the site name



AL01<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/AL01", pattern='csv', full.names=T,  recursive=TRUE), fread, select = c(2:4), skip=2, header=F, col.names=c("Datetime", "Temp_C", "Light_intensity"))) #OK the workaround means no header but we can add those later. #For one site/folder #next try loop

VI01<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/VI01", pattern='csv', full.names=T,  recursive=TRUE), fread, select = c(2:4), skip=2, header=F, col.names=c("Datetime", "Temp_C", "Light_intensity"))) 



#Note light intensity is measured in lum/ft² so will need to convert to lux or metres (whatever is metric)
              

setwd("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/")

filelist <- list.files(path="C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/")


for(i in (filelist)){             #here put list.files() of the path to By site 
  
  setwd(paste0("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/",i))

  Dat <-do.call(rbind, lapply(list.files(), fread, select = c(2:4), skip=2, header=F, col.names=c("Datetime", "Temp_C", "Light_intensity"))) 
  
assign(i,Dat)

}









data_files <- list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/")  # Identify file names
data_files                                                    # Print file names


#Now, we can write a for-loop containing the assign, paste0, and read.csv2 functions to read and save all files in our directory:
  
  for(i in 1:length(data_files)) { # Head of for-loop
    assign(paste0("data", I),                                   # Read and store data frames
           fread(paste0("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/",  
                            data_files[i])))
  }


Dat <- do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/light loggers_FRA/By site/",ID, sep=""), pattern='csv', full.names=T,  recursive=TRUE), fread, select = c(2:4), skip=2, header=F, col.names=c("Datetime", "Temp_C", "Light_intensity"))
















## Now run the same loop for the HOBOs

matTemp2<-matrix(rep(NA,nrow(air_temp_ibutton)*1),nrow=nrow(air_temp_ibutton),ncol=1,dimnames=(list(air_temp_ibutton$ID_unique ,c("Temp")))) #creates a matrix with one column for the temperature associated with each unique IF

## Loop for finding nearest temperature to each start time (ibuttons) 

for(i in 1:nrow(air_temp_ibutton)){
  I<-air_temp_ibutton[i,]
  ID<-I$File_name
  Dat <- fread(paste("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Ancillary Data/iButton_data_Dryver/Air_water_soil_temperature_ibuttons/Air/",ID,sep=""), header=TRUE)
  colnames(Dat) <- c("DateTime", "Unit", "Temp.C")
  Dat$date <- as.POSIXct(Dat$DateTime, format = "%d/%m/%y %I:%M:%S %p", tz="Europe/Paris", origin = "1970-01-01")
  
  I$time_start<- as.POSIXct(I$time_start, format="%Y-%m-%d %H:%M", tz="Europe/Paris", origin = "1970-01-01")
  
  setDT(Dat)            ## convert to data.table by reference
  setDT(I)            ## same
  
  I[, date := time_start] 
  setkey(I, time_start)    ## set the column to perform the join on
  setkey(Dat, date)    ## same as above
  
  ans = Dat[I, roll=Inf] ## perform rolling join
  
  matTemp[i,"Temp"]<-ans$Temp.C
}


write.table(matTemp,file="ibutton_air_temp.csv")





