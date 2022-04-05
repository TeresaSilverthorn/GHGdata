############# Extracting and cleaning temperature data from iButtons ##########
install.packages(c("survival"))
#load necessary packages
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(survival)

## Load the ancillary data
MetaData <- read.csv("C:/Users/romain.sarremejane/Documents/MARIE CURIE/DATA FROM FIELD AND LAB/Temperatures.csv",sep=";", header=T)
str(air_temp_ibutton)

# Reformat time columns
air_temp_ibutton$Deployment_date <- as.POSIXct(air_temp_ibutton$time_start, format="%Y-%m-%d %H:%M", tz="Europe/Paris", origin = "1970-01-01")


#Since you don't have all of the ancilary data, subset the rows without NAs
#MetaData <- MetaData[complete.cases(MetaData[ , "ID"]),]

### Manual clipping ####


matTemp<-matrix(rep(NA,nrow(air_temp_ibutton)*1),nrow=nrow(air_temp_ibutton),ncol=1,dimnames=(list(air_temp_ibutton$ID_unique ,c("Temp"))))

##AL01 Benth_leafpack ##   
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
}

write.table(matTemp,file="TempMSCA.csv")
  
with(Dat2, plot(Temp.C ~ DateTime, type="l")) #plot

#add ID columns
AL01_Benth_leafpack$Site <- c("AL01")
AL01_Benth_leafpack$Habitat <- c("Benth_leafpack")

#####################

## RO01 Benth_leafpack ##   

## 1) Loading an individual temperature file 
site.name <- "RO01_Benth_leafpack"
RO01_Benth_leafpack <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/iButton_data_Dryver/94_6E00000031E65E21_051721.csv", header=TRUE)
colnames(RO01_Benth_leafpack) <- c("DateTime", "Unit", "Temp.C")
head(RO01_Benth_leafpack)
str(RO01_Benth_leafpack)

# Uses the time format of the imported csv to define an "as.POSIXct" time - R's time format.
RO01_Benth_leafpack$DateTime <- as.POSIXct(RO01_Benth_leafpack$DateTime, format = "%d/%m/%y %I:%M:%S %p")
plot(RO01_Benth_leafpack$Temp.C ~ RO01_Benth_leafpack$DateTime, type="l")


## 2) subset by deployment start end dates
#Subset by deployment dates

site.date <- ancil_dat %>%
  filter(ID == site.name) %>%
  head
int <- interval(site.date$time_start, site.date$time_end)

RO01_Benth_leafpack <- RO01_Benth_leafpack[RO01_Benth_leafpack$DateTime %within% int,]
with(RO01_Benth_leafpack, plot(Temp.C ~ DateTime, type="l"))

#add ID columns
RO01_Benth_leafpack$Site <- c("RO01")
RO01_Benth_leafpack$Habitat <- c("Benth_leafpack")

#####################

## RO01 Rip_leafpack##   

## 1) Loading an individual temperature file 
site.name <- "RO01_Rip_leafpack"
RO01_Rip_leafpack <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/iButton_data_Dryver/95_2700000031E3B821_051721.csv", header=TRUE)
colnames(RO01_Rip_leafpack) <- c("DateTime", "Unit", "Temp.C")
head(RO01_Rip_leafpack)
str(RO01_Rip_leafpack)

# Uses the time format of the imported csv to define an "as.POSIXct" time - R's time format.
RO01_Rip_leafpack$DateTime <- as.POSIXct(RO01_Rip_leafpack$DateTime, format = "%d/%m/%y %I:%M:%S %p")
plot(RO01_Rip_leafpack$Temp.C ~ RO01_Rip_leafpack$DateTime, type="l")


## 2) subset by deployment start end dates
#Subset by deployment dates

site.date <- ancil_dat %>%
  filter(ID == site.name) %>%
  head
int <- interval(site.date$time_start, site.date$time_end)

RO01_Rip_leafpack <- RO01_Rip_leafpack[RO01_Rip_leafpack$DateTime %within% int,]
with(RO01_Rip_leafpack, plot(Temp.C ~ DateTime, type="l"))

#add ID columns
RO01_Rip_leafpack$Site <- c("RO01")
RO01_Rip_leafpack$Habitat <- c("Rip_leafpack")


#####################

## 	AL04_Rip_leafpack ##   

## 1) Loading an individual temperature file 
site.name <- "AL04_Rip_leafpack"
AL04_Rip_leafpack <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/iButton_data_Dryver/98_8900000031EB8421_051721.csv", header=TRUE)
colnames(AL04_Rip_leafpack) <- c("DateTime", "Unit", "Temp.C")
head(AL04_Rip_leafpack)
str(AL04_Rip_leafpack)

# Uses the time format of the imported csv to define an "as.POSIXct" time - R's time format.
AL04_Rip_leafpack$DateTime <- as.POSIXct(AL04_Rip_leafpack$DateTime, format = "%d/%m/%y %I:%M:%S %p")
plot(AL04_Rip_leafpack$Temp.C ~ AL04_Rip_leafpack$DateTime, type="l")


## 2) subset by deployment start end dates
#Subset by deployment dates

site.date <- ancil_dat %>%
  filter(ID == site.name) %>%
  head
int <- interval(site.date$time_start, site.date$time_end)

AL04_Rip_leafpack <- AL04_Rip_leafpack[AL04_Rip_leafpack$DateTime %within% int,]
with(AL04_Rip_leafpack, plot(Temp.C ~ DateTime, type="l"))

#add ID columns
AL04_Rip_leafpack$Site <- c("AL04")
AL04_Rip_leafpack$Habitat <- c("Rip_leafpack")

#####################

## 	BU02_Rip_leafpack ##   

## 1) Loading an individual temperature file 
site.name <- "BU02_Rip_leafpack"
BU02_Rip_leafpack <- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/iButton_data_Dryver/99_FF00000031E5CC21_051721.csv", header=TRUE)
colnames(BU02_Rip_leafpack) <- c("DateTime", "Unit", "Temp.C")
head(BU02_Rip_leafpack)
str(BU02_Rip_leafpack)

# Uses the time format of the imported csv to define an "as.POSIXct" time - R's time format.
BU02_Rip_leafpack$DateTime <- as.POSIXct(BU02_Rip_leafpack$DateTime, format = "%d/%m/%y %I:%M:%S %p")
plot(BU02_Rip_leafpack$Temp.C ~ BU02_Rip_leafpack$DateTime, type="l")


## 2) subset by deployment start end dates
#Subset by deployment dates

site.date <- ancil_dat %>%
  filter(ID == site.name) %>%
  head
int <- interval(site.date$time_start, site.date$time_end)

BU02_Rip_leafpack <- BU02_Rip_leafpack[BU02_Rip_leafpack$DateTime %within% int,]
with(BU02_Rip_leafpack, plot(Temp.C ~ DateTime, type="l"))

#add ID columns
BU02_Rip_leafpack$Site <- c("BU02")
BU02_Rip_leafpack$Habitat <- c("Rip_leafpack")

##########################################
#Combine the data, merge vertically

temp_sample <- rbind(AL01_Benth_leafpack, AL04_Rip_leafpack, BU02_Rip_leafpack, RO01_Benth_leafpack, RO01_Rip_leafpack)
temp_sample$ID <- paste(temp_sample$Site,temp_sample$Habitat,sep="_")
temp_sample$ID <- as.factor(temp_sample$ID)
temp_sample$Site <- as.factor(temp_sample$Site)
temp_sample$Habitat <- as.factor(temp_sample$Habitat)
str(temp_sample)


#Plot temp by site
ggplot(temp_sample, aes(x = DateTime, y = Temp.C)) + 
  geom_line(aes(color = ID), size = .2) + theme(legend.position="top") +
  theme_minimal()
















##### Get Romain to help you write a loop (need to cut befor you join all the data together) ####
## 1) Load all of the iButton .csv files

temp <-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/iButton_data_Dryver", pattern='csv', full.names=T, recursive=TRUE), fread ,header=T))
str(temp) #7531 obs.

colnames(temp) <- c("DateTime", "Unit", "Temp.C") #rename the columns
head(temp)

## 2) Define time as an "as.POSIXct" time - R's time format.
temp$DateTime <- as.POSIXct(temp$DateTime, format = "%d/%m/%y %I:%M:%S %p")
#plot to view
plot(temp$Temp.C ~ temp$DateTime, type="l")


## 4) Export results
#write.csv(air.temp, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/iButton data/air.temp.2021.03.csv")

