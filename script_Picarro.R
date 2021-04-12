library(lubridate)

#########################################
### load raw data and metadata
### put the date-time in the same format for all files
##########################################

#load raw data of each day

raw16March21<-read.csv("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/raw16March21.csv", header=T)
raw17March21<-read.csv("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/raw17March21.csv", header=T)
raw18March21<-read.csv("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/raw18March21.csv", header=T)
raw19March21<-read.csv("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/raw19March21.csv", header=T)
raw22March21<-read.csv("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/raw22March21.csv", header=T)
raw23March21<-read.csv("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/raw23March21.csv", header=T)
raw24March21<-read.csv("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/raw24March21.csv", header=T)
raw25March21<-read.csv("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/raw25March21.csv", header=T)

rawMarch21<-rbind(raw16March21,raw17March21,raw18March21,raw19March21,raw22March21,raw23March21,raw24March21,raw25March21)
str(rawMarch21)

write.csv(rawMarch21,"C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/rawMarch21.csv")

#load metadata
metadata<-read.csv("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/Campaign1_dataentry.csv", sep=";", header=T)
str(metadata)

#put date in the same format that raw data files: Y-m-d 
date_metadata<-format(as.Date(metadata$date,"%d/%m/%Y"),"%Y-%m-%d")
metadata<-cbind(metadata, date_metadata)
str(metadata)

metadata$start <- paste(date_metadata, metadata$time_start)
metadata$end <- paste(date_metadata, metadata$time_end)

#correct time (quit 1h) to fit with picarro data
metadata$start <- as.POSIXlt(metadata$start) -3600
metadata$end <- as.POSIXlt(metadata$end ) -3600

str(metadata)

#create unique id and subset only picarro data

metadata$ID<-paste0(metadata$siteID_new,"_",metadata$point)
metadata_pic <- subset (metadata, Picarro_LGR == "Picarro")
str(metadata_pic)

write.csv(metadata_pic, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/metadata_pic.csv")

#########################################################################
#### load metadata (start and end points)
#### load data from the picarro (already joint in one file
###########################################################

# Install packages 
library(lubridate)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(plyr)
library(gasfluxes)
library(tidyverse)
library(data.table)

metadata_pic<-read.csv("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/metadata_pic.csv")
rawMarch21<-read.csv("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/rawMarch21.csv")
head(metadata_pic)
head(rawMarch21)

# clip the raw data by start time and end time

ID <- metadata_pic$ID
startT<-metadata_pic$start #start times (118)
endT<-metadata_pic$end  # end times (118)

for(i in 1:length(startT)){
  st<-startT[i]
  se<-endT[i]
  id<-ID[i]
  data<-rawMarch21[rawMarch21$time >= st & rawMarch21$time <= se,]
  data$ID<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
}

Picarro_flow <-get(paste("data",length(startT),sep="_"))
str(Picarro_flow) # 38414 obs.

###########################
### now we have the data split by times
##############################

### there are duplicated time rows in the data, delete them

Picarro_flow2 <- Picarro_flow  %>% 
  # Base the removal on the "epoch_time" column
  distinct(EPOCH_TIME, .keep_all = TRUE)

str(Picarro_flow2) # 37989 obs.

#create function to rest the min time to each time
rescale <- function(x) (x-min(x))

Picarro_flow3 <- setDT(Picarro_flow2)[,c("flux_time"):=.(rescale(EPOCH_TIME/3600)),by=.(ID)]
str(Picarro_flow3) # 37989 obs.

## keep only the disered columns from picarro data
Picarro_flow4 <- subset(Picarro_flow3, select = c( "CavityTemp", "CH4_dry", "CO2_dry", "ID", "flux_time"))
str(Picarro_flow4) # 37989 obs.

## keep only the disered columns from metadata_pic
head(metadata_pic)
metadata_pic2 <- subset(metadata_pic, select = c( "ID", "total_volume_L", "chamber_area_m2"))

##### merge both files by ID

data_fluxCal<- merge (Picarro_flow4, metadata_pic2 , by="ID")



#############################################
### flux calculations                    ####
#############################################

#put CO2 and CH4 concentration (now in ppm) in mg/L
# mg/L = ((ppm  * molecular mass *1 atm )/1000) / (0.082 * 293K )

data_fluxCal$C_CO2_mg_L <- ((data_fluxCal$CO2_dry  * 12 *1 )/1000) / (0.082*(data_fluxCal$CavityTemp + 273))
data_fluxCal$C_CH4_ug_L <- ((data_fluxCal$CH4_dry  * 12 *1 )) / (0.082*(data_fluxCal$CavityTemp + 273))

#some values for CO2 or CH4 are < 0 , delete them

data_fluxCal_2<-data_fluxCal[!(data_fluxCal$C_CO2_mg_L<="0")] 
data_fluxCal_3<-data_fluxCal_2[!(data_fluxCal_2$C_CH4_ug_L<="0")] 

str(data_fluxCal) # 37989 obs.
str(data_fluxCal_2) #  37986 obs. 
str(data_fluxCal_3) # 37984 obs. 

write.csv(data_fluxCal_3, "C:/Users/naina/OneDrive/Escritorio/WP3_data analysis/data_fluxCal_3.csv")

#Check the units
#V = L
#A = m2
# flux time = h
# concentration of CO2 / CH4 = mg/L

#[f0] = mg/m^2/h

data_fluxCal_3$total_volume_L # 3.5... L
data_fluxCal_3$chamber_area_m2 # 0.045... m2
data_fluxCal_3$C_CO2_mg_L # ~2.30 mg/L
data_fluxCal_3$C_CH4_ug_L # ~1.53 mg/L
data_fluxCal_3$flux_time # Tfin= 0.0833h = 5min


CO2_FLOW <- gasfluxes(data_fluxCal_3, .id = "ID", 
		.V = "total_volume_L", .A = "chamber_area_m2", 
		.times = "flux_time",.C = "C_CO2_mg_L", 
		methods = c("robust linear"))

CH4_FLOW <- gasfluxes(data_fluxCal_3, .id = "ID", 
		.V = "total_volume_L", .A = "chamber_area_m2", 
		.times = "flux_time",.C = "C_CH4_ug_L", 
		methods = c("robust linear"))

#save model outputs

write.csv (CO2_FLOW, "C:/Users/naina/OneDrive/Escritorio/WP3_data analysis/CO2_FLOW.csv")
write.csv (CH4_FLOW, "C:/Users/naina/OneDrive/Escritorio/WP3_data analysis/CH4_FLOW.csv")

##################
# load those files

fluxes_CO2 <- read.csv ("C:/Users/naina/OneDrive/Escritorio/WP3_data analysis/CO2_FLOW.csv")
str(fluxes_CO2)

fluxes_CH4 <- read.csv ("C:/Users/naina/OneDrive/Escritorio/WP3_data analysis/CH4_FLOW.csv")
str(fluxes_CH4)

fluxes1 <- fluxes_CO2[, c(2,3)]
fluxes1 <- setNames(fluxes1, c("ID", "C_CO2_mg_m2_h"))

fluxes2 <- fluxes_CH4[, c(2,3)]
fluxes2 <- setNames(fluxes2, c("ID", "C_CH4_ug_m2_h"))

fluxes <- merge (fluxes1, fluxes2, by= "ID")
str(fluxes)

fluxes <- fluxes  %>%
  separate(ID, c("SITE", "measure"), "_")

par(mfrow=c(2,1))
boxplot(C_CO2_mg_m2_h ~ SITE, data=fluxes)
boxplot(C_CH4_ug_m2_h ~ SITE, data=fluxes, ylim=c(0, 200))



