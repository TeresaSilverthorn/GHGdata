#################
# Install packages 
library(lubridate)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(plyr)
library(gasfluxes)
library(tidyverse)
library(data.table)
####################


#########################################
### load raw data and metadata
### put the date-time in the same format for all files
##########################################

#load raw data of each day for the entire campaign

Picarro_March2021<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Campaign1_raw_data/Picarro_raw_data/", pattern='dat', full.names=T, recursive=TRUE), fread ,header=T))
#to include sub-directories, change the recursive T
str(Picarro_March2021)

# create a column merging date and time
time<-as.POSIXct(paste(Picarro_March2021$DATE, Picarro_March2021$TIME), format="%Y-%m-%d %H:%M:%S")
Picarro_March2021<-cbind(Picarro_March2021,time)
str(Picarro_March2021)

#Since the Picarro data is in GMT time, we need to add one hour to correspond to the actual time (GMT+1 time)
Picarro_March2021$time<- as.POSIXlt(Picarro_March2021$time) +3600

write.csv(Picarro_March2021,"C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Picarro_raw_March2021.csv")

##########################################################
#load the ancillary data

ancil_dat_aquatic <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Campaign1_raw_data/Ancillary_data/Campaign1_dataentry - Aquatic 2021.04.19.csv", header=T)
str(ancil_dat_aquatic)

ancil_dat_riparian <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Campaign1_raw_data/Ancillary_data/Campaign1_dataentry - Riparian 2021.04.19.csv", header=T)
str(ancil_dat_riparian)

#add an ID column 
ancil_dat_aquatic$ID <- paste(ancil_dat_aquatic$siteID_new,ancil_dat_aquatic$point,sep="_")
ancil_dat_riparian$ID <- paste(ancil_dat_riparian$siteID_new,ancil_dat_riparian$point,sep="_")

#combine the riparian and aquatic ancillary data
ancil_dat <- bind_rows(ancil_dat_aquatic, ancil_dat_riparian)

#add the date to the start and end time columns for 
ancil_dat$time_start<- as.POSIXct(paste(ancil_dat$date, ancil_dat$time_start), format="%Y-%m-%d %H:%M")
ancil_dat$time_end<- as.POSIXct(paste(ancil_dat$date, ancil_dat$time_end), format="%Y-%m-%d %H:%M")

#make soil temp and VWC average column
ancil_dat$soil_temp <- rowMeans(ancil_dat[,c('soil_temp1', 'soil_temp2', 'soil_temp3')], na.rm=TRUE)
ancil_dat$VWC <- rowMeans(ancil_dat[,c('VWC_1', 'VWC_2', 'VWC_3')], na.rm=TRUE)

#Subset the ancillary data for only picarro data
ancil_dat<-ancil_dat[ancil_dat$Picarro_LGR=="Picarro",]

#Subset just the useful columns: Date, ID, drying_regime, soil_temp, VWC, Picarro_LGR, total_volume_L, chamber_area_m2, flowing_state, habitat_type, pool_riffle_depth_cm, time_start, time_end 
ancil_dat <- ancil_dat %>% select("date", "ID", "time_start", "time_end", "drying_regime", "soil_temp", "VWC", "Picarro_LGR", "total_volume_L", "chamber_area_m2", "flow_state", "habitat_type", "pool_riffle_depth_cm")

str(ancil_dat) #233 obs

#Since the Picarro failed on 2021-03-23 at CO01, you can delete those rows
ancil_dat <-subset(ancil_dat, ancil_dat$date != "2021-03-23" | !grepl("CO01", ancil_dat$ID))

str(ancil_dat) #189 obs

write.csv(ancil_dat, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Picarro_ancil_dat.csv")

#data cleaning : check if there are any super long or super short intervals (potentially typos) 

ancil_dat$int <- difftime(ancil_dat$time_end, ancil_dat$time_start, unit = "mins")

#########################################################################
#### Clip the data by start and end times ####
###########################################################


ID <- ancil_dat$ID
startT<-ancil_dat$time_start #start times
endT<-ancil_dat$time_end  # end times 

for(i in 1:length(startT)){
  st<-startT[i]
  se<-endT[i]
  id<-ID[i]
  data<-Picarro_March2021[Picarro_March2021$time >= st & Picarro_March2021$time <= se,]
  data$ID<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
}

Picarro_dat<-get(paste("data",length(startT),sep="_"))
str(Picarro_dat) # 72158 obs


### there are duplicated time rows in the data, delete them

Picarro_dat <- Picarro_dat  %>% 
  # Base the removal on the "epoch_time" column
  distinct(EPOCH_TIME, .keep_all = TRUE)

str(Picarro_dat) # 	72144 obs. #14


#################################################
######### DATA CLEANING #########################
#################################################

#Now based on gasflux output using the raw data we can use the r2 and visual inspection to flag any problems and modify the start and end times as needed

#first load in those unaltered outputs
CO2.results_raw <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2.results_raw.csv")

CH4.results_raw <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CH4.results_raw.csv")

#Firstly identify points with an r2 less than 0.90
#Not sure what we want to do with CO2 uptake? I think this is unlikely, so not sure what is going on 
CO2_poorfit <- CO2.results_raw[(CO2.results_raw$linear.r <= "0.90") | (CO2.results_raw$linear.r <= "-0.90"), ] 
#.99/.98 is to restrictive, as many with an r2 of .98/.97/.96/.95 have a good linear relationship, just more noisy

str(CO2_poorfit) #95 obs.
# Now manually go through these and note which ones need to be clipped earlier or later

#Load the excel sheet with this information
CO2_QC <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_fluxrate_QAQC_March2021.csv")

#Picarro_dat$flux_time <- ifelse(CO2_QC$Clip ="Start_later", CO2_QC$ID), #Picarro_dat$flux_time + 0.008 , Picarro_dat$flux_time)

#I think you need a loop here


CO2_QC_start<-CO2_QC[CO2_QC$Clip == "start_later" ,]
CO2_QC__end<-CO2_QC[CO2_QC$Clip == "end_earlier" ,]

ID <- ancil_dat$ID
clip_start<-CO2_QC_start$ID
clip_end<-CO2_QC__end$ID

for(i in 1:length(clip_start)){
  clip_st<-clip_start[i]
  id<-ID[i]
  data<-Picarro_dat[ancil_dat$time_start >= ,]
  data$ID<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
}

Picarro_dat<-get(paste("data",length(startT),sep="_"))
str(Picarro_dat) # 18156 obs.



IF ID in ancil date = (list), then add 30 scond to time_start, if not leave



ancil_dat$time_start_adj <- ifelse(charmatch(CO2_QC_start$ID, ancil_dat$ID), ancil_dat$time_start + 30, ancil_dat$time_start)


chrs <- (charmatch(as.character(CO2_QC_start$ID), as.character(ancil_dat$ID)))

#charmatch which seeks matches for the elements of its first argument among those of its second





###########################
#### now that we have the data split by start/end times, we can set t0 to 0 hrs ####
##############################


#create function to rest the min time to each time
rescale <- function(x) (x-min(x))

Picarro_dat <- setDT(Picarro_dat)[,c("flux_time"):=.(rescale(EPOCH_TIME/3600)),by=.(ID)]
str(Picarro_dat) # 71816 obs. 

## keep only the desired columns from picarro data
Picarro_dat <- subset(Picarro_dat, select = c( "CavityTemp", "CH4_dry", "CO2_dry", "ID", "flux_time"))
str(Picarro_dat) # 71816 obs.

CO2_CH4_dat <- merge (Picarro_dat, ancil_dat , by="ID")

str(CO2_CH4_dat) # 71816 obs.


################## Make flux figures ###################

ID <- unique(CO2_CH4_dat$ID)

for (i in ID) {
  CO2_plot = ggplot(data=subset(CO2_CH4_dat, ID==i)) + 
    geom_point(size=1, aes(x=flux_time, y=CO2_dry)) +
    ggtitle(i) } + ### Stop here, then run the next part
  
  
  ggsave(CO2_plot, path="C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Flux_figures", 
         file=paste0("plot_", i,".pdf"), width = 7, height = 5, units = "cm")
}
  
############################################################
#### Finding breakpoints and creating new start/end times #
############################################################

### EXAMPLE FOR RA01_B1 
RA01_B1<-subset(data_fluxCal_3,ID== "RA01_B1") #ONE MEASURE OF 1 SITE
fit0<-lm(C_CH4_ug_L ~flux_time, data=RA01_B1) #SIMPLE LINEAR MODEL
fit1 <- segmented(fit0, seg.Z = ~ flux_time, psi = list(flux_time = c(0.005, 0.08))) # PSI??? HERE IS THE ERROR WHEN RUNNING THE LOOP
summary(fit1) #SHOWS YOU THE BREAKING POINT(S)
plot(C_CH4_ug_L ~flux_time, data=RA01_B1) #BASIC PLOT
lines(RA01_B1$flux_time, predict(fit1), col = "red", lwd=5) # ADD LINES FROM SEGMENTED FIT


## Loop that almost works
#### try to create loop to include slope-breaking points
# FAIL BY NOW

library(segmented)

for (i in ID) {
  fit0<-lm(C_CO2_mg_L ~flux_time, data=subset(data_fluxCal_3,ID== i))
  fit1 <- segmented(fit0, seg.Z = ~ flux_time, psi = list(flux_time = c(0.005, 0.08)))
  CO2_plot = ggplot(data=subset(data_fluxCal_3, ID==i)) +
    geom_point(size=1, aes(x=flux_time, y=C_CO2_mg_L)) +
    ggtitle(i) +
    geom_line(data=subset(data_fluxCal_3,ID== i), aes(x=flux_time, y=predict(fit1)), col = "red", lwd=1.5) +
    
    ggsave(CO2_plot, path="C:/Users/naina/OneDrive/Escritorio/WP3_data analysis/flux_figures/lines_fit_CO2", 
           file=paste0("plot_", i,".pdf"), width = 14, height = 10, units = "cm") 
}	



## Romain's loop

install.packages("segmented")
library(segmented)

ID <- unique(Picarro_dat$ID)
for (i in ID) {
  i=1
  ###RA01_B1
  IDd<-ID[i]
  x<-subset(Picarro_dat,ID== IDd)
  fit0<-lm(CO2_dry ~time, data=x)
  fit1 <- segmented(fit0, seg.Z = ~ time, psi = 1)
  summary(fit1)
  plot(C_CH4_ug_L ~flux_time, data=RA01_B1)
  
  

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

str(CO2_CH4_dat) # 71803 obs.

write.csv(CO2_CH4_dat,"C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_CH4_dat.csv")

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


CO2.results<- gasfluxes(CO2_CH4_dat, .id = "ID", 
		.V = "total_volume_L", .A = "chamber_area_m2", 
		.times = "flux_time",.C = "CO2_mg_L", 
		methods = c("linear"))

#turn plot to F if you don't want plots 

str(CO2.results)

CH4.results <- gasfluxes(CO2_CH4_dat, .id = "ID", 
		.V = "total_volume_L", .A = "chamber_area_m2",
		.times = "flux_time",.C = "CH4_ug_L", 
		methods = c("linear"), plot=FALSE)

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

CO2_CH4_fluxes <- merge (CO2_CH4_fluxes, ancil_dat_sub, by= "ID")

write.csv (CO2_CH4_fluxes, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_CH4_flux.results.csv")


dev.off()
boxplot(CO2_mg_m2_h ~ habitat_type, data= subset(CO2_CH4_fluxes, flowing_state="flowing"))

boxplot(CO2_mg_m2_h ~ drying_regime, data= subset(CO2_CH4_fluxes, flowing_state="flowing"))
        
boxplot(CH4_ug_m2_h ~ site, data=CO2_CH4_fluxes, ylim=c(-200, 200))



