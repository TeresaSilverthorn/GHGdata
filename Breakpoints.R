###################################################
###   CALCULATING BREAKPOINTS IN GHG DATA   #######
###    for automation of quality control   #########

################################
## Load the necessary packages

library(tidyverse)
library(mcp)
library(dpseg)
library(tidyr)
library(dplyr)
library(purrr)
library(data.table)
library(segmented)
library(strucchange)
library(mcp)

#########################################
## reclipping the data based on dpseg  ##
#########################################


#load the data for flux calculations 
# I have already got rid of the first and last 30 secs
# the two measures with few data points are already deleted (BU02_A1, BU02_B1)

data_fluxCal_4<-read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_CH4_dat_forbreakpoints.csv", header=T) 
str(data_fluxCal_4)

# I don?t know why I have the columns X.1 and X, but it doesn?t matter

#'data.frame':   30837 obs. of  13 variables:
# $ X.1            : int  1 2 3 4 5 6 7 8 9 10 ...
# $ X              : int  1 2 3 4 5 6 7 8 9 10 ...
# $ ID             : chr  "AL01_A1" "AL01_A1" "AL01_A1" "AL01_A1" ...
# $ time           : chr  "2021-03-24 09:41:30" "2021-03-24 09:41:31" "2021-03-24 09:41:32" "2021-03-24 09:41:33" ...
# $ CavityTemp     : num  12.5 12.5 12.5 12.5 12.5 ...
# $ CH4_dry        : num  3 3 3 3 3 ...
# $ CO2_dry        : num  459 459 459 459 460 ...
# $ flux_time      : num  0 0.000278 0.00054 0.000788 0.001047 ...
# $ siteID_new     : chr  "AL01" "AL01" "AL01" "AL01" ...
# $ total_volume_L : num  3.5 3.5 3.5 3.5 3.5 ...
# $ chamber_area_m2: num  0.0452 0.0452 0.0452 0.0452 0.0452 ...
# $ C_CO2_mg_L     : num  0.235 0.235 0.235 0.235 0.236 ...
# $ C_CH4_ug_L     : num  1.54 1.54 1.54 1.54 1.54 ...


### select one measure (do it for one and then incorporate this procedure to the nested data
BR02_A1<-subset(data_fluxCal_4,ID== "BR02_A1")

#split the fit in different segments
BR02_A1_dpseg <- dpseg(x=BR02_A1$flux_time,
                       y=BR02_A1$CO2_dry,
                       jumps = FALSE, #TRUE allows disconnected segments
                       P = round(estimateP(x=BR02_A1$flux_time, y=BR02_A1$CO2_dry,4)), # break-point penalty, higher number gives shorter segments
                       minl=60) # minimum number of points (for us, 60points = 60 secs = 1min)

print(BR02_A1_dpseg)
plot(BR02_A1_dpseg)
lines(predict(BR02_A1_dpseg),lty=2, lwd=3, col="yellow")

#save the values of the segments in a data.frame
segments <- BR02_A1_dpseg$segments

#create a new data table to store the new_start and new_end points
new_flux <- data.frame(matrix(ncol = 3, nrow = 1))
colnames(new_flux) <- c('ID', 'new_start', 'new_end')

#select the start and end points of the segment with the highest slope

new_start <-segments[which.max(segments$slope),1] # 1st column (start) of the segment with max slope
new_end <- segments[which.max(segments$slope),2] # 2nd column (end) of the segment with max slope

# fill the data table with put these values in the new_flux data table
new_flux$ID <- unique(BR02_A1$ID)
new_flux$new_start <- new_start 
new_flux$new_end <- new_end 

# we can then use this new data.table to re-clip the original Picarro data with this start and end points
# we need to check if it is better to have the epoch time column instead of the flux_time (already re-escaled)
# We would need to re-escale the new clipped data again to run the gasfluxes package

#####################################
# next step: NESTED DATA         ####
#####################################

# nest data by ID

dataFlux_nested <- data_fluxCal_4 %>% 
  group_by(ID) %>% 
  nest()

# create new data frame to store the new start and end points of each ID

new_flux <- data.frame(matrix(ncol = 5, nrow = 203)) #110= number of IDs
colnames(new_flux) <- c("ID", "new_start_CO2", "new_end_CO2","new_start_CH4", "new_end_CH4")

# functions to split the fit in segments and select the data frame with the segments

#PARAMETERS OF THE DPSEG FUNCTION
# minl=60 --> minimum segment lenght = 60 points (for us, 1 point=1sec; 60=1min)
# jumps = FALSE --> continous measure, no jumps between segments
# P = indicator of goodness of the fit; higher P allows longer segments
# funtion to estimate p based on the data (some plots are more disperse than others)
# I am using EPOCH_TIME to have unique numerical values for each time

CO2_seg <- function(x) { (dpseg(x=x$EPOCH_TIME,
                                y=x$CO2_mg_L,
                                jumps = FALSE, 
                                P = estimateP(x=x$EPOCH_TIME, y=x$CO2_mg_L),
                                minl=60))$segments }

CH4_seg <- function(x) { (dpseg(x=x$EPOCH_TIME,
                                y=x$CH4_ug_L,
                                jumps = FALSE, 
                                P = estimateP(x=x$EPOCH_TIME, y=x$CH4_ug_L),
                                minl=60))$segments }

# apply functions to nested data, store the results in CO2/CH4_segments 

dataFlux_nested_segs<- dataFlux_nested %>%  
  mutate(CO2_segments  = map(data, CO2_seg)) %>% 
  mutate(CH4_segments = map(data, CH4_seg))

#####################################################################
### try to plot this segments, doesn?t work because
### I don?t know how to apply a function that needs data from 2 different vectors of the tibble
### I was trying to do this to check the segments generated by dpseg
### you can skip this part and continue to the next
#####################################################################

#function to select values for plotting CO2 segments (X and Y)

CO2_predX <- function(x) { predict(dpseg(x=x$EPOCH_TIME,
                                         y=x$CO2_mg_L,
                                         jumps = FALSE, 
                                         P = estimateP(x=x$EPOCH_TIME, y=x$CO2_mg_L),
                                         minl=60))$x }

CO2_predY <- function(x) { predict(dpseg(x=x$EPOCH_TIME,
                                         y=x$CO2_mg_L,
                                         jumps = FALSE, 
                                         P = estimateP(x=x$EPOCH_TIME, y=x$CO2_mg_L),
                                         minl=60))$y }

dataFlux_nested_segs<- dataFlux_nested %>%  
  mutate(CO2_PredX  = map(data, CO2_predX)) %>% 
  mutate(CO2_PredY = map(data, CO2_predY))

str(dataFlux_nested_segs)

# I need to join the 3 vectors: data, CO2_PredX, CO2_PredY 
# so that we can apply the ggplot_CO2 function to that vector will all info
# HOW?


#function to create CO2 concentration~time plot and add the predicted segments 
ggplot_CO2 = function (df) {ggplot( df, aes(x= EPOCH_TIME, y= C_CO2_mg_L))+ 
    geom_point (df, aes(x= EPOCH_TIME, y= C_CO2_mg_L)) + 
    geom_line(aes(x= CO2_PredX , y= CO2_PredY), size=1, linetype=2, col="red")}



#fill the data table with IDs
new_flux$ID <- unique(dataFlux_nested$ID)

#functions to select the start and end points with highest slope

start<- function(x) { 
  x[which.max(x$slope),1] } # 1st column (start) of the segment with max slope
end<- function(x) { 
  x[which.max(x$slope),2] } # 2nd column (end) of the segment with max slope


# add columns with this information to nested data
dataFlux_nested_time<- dataFlux_nested_segs %>%  
  mutate(CO2_start  = map(CO2_segments , start)) %>% 
  mutate(CH4_start  = map(CH4_segments , start)) %>% 
  mutate(CO2_end  = map(CO2_segments , end)) %>% 
  mutate(CH4_end  = map(CH4_segments , end))

# unnest the data regarding new start and ed points
# not sure if this step is neccesary 
dataFlux_unnested_time <- dataFlux_nested_time %>% 
  unnest(CO2_start) %>% 
  unnest(CH4_start) %>% 
  unnest(CO2_end) %>% 
  unnest(CH4_end)

# put this info in the new_flux data table

new_flux$new_start_CO2 <- dataFlux_unnested_time$CO2_start
new_flux$new_end_CO2 <- dataFlux_unnested_time$CO2_end
new_flux$new_start_CH4 <- dataFlux_unnested_time$CH4_start
new_flux$new_end_CH4 <- dataFlux_unnested_time$CH4_end

################################

#Now try rerunning the gasfluxes package with these new start and end times
# First need to merge new_flux and data_flux_Cal4

new_flux_full <- merge (new_flux, data_fluxCal_4, by= "ID")

#Clip data by new start and end time

#### Clip the data by start and end times ####
###########################################################
# May need to format data to epoch time format
 # ERROR WITH LOOP loop freaks out and takes a lot of time.... 

#For CO2
ID <- new_flux$ID
startT_CO2<-new_flux$new_start_CO2 #start times
endT_CO2<-new_flux$new_end_CO2   # end times 

for(i in 1:length(startT_CO2)){
  st<-startT_CO2[i]
  se<-endT_CO2[i]
  id<-ID[i]
  data<-new_flux_full[new_flux_full$EPOCH_TIME >= st & new_flux_full$EPOCH_TIME <= se,]
  data$ID<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
}

CO2_dat<-get(paste("data",length(startT_CO2),sep="_"))
str(CO2_dat)

#For CH4
ID <- new_flux$ID
startT_CH4<-new_flux$new_start_CH4 #start times
endT_CH4<-new_flux$new_end_CH4  # end times 


for(i in 1:length(startT_CH4)){
  st<-startT_CH4[i]
  se<-endT_CH4[i]
  id<-ID[i]
  data<-new_flux_full[new_flux_full$EPOCH_TIME >= st & new_flux_full$EPOCH_TIME <= se,]
  data$ID<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
}

CH4_dat<-get(paste("data",length(startT_CH4),sep="_"))
str(CO2_dat)

##############################################################
# now we have one data frame for linear segments of CO2 measures 
# and other for linear segments of CH4 measures

str(CO2_dat) #39329 obs. of  27 variables
str(CH4_dat) #53865 obs. of  27 variables

# Not sure if this is necessary, but rescale flux_time

rescale <- function(x) (x-min(x))

CO2_dat <- setDT(CO2_dat)[,c("flux_time"):=.(rescale(EPOCH_TIME/3600)),by=.(ID)]
CH4_dat <- setDT(CH4_dat)[,c("flux_time"):=.(rescale(EPOCH_TIME/3600)),by=.(ID)]

str(CO2_dat) # 
str(CH4_dat) #  

### delete duplicated time rows --not sure if this is necessary
# Base the removal on the "epoch_time" column

CO2_dat <- CO2_dat  %>% 
  distinct(EPOCH_TIME, .keep_all = TRUE) 
CH4_dat <- CH4_dat  %>% 
  distinct(EPOCH_TIME, .keep_all = TRUE) 

str(CO2_dat) # 39329 obs. of  27 variables
str(CH4_dat) # 53865 obs. of  27 variables

#Check the units of the gasfluxes package

#V = L
#A = m2
# flux time = h
# concentration of CO2 = mg/L / CH4 = ug/L

#[f0] = mg/m^2/h

mean(CO2_dat$total_volume_L) # 4.5... L
mean(CO2_dat$chamber_area_m2) # 0.045... m2
median(CO2_dat$CO2_mg_L) # ~0.23 mg/L          
mean(CO2_dat$flux_time) # 0.05h = 3 minutes

mean(CH4_dat$total_volume_L) # 4.5... L
mean(CH4_dat$chamber_area_m2) # 0.045... m2
median(CH4_dat$CH4_ug_L) # ~1.53 ug/L         
mean(CH4_dat$flux_time) # 0.05h = 3 minutes

CO2.results <- gasfluxes(CO2_dat, .id = "ID", 
                             .V = "total_volume_L", .A = "chamber_area_m2", 
                             .times = "flux_time",.C = "CO2_mg_L", 
                             methods = c("linear"), plot = TRUE)
str(CO2.results) #203 obs. of  10 variables:


CH4.results <- gasfluxes(CH4_dat, .id = "ID", 
                             .V = "total_volume_L", .A = "chamber_area_m2", 
                             .times = "flux_time",.C = "CH4_ug_L", 
                             methods = c("linear"),plot = TRUE)
str(CH4.results)
