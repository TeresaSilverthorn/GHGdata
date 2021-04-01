# Install pacakges 
library(lubridate)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(plyr)
library(gasfluxes)
library(tidyverse)
library(data.table)


#################################
### Import data  22-March-2021  #  AL02 - RO01
#################################

# RAW DATA is in different files
# load all the files

raw22March21_1<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-22_Picarro/raw22March21_1.dat", header=T)
raw22March21_2<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-22_Picarro/raw22March21_2.dat", header=T)
raw22March21_3<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-22_Picarro/raw22March21_3.dat", header=T)
raw22March21_4<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-22_Picarro/raw22March21_4.dat", header=T)
raw22March21_5<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-22_Picarro/raw22March21_5.dat", header=T)
raw22March21_6<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-22_Picarro/raw22March21_6.dat", header=T)
raw22March21_7<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-22_Picarro/raw22March21_7.dat", header=T)


# merge all the files of each date in one file

raw22March21 <- rbind(raw22March21_1, raw22March21_2,raw22March21_3,raw22March21_4,raw22March21_5,raw22March21_6, raw22March21_7)
str(raw22March21)
head(raw22March21)

# create a column merging date and time

time<-as.POSIXct(paste(raw22March21$DATE, raw22March21$TIME), format="%Y-%m-%d %H:%M:%S")
raw22March21<-cbind(raw22March21,time)
str(raw22March21)

#transform data.frame into data.table

raw22March21 <- as.data.table(raw22March21)

#Select only the columns we are interested in 
# we are lso keeping the column with the date-time in numerical format

raw22March21 <- raw22March21[, c(4,9,10,21,22,23)]
raw22March21 <- setNames(raw22March21, c("frac_hours", "CH4", "CO2", "CH4_dry", "CO2_dry","time"))

#select the time range of each measure
# YOU MUST QUIT 1H TO THE TIMES OF THE EXCEL FILE

M1_AL02<-raw22March21[time >= "2021-03-22 9:44:00" & time <= "2021-03-22 9:48:00"]
M2_AL02<-raw22March21[time >= "2021-03-22 9:55:00" & time <= "2021-03-22 10:00:00"]
M3_AL02<-raw22March21[time >= "2021-03-22 10:33:00" & time <= "2021-03-22 10:38:00"]
M4_AL02<-raw22March21[time >= "2021-03-22 10:45:00" & time <= "2021-03-22 10:50:00"]
M5_AL02<-raw22March21[time >= "2021-03-22 11:14:00" & time <= "2021-03-22 11:19:00"]
M6_AL02<-raw22March21[time >= "2021-03-22 11:21:00" & time <= "2021-03-22 11:26:00"]

M1_RO01<-raw22March21[time >= "2021-03-22 13:03:00" & time <= "2021-03-22 13:08:00"]
M2_RO01<-raw22March21[time >= "2021-03-22 13:27:00" & time <= "2021-03-22 13:32:00"]
M3_RO01<-raw22March21[time >= "2021-03-22 13:49:00" & time <= "2021-03-22 13:54:00"]
M4_RO01<-raw22March21[time >= "2021-03-22 14:07:00" & time <= "2021-03-22 14:12:00"]
M5_RO01<-raw22March21[time >= "2021-03-22 14:21:00" & time <= "2021-03-22 14:26:00"]
M6_RO01<-raw22March21[time >= "2021-03-22 14:41:00" & time <= "2021-03-22 14:46:00"]


### the same for RIPARIAN measures

R1_AL02<-raw22March21[time >= "2021-03-22 9:35:00" & time <= "2021-03-22 9:41:00"]
R2_AL02<-raw22March21[time >= "2021-03-22 10:05:00" & time <= "2021-03-22 10:10:00"]
R3_AL02<-raw22March21[time >= "2021-03-22 10:18:00" & time <= "2021-03-22 10:25:00"]
R4_AL02<-raw22March21[time >= "2021-03-22 10:51:00" & time <= "2021-03-22 10:57:00"]
R5_AL02<-raw22March21[time >= "2021-03-22 11:04:00" & time <= "2021-03-22 11:09:00"]
R6_AL02<-raw22March21[time >= "2021-03-22 11:28:00" & time <= "2021-03-22 11:33:00"]

R1_RO01<-raw22March21[time >= "2021-03-22 13:11:00" & time <= "2021-03-22 13:16:00"]
R2_RO01<-raw22March21[time >= "2021-03-22 13:19:00" & time <= "2021-03-22 13:24:00"]
R3_RO01<-raw22March21[time >= "2021-03-22 13:36:00" & time <= "2021-03-22 13:41:00"]
R4_RO01<-raw22March21[time >= "2021-03-22 13:57:00" & time <= "2021-03-22 14:02:00"]
R5_RO01<-raw22March21[time >= "2021-03-22 14:14:00" & time <= "2021-03-22 14:19:00"]
R6_RO01<-raw22March21[time >= "2021-03-22 14:28:00" & time <= "2021-03-22 14:35:00"]


#create an ID column for each measure

#FLOWING

M1_AL02$ID <- rep("M1_AL02",nrow(M1_AL02)) 
M2_AL02$ID <- rep("M2_AL03",nrow(M2_AL02)) 
M3_AL02$ID <- rep("M3_AL02",nrow(M3_AL02)) 
M4_AL02$ID <- rep("M4_AL02",nrow(M4_AL02)) 
M5_AL02$ID <- rep("M5_AL02",nrow(M5_AL02)) 
M6_AL02$ID <- rep("M6_AL02",nrow(M6_AL02)) 

M1_RO01$ID <- rep("M1_RO01",nrow(M1_RO01)) 
M2_RO01$ID <- rep("M2_RO01",nrow(M2_RO01)) 
M3_RO01$ID <- rep("M3_RO01",nrow(M3_RO01)) 
M4_RO01$ID <- rep("M4_RO01",nrow(M4_RO01)) 
M5_RO01$ID <- rep("M5_RO01",nrow(M5_RO01)) 
M6_RO01$ID <- rep("M6_RO01",nrow(M6_RO01)) 

#RIPARIAN

R1_AL02$ID <- rep("R1_AL02",nrow(R1_AL02)) 
R2_AL02$ID <- rep("R2_AL02",nrow(R2_AL02)) 
R3_AL02$ID <- rep("R3_AL02",nrow(R3_AL02)) 
R4_AL02$ID <- rep("R4_AL02",nrow(R4_AL02)) 
R5_AL02$ID <- rep("R5_AL02",nrow(R5_AL02)) 
R6_AL02$ID <- rep("R6_AL02",nrow(R6_AL02)) 

R1_RO01$ID <- rep("R1_RO01",nrow(R1_RO01)) 
R2_RO01$ID <- rep("R2_RO01",nrow(R2_RO01)) 
R3_RO01$ID <- rep("R3_RO01",nrow(R3_RO01)) 
R4_RO01$ID <- rep("R4_RO01",nrow(R4_RO01)) 
R5_RO01$ID <- rep("R5_RO01",nrow(R5_RO01)) 
R6_RO01$ID <- rep("R6_RO01",nrow(R6_RO01)) 

# merge all the data in one file

AL02 <- rbind(M1_AL02, M2_AL02, M3_AL02, M4_AL02, M5_AL02, M6_AL02, R1_AL02, R2_AL02, R3_AL02, R4_AL02, R5_AL02, R6_AL02)
RO01 <- rbind(M1_RO01, M2_RO01, M3_RO01, M4_RO01, M5_RO01, M6_RO01, R1_RO01, R2_RO01, R3_RO01, R4_RO01, R5_RO01, R6_RO01)

##EXPORT DATA FILES

write.csv(AL02, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/AL02.csv")
write.csv(RO01, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/RO01.csv")

March_22 <- rbind (AL02, RO01)
write.csv(March_22, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/March_22.csv")


#########
# PLOTS #
#########

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=M1_AL02)
plot(CO2_dry~ time, data=M2_AL02)
plot(CO2_dry~ time, data=M3_AL02)
plot(CO2_dry~ time, data=M4_AL02)
plot(CO2_dry~ time, data=M5_AL02)
plot(CO2_dry~ time, data=M6_AL02)

plot(CH4_dry~ time, data=M1_AL02)
plot(CH4_dry~ time, data=M2_AL02)
plot(CH4_dry~ time, data=M3_AL02)
plot(CH4_dry~ time, data=M4_AL02)
plot(CH4_dry~ time, data=M5_AL02)
plot(CH4_dry~ time, data=M6_AL02)

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=M1_RO01)
plot(CO2_dry~ time, data=M2_RO01)
plot(CO2_dry~ time, data=M3_RO01)
plot(CO2_dry~ time, data=M4_RO01)
plot(CO2_dry~ time, data=M5_RO01)
plot(CO2_dry~ time, data=M6_RO01)

plot(CH4_dry~ time, data=M1_RO01)
plot(CH4_dry~ time, data=M2_RO01)
plot(CH4_dry~ time, data=M3_RO01)
plot(CH4_dry~ time, data=M4_RO01)
plot(CH4_dry~ time, data=M5_RO01)
plot(CH4_dry~ time, data=M6_RO01)


### RIPARIAN

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=R1_AL02)
plot(CO2_dry~ time, data=R2_AL02)
plot(CO2_dry~ time, data=R3_AL02)
plot(CO2_dry~ time, data=R4_AL02)
plot(CO2_dry~ time, data=R5_AL02)
plot(CO2_dry~ time, data=R6_AL02)

plot(CH4_dry~ time, data=R1_AL02)
plot(CH4_dry~ time, data=R2_AL02)
plot(CH4_dry~ time, data=R3_AL02)
plot(CH4_dry~ time, data=R4_AL02)
plot(CH4_dry~ time, data=R5_AL02)
plot(CH4_dry~ time, data=R6_AL02)

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=R1_RO01)
plot(CO2_dry~ time, data=R2_RO01)
plot(CO2_dry~ time, data=R3_RO01)
plot(CO2_dry~ time, data=R4_RO01)
plot(CO2_dry~ time, data=R5_RO01)
plot(CO2_dry~ time, data=R6_RO01)

plot(CH4_dry~ time, data=R1_RO01)
plot(CH4_dry~ time, data=R2_RO01)
plot(CH4_dry~ time, data=R3_RO01)
plot(CH4_dry~ time, data=R4_RO01)
plot(CH4_dry~ time, data=R5_RO01)
plot(CH4_dry~ time, data=R6_RO01)


#########################
#### FLUX CALCULATIONS ##
#########################

gasfluxes(M1_CA02, .id = "ID", .V = "V", .A = "A", .times = "time",
.C = "C", methods = c("linear", "robust linear", "HMR", "NDFE"),
k_HMR = log(1.5), k_NDFE = log(0.01), verbose = TRUE,
plot = TRUE, select, maxiter = 100, ...)




