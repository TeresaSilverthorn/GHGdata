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
### Import data  17-March-2021  #  GR01 - RA01 - BU01
#################################

# RAW DATA is in different files
# load all the files

raw17March21_1<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-17_Picarro/HIDS2380-20210317-080127Z-DataLog_User_Minimal.dat", header=T)
raw17March21_2<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-17_Picarro/HIDS2380-20210317-090132Z-DataLog_User_Minimal.dat", header=T)
raw17March21_3<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-17_Picarro/HIDS2380-20210317-105459Z-DataLog_User_Minimal.dat", header=T)
raw17March21_4<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-17_Picarro/HIDS2380-20210317-115919Z-DataLog_User_Minimal.dat", header=T)
raw17March21_5<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-17_Picarro/HIDS2380-20210317-125923Z-DataLog_User_Minimal.dat", header=T)
raw17March21_6<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-17_Picarro/HIDS2380-20210317-142830Z-DataLog_User_Minimal.dat", header=T)
raw17March21_7<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-17_Picarro/HIDS2380-20210317-152834Z-DataLog_User_Minimal.dat", header=T)

# merge all the files of each date in one file

raw17March21 <- rbind(raw17March21_1, raw17March21_2,raw17March21_3,raw17March21_4,raw17March21_5,raw17March21_6,raw17March21_7)
str(raw17March21)
head(raw17March21)

# create a column merging date and time

time<-as.POSIXct(paste(raw17March21$DATE, raw17March21$TIME), format="%Y-%m-%d %H:%M:%S")
raw17March21<-cbind(raw17March21,time)
str(raw17March21)

#transform data.frame into data.table

raw17March21 <- as.data.table(raw17March21)

#Select only the columns we are interested in 
# we are lso keeping the column with the date-time in numerical format

raw17March21 <- raw17March21[, c(4,9,10,21,22,23)]
raw17March21 <- setNames(raw17March21, c("frac_hours", "CH4", "CO2", "CH4_dry", "CO2_dry","time"))

#select the time range of each measure
# YOU MUST QUIT 1H TO THE TIMES OF THE EXCEL FILE

#FLOWING
M1_GR01<-raw17March21[time >= "2021-03-17 8:18:00" & time <= "2021-03-17 8:26:00"]
M2_GR01<-raw17March21[time >= "2021-03-17 8:37:00" & time <= "2021-03-17 8:44:00"]
M3_GR01<-raw17March21[time >= "2021-03-17 8:58:00" & time <= "2021-03-17 9:03:00"]
M4_GR01<-raw17March21[time >= "2021-03-17 9:18:00" & time <= "2021-03-17 9:23:00"]
M5_GR01<-raw17March21[time >= "2021-03-17 11:04:00" & time <= "2021-03-17 11:09:00"]
M6_GR01<-raw17March21[time >= "2021-03-17 11:09:00" & time <= "2021-03-17 11:14:00"]

M1_RA01<-raw17March21[time >= "2021-03-17 12:04:00" & time <= "2021-03-17 12:10:00"]
M2_RA01<-raw17March21[time >= "2021-03-17 12:27:00" & time <= "2021-03-17 12:32:00"]
M3_RA01<-raw17March21[time >= "2021-03-17 12:53:00" & time <= "2021-03-17 12:58:00"]
M4_RA01<-raw17March21[time >= "2021-03-17 13:09:00" & time <= "2021-03-17 13:14:00"]
M5_RA01<-raw17March21[time >= "2021-03-17 13:26:00" & time <= "2021-03-17 13:31:00"]

M1_BU01<-raw17March21[time >= "2021-03-17 14:33:00" & time <= "2021-03-17 14:38:00"]
M2_BU01<-raw17March21[time >= "2021-03-17 14:58:00" & time <= "2021-03-17 15:03:00"]
M3_BU01<-raw17March21[time >= "2021-03-17 15:06:00" & time <= "2021-03-17 15:11:00"]
M4_BU01<-raw17March21[time >= "2021-03-17 15:32:00" & time <= "2021-03-17 15:37:00"]
M5_BU01<-raw17March21[time >= "2021-03-17 15:40:00" & time <= "2021-03-17 15:45:00"]
M6_BU01<-raw17March21[time >= "2021-03-17 15:57:00" & time <= "2021-03-17 16:02:00"]

#RIPARIAN
R1_GR01<-raw17March21[time >= "2021-03-17 8:29:00" & time <= "2021-03-17 8:37:00"]
R2_GR01<-raw17March21[time >= "2021-03-17 8:49:00" & time <= "2021-03-17 8:55:00"]
R3_GR01<-raw17March21[time >= "2021-03-17 9:06:00" & time <= "2021-03-17 9:14:00"]
R4_GR01<-raw17March21[time >= "2021-03-17 9:27:00" & time <= "2021-03-17 9:36:00"]
R5_GR01<-raw17March21[time >= "2021-03-17 9:53:00" & time <= "2021-03-17 10:01:00"]
R6_GR01<-raw17March21[time >= "2021-03-17 10:19:00" & time <= "2021-03-17 10:23:00"]

R1_RA01<-raw17March21[time >= "2021-03-17 12:14:00" & time <= "2021-03-17 12:19:00"]
R2_RA01<-raw17March21[time >= "2021-03-17 12:35:00" & time <= "2021-03-17 12:40:00"]
R3_RA01<-raw17March21[time >= "2021-03-17 12:42:00" & time <= "2021-03-17 12:47:00"]
R4_RA01<-raw17March21[time >= "2021-03-17 13:00:00" & time <= "2021-03-17 13:06:00"]
R5_RA01<-raw17March21[time >= "2021-03-17 13:18:00" & time <= "2021-03-17 13:23:00"]

R1_BU01<-raw17March21[time >= "2021-03-17 14:42:00" & time <= "2021-03-17 14:47:00"]
R2_BU01<-raw17March21[time >= "2021-03-17 14:50:00" & time <= "2021-03-17 14:55:00"]
R3_BU01<-raw17March21[time >= "2021-03-17 15:13:00" & time <= "2021-03-17 15:18:00"]
R4_BU01<-raw17March21[time >= "2021-03-17 15:19:00" & time <= "2021-03-17 15:28:00"]
R5_BU01<-raw17March21[time >= "2021-03-17 15:48:00" & time <= "2021-03-17 15:53:00"]
R6_BU01<-raw17March21[time >= "2021-03-17 16:04:00" & time <= "2021-03-17 16:09:00"]


#create an ID column for each measure

M1_GR01$ID <- rep("M1_GR01",nrow(M1_GR01)) 
M2_GR01$ID <- rep("M2_GR01",nrow(M2_GR01)) 
M3_GR01$ID <- rep("M3_GR01",nrow(M3_GR01)) 
M4_GR01$ID <- rep("M4_GR01",nrow(M4_GR01)) 
M5_GR01$ID <- rep("M5_GR01",nrow(M5_GR01)) 
M6_GR01$ID <- rep("M6_GR01",nrow(M6_GR01)) 

M1_RA01$ID <- rep("M1_RA01",nrow(M1_RA01)) 
M2_RA01$ID <- rep("M2_RA01",nrow(M2_RA01)) 
M3_RA01$ID <- rep("M3_RA01",nrow(M3_RA01)) 
M4_RA01$ID <- rep("M4_RA01",nrow(M4_RA01)) 
M5_RA01$ID <- rep("M5_RA01",nrow(M5_RA01))

M1_BU01$ID <- rep("M1_BU01",nrow(M1_BU01)) 
M2_BU01$ID <- rep("M2_BU01",nrow(M2_BU01)) 
M3_BU01$ID <- rep("M3_BU01",nrow(M3_BU01)) 
M4_BU01$ID <- rep("M4_BU01",nrow(M4_BU01)) 
M5_BU01$ID <- rep("M5_BU01",nrow(M5_BU01)) 
M6_BU01$ID <- rep("M6_BU01",nrow(M6_BU01)) 

#
R1_GR01$ID <- rep("R1_GR01",nrow(R1_GR01)) 
R2_GR01$ID <- rep("R2_GR01",nrow(R2_GR01)) 
R3_GR01$ID <- rep("R3_GR01",nrow(R3_GR01)) 
R4_GR01$ID <- rep("R4_GR01",nrow(R4_GR01)) 
R5_GR01$ID <- rep("R5_GR01",nrow(R5_GR01)) 
R6_GR01$ID <- rep("R6_GR01",nrow(R6_GR01)) 

R1_RA01$ID <- rep("R1_RA01",nrow(R1_RA01)) 
R2_RA01$ID <- rep("R2_RA01",nrow(R2_RA01)) 
R3_RA01$ID <- rep("R3_RA01",nrow(R3_RA01)) 
R4_RA01$ID <- rep("R4_RA01",nrow(R4_RA01)) 
R5_RA01$ID <- rep("R5_RA01",nrow(R5_RA01))

R1_BU01$ID <- rep("R1_BU01",nrow(R1_BU01)) 
R2_BU01$ID <- rep("R2_BU01",nrow(R2_BU01)) 
R3_BU01$ID <- rep("R3_BU01",nrow(R3_BU01)) 
R4_BU01$ID <- rep("R4_BU01",nrow(R4_BU01)) 
R5_BU01$ID <- rep("R5_BU01",nrow(R5_BU01)) 
R6_BU01$ID <- rep("R6_BU01",nrow(R6_BU01)) 

# merge all the data in one file

GR01 <- rbind(M1_GR01, M2_GR01, M3_GR01, M4_GR01, M5_GR01, M6_GR01, R1_GR01, R2_GR01, R3_GR01, R4_GR01, R5_GR01, R6_GR01)
RA01 <- rbind(M1_RA01, M2_RA01, M3_RA01, M4_RA01, M5_RA01, R1_RA01, R2_RA01, R3_RA01, R4_RA01, R5_RA01)
BU01 <- rbind(M1_BU01, M2_BU01, M3_BU01, M4_BU01, M5_BU01, M6_BU01, R1_BU01, R2_BU01, R3_BU01, R4_BU01, R5_BU01, R6_BU01)

##EXPORT DATA FILES

write.csv(GR01, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/GR01.csv")
write.csv(RA01, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/RA01.csv")
write.csv(BU01, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/BU01.csv")

March_17 <- rbind(GR01, RA01, BU01)
write.csv (March_17, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/March_17.csv")


######## PLOTS

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=M1_GR01)
plot(CO2_dry~ time, data=M2_GR01)
plot(CO2_dry~ time, data=M3_GR01)
plot(CO2_dry~ time, data=M4_GR01)
plot(CO2_dry~ time, data=M5_GR01)
plot(CO2_dry~ time, data=M6_GR01)

plot(CH4_dry~ time, data=M1_GR01)
plot(CH4_dry~ time, data=M2_GR01)
plot(CH4_dry~ time, data=M3_GR01)
plot(CH4_dry~ time, data=M4_GR01)
plot(CH4_dry~ time, data=M5_GR01)
plot(CH4_dry~ time, data=M6_GR01)

par(mfrow=c(2,5))
plot(CO2_dry~ time, data=M1_RA01)
plot(CO2_dry~ time, data=M2_RA01)
plot(CO2_dry~ time, data=M3_RA01)
plot(CO2_dry~ time, data=M4_RA01)
plot(CO2_dry~ time, data=M5_RA01)

plot(CH4_dry~ time, data=M1_RA01)
plot(CH4_dry~ time, data=M2_RA01)
plot(CH4_dry~ time, data=M3_RA01)
plot(CH4_dry~ time, data=M4_RA01)
plot(CH4_dry~ time, data=M5_RA01)


par(mfrow=c(2,5))
plot(CO2_dry~ time, data=M1_CA01)
plot(CO2_dry~ time, data=M2_CA01)
plot(CO2_dry~ time, data=M3_CA01)
plot(CO2_dry~ time, data=M4_CA01)
plot(CO2_dry~ time, data=M5_CA01)


plot(CH4_dry~ time, data=M1_CA01)
plot(CH4_dry~ time, data=M2_CA01)
plot(CH4_dry~ time, data=M3_CA01)
plot(CH4_dry~ time, data=M4_CA01)
plot(CH4_dry~ time, data=M5_CA01)

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=M1_BU01)
plot(CO2_dry~ time, data=M2_BU01)
plot(CO2_dry~ time, data=M3_BU01)
plot(CO2_dry~ time, data=M4_BU01)
plot(CO2_dry~ time, data=M5_BU01)
plot(CO2_dry~ time, data=M6_BU01)

plot(CH4_dry~ time, data=M1_BU01)
plot(CH4_dry~ time, data=M2_BU01)
plot(CH4_dry~ time, data=M3_BU01)
plot(CH4_dry~ time, data=M4_BU01)
plot(CH4_dry~ time, data=M5_BU01)
plot(CH4_dry~ time, data=M6_BU01)


#########################
#### FLUX CALCULATIONS ##
#########################

gasfluxes(M1_CA02, .id = "ID", .V = "V", .A = "A", .times = "time",
.C = "C", methods = c("linear", "robust linear", "HMR", "NDFE"),
k_HMR = log(1.5), k_NDFE = log(0.01), verbose = TRUE,
plot = TRUE, select, maxiter = 100, ...)




