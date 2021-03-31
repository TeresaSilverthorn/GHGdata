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
### Import data  16-March-2021  #  CA02 -  CA01
#################################

# RAW DATA is in different files
# load all the files

raw16March21_1<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-16_Picarro/HIDS2380-20210316-090304Z-DataLog_User_Minimal.dat", header=T)
raw16March21_2<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-16_Picarro/HIDS2380-20210316-100308Z-DataLog_User_Minimal.dat", header=T)
raw16March21_3<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-16_Picarro/HIDS2380-20210316-110312Z-DataLog_User_Minimal.dat", header=T)

raw16March21_4<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-16_Picarro/HIDS2380-20210316-122003Z-DataLog_User_Minimal.dat", header=T)

raw16March21_5<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-16_Picarro/HIDS2380-20210316-132007Z-DataLog_User_Minimal.dat", header=T)

raw16March21_6<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-16_Picarro/HIDS2380-20210316-140928Z-DataLog_User_Minimal.dat", header=T)

raw16March21_7<-read.table("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_03.2021/Picarro/2021-03-16_Picarro/HIDS2380-20210316-150932Z-DataLog_User_Minimal.dat", header=T)


# merge all the files of each date in one file

raw16March21 <- rbind(raw16March21_1, raw16March21_2,raw16March21_3,raw16March21_4,raw16March21_5,raw16March21_6,raw16March21_7)
str(raw16March21)
head(raw16March21)

# create a column merging date and time

time<-as.POSIXct(paste(raw16March21$DATE, raw16March21$TIME), format="%Y-%m-%d %H:%M:%S")
raw16March21<-cbind(raw16March21,time)
str(raw16March21)

#transform data.frame into data.table

raw16March21 <- as.data.table(raw16March21)

#Select only the columns we are interested in 
# we are also keeping the column with the date-time in numerical format

raw16March21 <- raw16March21[, c(4,9,10,21,22,23)]
raw16March21 <- setNames(raw16March21, c("frac_hours", "CH4", "CO2", "CH4_dry", "CO2_dry","time"))

#select the time range of each measure
# YOU MUST ADD 1H TO THE TIMES OF THE EXCEL FILE (or rather subtract one hour from the Picarro data files as they are in GMT time)

#FLOWING

M1_CA02<-raw16March21[time >= "2021-03-16 9:07:00" & time <= "2021-03-16 9:15:00"]
M2_CA02<-raw16March21[time >= "2021-03-16 9:20:00" & time <= "2021-03-16 9:29:00"]
M3_CA02<-raw16March21[time >= "2021-03-16 10:06:00" & time <= "2021-03-16 10:11:00"]
M4_CA02<-raw16March21[time >= "2021-03-16 11:20:00" & time <= "2021-03-16 11:25:00"]
M5_CA02<-raw16March21[time >= "2021-03-16 12:25:00" & time <= "2021-03-16 12:41:00"]

M1_CA01<-raw16March21[time >= "2021-03-16 14:13:00" & time <= "2021-03-16 14:18:00"]
M2_CA01<-raw16March21[time >= "2021-03-16 14:36:00" & time <= "2021-03-16 14:40:00"]
M3_CA01<-raw16March21[time >= "2021-03-16 14:59:00" & time <= "2021-03-16 15:05:00"]
M4_CA01<-raw16March21[time >= "2021-03-16 15:21:00" & time <= "2021-03-16 15:26:00"]
M5_CA01<-raw16March21[time >= "2021-03-16 15:50:00" & time <= "2021-03-16 15:56:00"]

# RIPARIAN

R1_CA02<-raw16March21[time >= "2021-03-16 9:31:00" & time <= "2021-03-16 9:43:00"]
R2_CA02<-raw16March21[time >= "2021-03-16 10:30:00" & time <= "2021-03-16 10:37:00"]
R3_CA02<-raw16March21[time >= "2021-03-16 11:27:00" & time <= "2021-03-16 11:33:00"]
R4_CA02<-raw16March21[time >= "2021-03-16 12:46:00" & time <= "2021-03-16 12:51:00"]
R5_CA02<-raw16March21[time >= "2021-03-16 13:21:00" & time <= "2021-03-16 13:26:00"]

R1_CA01<-raw16March21[time >= "2021-03-16 14:23:00" & time <= "2021-03-16 14:29:00"]
R2_CA01<-raw16March21[time >= "2021-03-16 14:47:00" & time <= "2021-03-16 14:52:00"]
R3_CA01<-raw16March21[time >= "2021-03-16 14:55:00" & time <= "2021-03-16 15:03:00"]
R4_CA01<-raw16March21[time >= "2021-03-16 15:30:00" & time <= "2021-03-16 15:35:00"]
R5_CA01<-raw16March21[time >= "2021-03-16 15:37:00" & time <= "2021-03-16 15:45:00"]

#create an ID column for each measure

M1_CA02$ID <- rep("M1_CA02",nrow(M1_CA02)) 
M2_CA02$ID <- rep("M2_CA02",nrow(M2_CA02)) 
M3_CA02$ID <- rep("M3_CA02",nrow(M3_CA02)) 
M4_CA02$ID <- rep("M4_CA02",nrow(M4_CA02)) 
M5_CA02$ID <- rep("M5_CA02",nrow(M5_CA02)) 

M1_CA01$ID <- rep("M1_CA01",nrow(M1_CA01)) 
M2_CA01$ID <- rep("M2_CA01",nrow(M2_CA01)) 
M3_CA01$ID <- rep("M3_CA01",nrow(M3_CA01)) 
M4_CA01$ID <- rep("M4_CA01",nrow(M4_CA01)) 
M5_CA01$ID <- rep("M5_CA01",nrow(M5_CA01)) 


R1_CA02$ID <- rep("R1_CA02",nrow(R1_CA02)) 
R2_CA02$ID <- rep("R2_CA02",nrow(R2_CA02)) 
R3_CA02$ID <- rep("R3_CA02",nrow(R3_CA02)) 
R4_CA02$ID <- rep("R4_CA02",nrow(R4_CA02)) 
R5_CA02$ID <- rep("R5_CA02",nrow(R5_CA02)) 

R1_CA01$ID <- rep("R1_CA01",nrow(R1_CA01)) 
R2_CA01$ID <- rep("R2_CA01",nrow(R2_CA01)) 
R3_CA01$ID <- rep("R3_CA01",nrow(R3_CA01)) 
R4_CA01$ID <- rep("R4_CA01",nrow(R4_CA01)) 
R5_CA01$ID <- rep("R5_CA01",nrow(R5_CA01)) 


# merge all the data in one file

CA02 <- rbind(M1_CA02, M2_CA02, M3_CA02,M4_CA02, M5_CA02,R1_CA02, R2_CA02, R3_CA02, R4_CA02, R5_CA02)
CA01 <- rbind(M1_CA01, M2_CA01, M3_CA01,M4_CA01, M5_CA01, R1_CA01, R2_CA01, R3_CA01, R4_CA01, R5_CA01)

##EXPORT DATA FILES

write.csv(CA02, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/CA02_Picarro_03.2021.csv")
write.csv(CA01, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/CA01_Picarro_3.2021.csv")

March_16 <- rbind (CA02, CA01)
write.csv (March_16,  "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/Picarro_2021.03.16.csv")


## PLOTS

par(mfrow=c(2,5))
plot(CO2_dry~ time, data=M1_CA02)
plot(CO2_dry~ time, data=M2_CA02)
plot(CO2_dry~ time, data=M3_CA02)
plot(CO2_dry~ time, data=M4_CA02)
plot(CO2_dry~ time, data=M5_CA02)


plot(CH4_dry~ time, data=M1_CA02)
plot(CH4_dry~ time, data=M2_CA02)
plot(CH4_dry~ time, data=M3_CA02)
plot(CH4_dry~ time, data=M4_CA02)
plot(CH4_dry~ time, data=M5_CA02)


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

#########################
#### FLUX CALCULATIONS ##
#########################

gasfluxes(M1_CA02, .id = "ID", .V = "V", .A = "A", .times = "time",
.C = "C", methods = c("linear", "robust linear", "HMR", "NDFE"),
k_HMR = log(1.5), k_NDFE = log(0.01), verbose = TRUE,
plot = TRUE, select, maxiter = 100, ...)




