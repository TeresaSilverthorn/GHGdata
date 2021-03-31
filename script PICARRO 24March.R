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
### Import data  24-March-2021  #  CO01 (REPETITION) - AL01 - BU02
#################################

# RAW DATA is in different files
# load all the files

raw24March21_1<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-24_Picarro/raw24March21_1.dat", header=T)
raw24March21_2<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-24_Picarro/raw24March21_2.dat", header=T)
raw24March21_3<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-24_Picarro/raw24March21_3.dat", header=T)
raw24March21_4<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-24_Picarro/raw24March21_4.dat", header=T)
raw24March21_5<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-24_Picarro/raw24March21_5.dat", header=T)
raw24March21_6<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-24_Picarro/raw24March21_6.dat", header=T)
raw24March21_7<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-24_Picarro/raw24March21_7.dat", header=T)

# merge all the files in one file

raw24March21 <- rbind(raw24March21_1, raw24March21_2,raw24March21_3,raw24March21_4,raw24March21_5,raw24March21_6, raw24March21_7)
str(raw24March21)
head(raw24March21)



# create a column merging date and time

time<-as.POSIXct(paste(raw24March21$DATE, raw24March21$TIME), format="%Y-%m-%d %H:%M:%S")
raw24March21<-cbind(raw24March21,time)
str(raw24March21)

write.csv(raw24March21,"C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/raw24March21.csv") 

#transform data.frame into data.table

raw24March21 <- as.data.table(raw24March21)

#Select only the columns we are interested in 
# we are lso keeping the column with the date-time in numerical format

raw24March21 <- raw24March21[, c(4,9,10,21,22,23)]
raw24March21 <- setNames(raw24March21, c("frac_hours", "CH4", "CO2", "CH4_dry", "CO2_dry","time"))

#select the time range of each measure
# YOU MUST QUIT 1H TO THE TIMES OF THE EXCEL FILE

#FLOWING
M1_CO01<-raw24March21[time >= "2021-03-24 7:44:00" & time <= "2021-03-24 7:49:00"]
M2_CO01<-raw24March21[time >= "2021-03-24 7:49:00" & time <= "2021-03-24 7:54:00"]
M3_CO01<-raw24March21[time >= "2021-03-24 8:00:00" & time <= "2021-03-24 8:05:00"]
M4_CO01<-raw24March21[time >= "2021-03-24 8:06:00" & time <= "2021-03-24 8:11:00"]
M5_CO01<-raw24March21[time >= "2021-03-24 8:26:00" & time <= "2021-03-24 8:31:00"]

M1_AL01<-raw24March21[time >= "2021-03-24 9:41:00" & time <= "2021-03-24 9:46:00"]
M2_AL01<-raw24March21[time >= "2021-03-24 10:08:00" & time <= "2021-03-24 10:13:00"]
M3_AL01<-raw24March21[time >= "2021-03-24 10:25:00" & time <= "2021-03-24 10:30:00"]
M4_AL01<-raw24March21[time >= "2021-03-24 10:48:00" & time <= "2021-03-24 10:53:00"]
M5_AL01<-raw24March21[time >= "2021-03-24 10:56:00" & time <= "2021-03-24 11:01:00"]

#problem with the picarro, not complete measures for M1_BU02 and M2_BU02
M1_BU02<-raw24March21[time >= "2021-03-24 14:22:00" & time <= "2021-03-24 14:27:00"]
M2_BU02<-raw24March21[time >= "2021-03-24 14:29:00" & time <= "2021-03-24 14:34:00"]
M3_BU02<-raw24March21[time >= "2021-03-24 14:49:00" & time <= "2021-03-24 14:54:00"]
M4_BU02<-raw24March21[time >= "2021-03-24 15:02:00" & time <= "2021-03-24 15:07:00"]
M5_BU02<-raw24March21[time >= "2021-03-24 15:20:00" & time <= "2021-03-24 15:25:00"]
M6_BU02<-raw24March21[time >= "2021-03-24 15:27:00" & time <= "2021-03-24 15:32:00"]

#RIPARIAN
R1_CO01<-raw24March21[time >= "2021-03-24 7:38:00" & time <= "2021-03-24 7:43:00"]
R2_CO01<-raw24March21[time >= "2021-03-24 7:55:00" & time <= "2021-03-24 8:00:00"]
R3_CO01<-raw24March21[time >= "2021-03-24 8:11:00" & time <= "2021-03-24 8:16:00"]
R4_CO01<-raw24March21[time >= "2021-03-24 8:16:00" & time <= "2021-03-24 8:21:00"]
R5_CO01<-raw24March21[time >= "2021-03-24 8:21:00" & time <= "2021-03-24 8:26:00"]

R1_AL01<-raw24March21[time >= "2021-03-24 9:48:00" & time <= "2021-03-24 9:53:00"]
R2_AL01<-raw24March21[time >= "2021-03-24 10:00:00" & time <= "2021-03-24 10:05:00"]
R3_AL01<-raw24March21[time >= "2021-03-24 10:25:00" & time <= "2021-03-24 10:30:00"]
R5_AL01<-raw24March21[time >= "2021-03-24 10:41:00" & time <= "2021-03-24 10:46:00"]
R6_AL01<-raw24March21[time >= "2021-03-24 11:04:00" & time <= "2021-03-24 11:09:00"]

R1_BU02<-raw24March21[time >= "2021-03-24 14:15:00" & time <= "2021-03-24 14:20:00"]
R2_BU02<-raw24March21[time >= "2021-03-24 14:35:00" & time <= "2021-03-24 14:40:00"]
R3_BU02<-raw24March21[time >= "2021-03-24 14:43:00" & time <= "2021-03-24 14:48:00"]
R4_BU02<-raw24March21[time >= "2021-03-24 14:56:00" & time <= "2021-03-24 15:01:00"]
R5_BU02<-raw24March21[time >= "2021-03-24 15:14:00" & time <= "2021-03-24 15:19:00"]
R6_BU02<-raw24March21[time >= "2021-03-24 15:34:00" & time <= "2021-03-24 15:39:00"]


#create an ID column for each measure

#FLOWING
M1_CO01$ID <- rep("M1_CO01",nrow(M1_CO01)) 
M2_CO01$ID <- rep("M2_CO01",nrow(M2_CO01)) 
M3_CO01$ID <- rep("M3_CO01",nrow(M3_CO01)) 
M4_CO01$ID <- rep("M4_CO01",nrow(M4_CO01)) 
M5_CO01$ID <- rep("M5_CO01",nrow(M5_CO01)) 

M1_AL01$ID <- rep("M1_AL01",nrow(M1_AL01)) 
M2_AL01$ID <- rep("M2_AL01",nrow(M2_AL01)) 
M3_AL01$ID <- rep("M3_AL01",nrow(M3_AL01)) 
M4_AL01$ID <- rep("M4_AL01",nrow(M4_AL01)) 
M5_AL01$ID <- rep("M5_AL01",nrow(M5_AL01)) 

M1_BU02$ID <- rep("M1_BU02",nrow(M1_BU02)) 
M2_BU02$ID <- rep("M2_BU02",nrow(M2_BU02)) 
M3_BU02$ID <- rep("M3_BU02",nrow(M3_BU02)) 
M4_BU02$ID <- rep("M4_BU02",nrow(M4_BU02)) 
M5_BU02$ID <- rep("M5_BU02",nrow(M5_BU02)) 
M6_BU02$ID <- rep("M6_BU02",nrow(M6_BU02)) 

#RIPARIAN
R1_CO01$ID <- rep("R1_CO01",nrow(R1_CO01)) 
R2_CO01$ID <- rep("R2_CO01",nrow(R2_CO01)) 
R3_CO01$ID <- rep("R3_CO01",nrow(R3_CO01)) 
R4_CO01$ID <- rep("R4_CO01",nrow(R4_CO01)) 
R5_CO01$ID <- rep("R5_CO01",nrow(R5_CO01)) 

R1_AL01$ID <- rep("R1_AL01",nrow(R1_AL01)) 
R2_AL01$ID <- rep("R2_AL01",nrow(R2_AL01)) 
R3_AL01$ID <- rep("R3_AL01",nrow(R3_AL01)) 
R5_AL01$ID <- rep("R5_AL01",nrow(R5_AL01)) 
R6_AL01$ID <- rep("R6_AL01",nrow(R6_AL01)) 

R1_BU02$ID <- rep("R1_BU02",nrow(R1_BU02)) 
R2_BU02$ID <- rep("R2_BU02",nrow(R2_BU02)) 
R3_BU02$ID <- rep("R3_BU02",nrow(R3_BU02)) 
R4_BU02$ID <- rep("R4_BU02",nrow(R4_BU02)) 
R5_BU02$ID <- rep("R5_BU02",nrow(R5_BU02)) 
R6_BU02$ID <- rep("R6_BU02",nrow(R6_BU02)) 


# merge all the data in one file

CO01 <- rbind(M1_CO01, M2_CO01, M3_CO01, M4_CO01, M5_CO01, R1_CO01, R2_CO01, R3_CO01, R4_CO01, R5_CO01)
AL01 <- rbind(M1_AL01, M2_CO01, M3_AL01, M4_AL01, M5_AL01, R1_AL01, R2_AL01, R3_AL01, R5_AL01, R6_AL01)
BU02 <- rbind(M1_BU02, M2_BU02, M3_BU02, M4_BU02, M5_BU02, M6_BU02, R1_BU02, R2_BU02, R3_BU02, R4_BU02, R5_BU02, R6_BU02)

##EXPORT DATA FILES

write.csv(CO01, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/CO01.csv")
write.csv(AL01, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/AL01.csv")
write.csv(BU02, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/BU02.csv")

March_24 <- rbind(CO01, AL01, BU02)
write.csv(March_24,"C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/March_24.csv")

#########
# PLOTS #
#########

par(mfrow=c(2,5))
plot(CO2_dry~ time, data=M1_CO01)
plot(CO2_dry~ time, data=M2_CO01)
plot(CO2_dry~ time, data=M3_CO01)
plot(CO2_dry~ time, data=M4_CO01)
plot(CO2_dry~ time, data=M5_CO01)

plot(CH4_dry~ time, data=M1_CO01)
plot(CH4_dry~ time, data=M2_CO01)
plot(CH4_dry~ time, data=M3_CO01)
plot(CH4_dry~ time, data=M4_CO01)
plot(CH4_dry~ time, data=M5_CO01)


par(mfrow=c(2,5))
plot(CO2_dry~ time, data=M1_AL01)
plot(CO2_dry~ time, data=M2_AL01)
plot(CO2_dry~ time, data=M3_AL01)
plot(CO2_dry~ time, data=M4_AL01)
plot(CO2_dry~ time, data=M5_AL01)

plot(CH4_dry~ time, data=M1_AL01)
plot(CH4_dry~ time, data=M2_AL01)
plot(CH4_dry~ time, data=M3_AL01)
plot(CH4_dry~ time, data=M4_AL01)
plot(CH4_dry~ time, data=M5_AL01)

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=M1_BU02)
plot(CO2_dry~ time, data=M2_BU02)
plot(CO2_dry~ time, data=M3_BU02)
plot(CO2_dry~ time, data=M4_BU02)
plot(CO2_dry~ time, data=M5_BU02)
plot(CO2_dry~ time, data=M6_BU02)

plot(CH4_dry~ time, data=M1_BU02)
plot(CH4_dry~ time, data=M2_BU02)
plot(CH4_dry~ time, data=M3_BU02)
plot(CH4_dry~ time, data=M4_BU02)
plot(CH4_dry~ time, data=M5_BU02)
plot(CH4_dry~ time, data=M6_BU02)

#########################
#### FLUX CALCULATIONS ##
#########################

gasfluxes(M1_CA02, .id = "ID", .V = "V", .A = "A", .times = "time",
.C = "C", methods = c("linear", "robust linear", "HMR", "NDFE"),
k_HMR = log(1.5), k_NDFE = log(0.01), verbose = TRUE,
plot = TRUE, select, maxiter = 100, ...)




