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
### Import data  25-March-2021  #   AL07 - AL06 - AL04
#################################

# RAW DATA is in different files
# load all the files

raw25March21_1<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-25_Picarro/raw25March21_1.dat", header=T)
raw25March21_2<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-25_Picarro/raw25March21_2.dat", header=T)
raw25March21_3<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-25_Picarro/raw25March21_3.dat", header=T)
raw25March21_4<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-25_Picarro/raw25March21_4.dat", header=T)
raw25March21_5<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-25_Picarro/raw25March21_5.dat", header=T)
raw25March21_6<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-25_Picarro/raw25March21_6.dat", header=T)
raw25March21_7<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-25_Picarro/raw25March21_7.dat", header=T)

# merge all the files in one file

raw25March21 <- rbind(raw25March21_1, raw25March21_2,raw25March21_3,raw25March21_4,raw25March21_5,raw25March21_6, raw25March21_7)
str(raw25March21)
head(raw25March21)


# create a column merging date and time

time<-as.POSIXct(paste(raw25March21$DATE, raw25March21$TIME), format="%Y-%m-%d %H:%M:%S")
raw25March21<-cbind(raw25March21,time)
str(raw25March21)

write.csv(raw25March21,"C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/raw25March21.csv") 

#transform data.frame into data.table

raw25March21 <- as.data.table(raw25March21)

#Select only the columns we are interested in 
# we are also keeping the column with the date-time in numerical format

raw25March21 <- raw25March21[, c(4,9,10,21,22,23)]
raw25March21 <- setNames(raw25March21, c("frac_hours", "CH4", "CO2", "CH4_dry", "CO2_dry","time"))

#select the time range of each measure
# YOU MUST QUIT 1H TO THE TIMES OF THE EXCEL FILE

#FLOWING
M1_AL07<-raw25March21[time >= "2021-03-25 8:08:00" & time <= "2021-03-25 8:13:00"]
M2_AL07<-raw25March21[time >= "2021-03-25 8:29:00" & time <= "2021-03-25 8:34:00"]
M3_AL07<-raw25March21[time >= "2021-03-25 8:35:00" & time <= "2021-03-25 8:40:00"]
M4_AL07<-raw25March21[time >= "2021-03-25 8:58:00" & time <= "2021-03-25 9:03:00"]
M5_AL07<-raw25March21[time >= "2021-03-25 9:05:00" & time <= "2021-03-25 9:10:00"]
M6_AL07<-raw25March21[time >= "2021-03-25 9:25:00" & time <= "2021-03-25 9:30:00"]

M1_AL06<-raw25March21[time >= "2021-03-25 10:36:00" & time <= "2021-03-25 10:41:00"]
M2_AL06<-raw25March21[time >= "2021-03-25 10:42:00" & time <= "2021-03-25 10:47:00"]
M3_AL06<-raw25March21[time >= "2021-03-25 11:02:00" & time <= "2021-03-25 11:07:00"]
M4_AL06<-raw25March21[time >= "2021-03-25 11:10:00" & time <= "2021-03-25 11:15:00"]
M5_AL06<-raw25March21[time >= "2021-03-25 11:32:00" & time <= "2021-03-25 11:37:00"]
M6_AL06<-raw25March21[time >= "2021-03-25 11:48:00" & time <= "2021-03-25 11:53:00"]

M1_AL04<-raw25March21[time >= "2021-03-25 13:47:00" & time <= "2021-03-25 13:52:00"]
M2_AL04<-raw25March21[time >= "2021-03-25 13:56:00" & time <= "2021-03-25 14:01:00"]
M3_AL04<-raw25March21[time >= "2021-03-25 14:27:00" & time <= "2021-03-25 14:32:00"]
M4_AL04<-raw25March21[time >= "2021-03-25 14:56:00" & time <= "2021-03-25 15:01:00"]
M5_AL04<-raw25March21[time >= "2021-03-25 15:18:00" & time <= "2021-03-25 15:23:00"]

#RIPARIAN
R1_AL07<-raw25March21[time >= "2021-03-25 8:16:00" & time <= "2021-03-25 8:21:00"]
R2_AL07<-raw25March21[time >= "2021-03-25 8:22:00" & time <= "2021-03-25 8:27:00"]
R3_AL07<-raw25March21[time >= "2021-03-25 8:42:00" & time <= "2021-03-25 8:47:00"]
R4_AL07<-raw25March21[time >= "2021-03-25 8:50:00" & time <= "2021-03-25 8:55:00"]
R5_AL07<-raw25March21[time >= "2021-03-25 9:12:00" & time <= "2021-03-25 9:17:00"]
R6_AL07<-raw25March21[time >= "2021-03-25 9:18:00" & time <= "2021-03-25 9:23:00"]

R4_AL06<-raw25March21[time >= "2021-03-25 10:26:00" & time <= "2021-03-25 10:31:00"]
R1_AL06<-raw25March21[time >= "2021-03-25 10:48:00" & time <= "2021-03-25 10:53:00"]
R2_AL06<-raw25March21[time >= "2021-03-25 10:55:00" & time <= "2021-03-25 11:00:00"]
R3_AL06<-raw25March21[time >= "2021-03-25 11:17:00" & time <= "2021-03-25 11:22:00"]
R5_AL06<-raw25March21[time >= "2021-03-25 11:25:00" & time <= "2021-03-25 11:30:00"]
R6_AL06<-raw25March21[time >= "2021-03-25 11:42:00" & time <= "2021-03-25 11:47:00"]

R1_AL04<-raw25March21[time >= "2021-03-25 13:40:00" & time <= "2021-03-25 13:45:00"]
R2_AL04<-raw25March21[time >= "2021-03-25 14:03:00" & time <= "2021-03-25 14:08:00"]
R3_AL04<-raw25March21[time >= "2021-03-25 14:21:00" & time <= "2021-03-25 14:26:00"]
R5_AL04<-raw25March21[time >= "2021-03-25 14:50:00" & time <= "2021-03-25 14:55:00"]
R6_AL04<-raw25March21[time >= "2021-03-25 15:12:00" & time <= "2021-03-25 15:17:00"]

#create an ID column for each measure

#FLOWING
M1_AL07$ID <- rep("M1_AL07",nrow(M1_AL07)) 
M2_AL07$ID <- rep("M2_AL07",nrow(M2_AL07)) 
M3_AL07$ID <- rep("M3_AL07",nrow(M3_AL07)) 
M4_AL07$ID <- rep("M4_AL07",nrow(M4_AL07)) 
M5_AL07$ID <- rep("M5_AL07",nrow(M5_AL07))
M6_AL07$ID <- rep("M6_AL07",nrow(M6_AL07))

M1_AL06$ID <- rep("M1_AL06",nrow(M1_AL06)) 
M2_AL06$ID <- rep("M2_AL06",nrow(M2_AL06)) 
M3_AL06$ID <- rep("M3_AL06",nrow(M3_AL06)) 
M4_AL06$ID <- rep("M4_AL06",nrow(M4_AL06)) 
M5_AL06$ID <- rep("M5_AL06",nrow(M5_AL06))
M6_AL06$ID <- rep("M6_AL06",nrow(M6_AL06))

M1_AL04$ID <- rep("M1_AL04",nrow(M1_AL04)) 
M2_AL04$ID <- rep("M2_AL04",nrow(M2_AL04)) 
M3_AL04$ID <- rep("M3_AL04",nrow(M3_AL04)) 
M4_AL04$ID <- rep("M4_AL04",nrow(M4_AL04)) 
M5_AL04$ID <- rep("M5_AL04",nrow(M5_AL04))

#RIPARIAN
R1_AL07$ID <- rep("R1_AL07",nrow(R1_AL07)) 
R2_AL07$ID <- rep("R2_AL07",nrow(R2_AL07)) 
R3_AL07$ID <- rep("R3_AL07",nrow(R3_AL07)) 
R4_AL07$ID <- rep("R4_AL07",nrow(R4_AL07)) 
R5_AL07$ID <- rep("R5_AL07",nrow(R5_AL07))
R6_AL07$ID <- rep("R6_AL07",nrow(R6_AL07))

R1_AL06$ID <- rep("R1_AL06",nrow(R1_AL06)) 
R2_AL06$ID <- rep("R2_AL06",nrow(R2_AL06)) 
R3_AL06$ID <- rep("R3_AL06",nrow(R3_AL06)) 
R4_AL06$ID <- rep("R4_AL06",nrow(R4_AL06)) 
R5_AL06$ID <- rep("R5_AL06",nrow(R5_AL06))
R6_AL06$ID <- rep("R6_AL06",nrow(R6_AL06))

R1_AL04$ID <- rep("R1_AL04",nrow(R1_AL04)) 
R2_AL04$ID <- rep("R2_AL04",nrow(R2_AL04)) 
R3_AL04$ID <- rep("R3_AL04",nrow(R3_AL04)) 
R5_AL04$ID <- rep("R5_AL04",nrow(R5_AL04)) 
R6_AL04$ID <- rep("R6_AL04",nrow(R6_AL04))

# merge all the data in one file

AL07 <- rbind(M1_AL07, M2_AL07, M3_AL07, M4_AL07, M5_AL07, M6_AL07, R1_AL07, R2_AL07, R3_AL07, R4_AL07, R5_AL07, R6_AL07)
AL06 <- rbind(M1_AL06, M2_AL06, M3_AL06, M4_AL06, M5_AL06, M6_AL06, R1_AL06, R2_AL06, R3_AL06, R4_AL06, R5_AL06, R6_AL06)
AL04 <- rbind(M1_AL04, M2_AL04, M3_AL04, M4_AL04, M5_AL04, R1_AL04, R2_AL04, R3_AL04, R5_AL04, R6_AL04)

March_25 <- rbind(AL07, AL06, AL04)

write.csv(March_25,"C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/March_25.csv")
 
##EXPORT DATA FILES

write.csv(AL07, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/AL07.csv")
write.csv(AL06, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/AL06.csv")
write.csv(AL04, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/AL04.csv")

#########
# PLOTS #
#########

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=M1_AL07)
plot(CO2_dry~ time, data=M2_AL07)
plot(CO2_dry~ time, data=M3_AL07)
plot(CO2_dry~ time, data=M4_AL07)
plot(CO2_dry~ time, data=M5_AL07)
plot(CO2_dry~ time, data=M6_AL07)

plot(CH4_dry~ time, data=M1_AL07)
plot(CH4_dry~ time, data=M2_AL07)
plot(CH4_dry~ time, data=M3_AL07)
plot(CH4_dry~ time, data=M4_AL07)
plot(CH4_dry~ time, data=M5_AL07)
plot(CH4_dry~ time, data=M6_AL07)

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=M1_AL06)
plot(CO2_dry~ time, data=M2_AL06)
plot(CO2_dry~ time, data=M3_AL06)
plot(CO2_dry~ time, data=M4_AL06)
plot(CO2_dry~ time, data=M5_AL06)
plot(CO2_dry~ time, data=M6_AL06)

plot(CH4_dry~ time, data=M1_AL06)
plot(CH4_dry~ time, data=M2_AL06)
plot(CH4_dry~ time, data=M3_AL06)
plot(CH4_dry~ time, data=M4_AL06)
plot(CH4_dry~ time, data=M5_AL06)
plot(CH4_dry~ time, data=M6_AL06)

par(mfrow=c(2,5))
plot(CO2_dry~ time, data=M1_AL04)
plot(CO2_dry~ time, data=M2_AL04)
plot(CO2_dry~ time, data=M3_AL04)
plot(CO2_dry~ time, data=M4_AL04)
plot(CO2_dry~ time, data=M5_AL04)

plot(CH4_dry~ time, data=M1_AL04)
plot(CH4_dry~ time, data=M2_AL04)
plot(CH4_dry~ time, data=M3_AL04)
plot(CH4_dry~ time, data=M4_AL04)
plot(CH4_dry~ time, data=M5_AL04)


#########################
#### FLUX CALCULATIONS ##
#########################

gasfluxes(M1_CA02, .id = "ID", .V = "V", .A = "A", .times = "time",
.C = "C", methods = c("linear", "robust linear", "HMR", "NDFE"),
k_HMR = log(1.5), k_NDFE = log(0.01), verbose = TRUE,
plot = TRUE, select, maxiter = 100, ...)




