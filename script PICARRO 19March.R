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
### Import data  19-March-2021  #  AL03 - ME01
#################################

# RAW DATA is in different files
# load all the files

raw19March21_1<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-19_Picarro/raw19March21_1.dat", header=T)
raw19March21_2<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-19_Picarro/raw19March21_2.dat", header=T)
raw19March21_3<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-19_Picarro/raw19March21_3.dat", header=T)
raw19March21_4<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-19_Picarro/raw19March21_4.dat", header=T)
raw19March21_5<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-19_Picarro/raw19March21_5.dat", header=T)
raw19March21_6<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-19_Picarro/raw19March21_6.dat", header=T)

head(raw19March21_3)
# merge all the files of each date in one file

raw19March21 <- rbind(raw19March21_1, raw19March21_2,raw19March21_3,raw19March21_4,raw19March21_5,raw19March21_6)
str(raw19March21)
head(raw19March21)

# create a column merging date and time

time<-as.POSIXct(paste(raw19March21$DATE, raw19March21$TIME), format="%Y-%m-%d %H:%M:%S")
raw19March21<-cbind(raw19March21,time)
str(raw19March21)

#transform data.frame into data.table

raw19March21 <- as.data.table(raw19March21)

#Select only the columns we are interested in 
# we are lso keeping the column with the date-time in numerical format

raw19March21 <- raw19March21[, c(4,9,10,21,22,23)]
raw19March21 <- setNames(raw19March21, c("frac_hours", "CH4", "CO2", "CH4_dry", "CO2_dry","time"))

#select the time range of each measure
# YOU MUST QUIT 1H TO THE TIMES OF THE EXCEL FILE

#FLOWING
M2_AL03<-raw19March21[time >= "2021-03-19 8:43:00" & time <= "2021-03-19 8:48:00"]
M1_AL03<-raw19March21[time >= "2021-03-19 8:56:00" & time <= "2021-03-19 9:01:00"]
M3_AL03<-raw19March21[time >= "2021-03-19 9:26:00" & time <= "2021-03-19 9:31:00"]
M4_AL03<-raw19March21[time >= "2021-03-19 9:43:00" & time <= "2021-03-19 9:48:00"]
M5_AL03<-raw19March21[time >= "2021-03-19 10:06:00" & time <= "2021-03-19 10:11:00"]
M6_AL03<-raw19March21[time >= "2021-03-19 10:14:00" & time <= "2021-03-19 10:21:00"]

M1_ME01<-raw19March21[time >= "2021-03-19 13:10:00" & time <= "2021-03-19 13:15:00"]
M2_ME01<-raw19March21[time >= "2021-03-19 13:17:00" & time <= "2021-03-19 13:23:00"]
M3_ME01<-raw19March21[time >= "2021-03-19 13:45:00" & time <= "2021-03-19 13:50:00"]
M4_ME01<-raw19March21[time >= "2021-03-19 13:52:00" & time <= "2021-03-19 13:57:00"]
M5_ME01<-raw19March21[time >= "2021-03-19 14:20:00" & time <= "2021-03-19 14:25:00"]

#RIPARIAN
R2_AL03<-raw19March21[time >= "2021-03-19 8:36:00" & time <= "2021-03-19 8:42:00"]
R1_AL03<-raw19March21[time >= "2021-03-19 9:04:00" & time <= "2021-03-19 9:09:00"]
R3_AL03<-raw19March21[time >= "2021-03-19 9:19:00" & time <= "2021-03-19 9:24:00"]
R4_AL03<-raw19March21[time >= "2021-03-19 9:35:00" & time <= "2021-03-19 9:40:00"]
R5_AL03<-raw19March21[time >= "2021-03-19 9:57:00" & time <= "2021-03-19 10:02:00"]
R6_AL03<-raw19March21[time >= "2021-03-19 10:24:00" & time <= "2021-03-19 10:30:00"]

R1_ME01<-raw19March21[time >= "2021-03-19 13:03:00" & time <= "2021-03-19 13:08:00"]
R2_ME01<-raw19March21[time >= "2021-03-19 13:26:00" & time <= "2021-03-19 13:31:00"]
R3_ME01<-raw19March21[time >= "2021-03-19 13:38:00" & time <= "2021-03-19 13:43:00"]
R4_ME01<-raw19March21[time >= "2021-03-19 13:58:00" & time <= "2021-03-19 14:03:00"]
R5_ME01<-raw19March21[time >= "2021-03-19 14:12:00" & time <= "2021-03-19 14:17:00"]


#create an ID column for each measure

M1_AL03$ID <- rep("M1_AL03",nrow(M1_AL03)) 
M2_AL03$ID <- rep("M2_AL03",nrow(M2_AL03)) 
M3_AL03$ID <- rep("M3_AL03",nrow(M3_AL03)) 
M4_AL03$ID <- rep("M4_AL03",nrow(M4_AL03)) 
M5_AL03$ID <- rep("M5_AL03",nrow(M5_AL03)) 
M6_AL03$ID <- rep("M6_AL03",nrow(M6_AL03)) 

M1_ME01$ID <- rep("M1_ME01",nrow(M1_ME01)) 
M2_ME01$ID <- rep("M2_ME01",nrow(M2_ME01)) 
M3_ME01$ID <- rep("M3_ME01",nrow(M3_ME01)) 
M4_ME01$ID <- rep("M4_ME01",nrow(M4_ME01)) 
M5_ME01$ID <- rep("M5_ME01",nrow(M5_ME01))

R1_AL03$ID <- rep("R1_AL03",nrow(R1_AL03)) 
R2_AL03$ID <- rep("R2_AL03",nrow(R2_AL03)) 
R3_AL03$ID <- rep("R3_AL03",nrow(R3_AL03)) 
R4_AL03$ID <- rep("R4_AL03",nrow(R4_AL03)) 
R5_AL03$ID <- rep("R5_AL03",nrow(R5_AL03)) 
R6_AL03$ID <- rep("R6_AL03",nrow(R6_AL03)) 

R1_ME01$ID <- rep("R1_ME01",nrow(R1_ME01)) 
R2_ME01$ID <- rep("R2_ME01",nrow(R2_ME01)) 
R3_ME01$ID <- rep("R3_ME01",nrow(R3_ME01)) 
R4_ME01$ID <- rep("R4_ME01",nrow(R4_ME01)) 
R5_ME01$ID <- rep("R5_ME01",nrow(R5_ME01))

# merge all the FLOWING data in one file

AL03 <- rbind(M1_AL03, M2_AL03, M3_AL03, M4_AL03, M5_AL03, M6_AL03,R1_AL03, R2_AL03, R3_AL03, R4_AL03, R5_AL03, R6_AL03)
ME01 <- rbind(M1_ME01, M2_ME01, M3_ME01, M4_ME01, M5_ME01, R1_ME01, R2_ME01, R3_ME01, R4_ME01, R5_ME01)

##EXPORT DATA FILES

write.csv(AL03, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/AL03.csv")
write.csv(ME01, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/ME01.csv")

March_19<- rbind (AL03, ME01)
write.csv (March_19, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/March_19.csv")


############
## plot FLOWING

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=M1_AL03)
plot(CO2_dry~ time, data=M2_AL03)
plot(CO2_dry~ time, data=M3_AL03)
plot(CO2_dry~ time, data=M4_AL03)
plot(CO2_dry~ time, data=M5_AL03)
plot(CO2_dry~ time, data=M6_AL03)

plot(CH4_dry~ time, data=M1_AL03)
plot(CH4_dry~ time, data=M2_AL03)
plot(CH4_dry~ time, data=M3_AL03)
plot(CH4_dry~ time, data=M4_AL03)
plot(CH4_dry~ time, data=M5_AL03)
plot(CH4_dry~ time, data=M6_AL03)

par(mfrow=c(2,5))
plot(CO2_dry~ time, data=M1_ME01)
plot(CO2_dry~ time, data=M2_ME01)
plot(CO2_dry~ time, data=M3_ME01)
plot(CO2_dry~ time, data=M4_ME01)
plot(CO2_dry~ time, data=M5_ME01)

plot(CH4_dry~ time, data=M1_ME01)
plot(CH4_dry~ time, data=M2_ME01)
plot(CH4_dry~ time, data=M3_ME01)
plot(CH4_dry~ time, data=M4_ME01)
plot(CH4_dry~ time, data=M5_ME01)


############
## plot RIPARIAN

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=R1_AL03)
plot(CO2_dry~ time, data=R2_AL03)
plot(CO2_dry~ time, data=R3_AL03)
plot(CO2_dry~ time, data=R4_AL03)
plot(CO2_dry~ time, data=R5_AL03)
plot(CO2_dry~ time, data=R6_AL03)

plot(CH4_dry~ time, data=R1_AL03)
plot(CH4_dry~ time, data=R2_AL03)
plot(CH4_dry~ time, data=R3_AL03)
plot(CH4_dry~ time, data=R4_AL03)
plot(CH4_dry~ time, data=R5_AL03)
plot(CH4_dry~ time, data=R6_AL03)

par(mfrow=c(2,5))
plot(CO2_dry~ time, data=R1_ME01)
plot(CO2_dry~ time, data=R2_ME01)
plot(CO2_dry~ time, data=R3_ME01)
plot(CO2_dry~ time, data=R4_ME01)
plot(CO2_dry~ time, data=R5_ME01)

plot(CH4_dry~ time, data=R1_ME01)
plot(CH4_dry~ time, data=R2_ME01)
plot(CH4_dry~ time, data=R3_ME01)
plot(CH4_dry~ time, data=R4_ME01)
plot(CH4_dry~ time, data=R5_ME01)

#########################
#### FLUX CALCULATIONS ##
#########################

gasfluxes(DATA, .id = "ID", .V = "V", .A = "A", .times = "time",
.C = "C", methods = c("linear", "robust linear", "HMR", "NDFE"),
k_HMR = log(1.5), k_NDFE = log(0.01), verbose = TRUE,
plot = TRUE, select, maxiter = 100, ...)




