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
### Import data  18-March-2021  #  BR01 - BR02 - VI01
#################################

# RAW DATA is in different files
# load all the files

raw18March21_1<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-18_Picarro/raw18March21_1.dat", header=T)
raw18March21_2<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-18_Picarro/raw18March21_2.dat", header=T)
raw18March21_3<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-18_Picarro/raw18March21_3.dat", header=T)
raw18March21_4<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-18_Picarro/raw18March21_4.dat", header=T)
raw18March21_5<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-18_Picarro/raw18March21_5.dat", header=T)
raw18March21_6<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-18_Picarro/raw18March21_6.dat", header=T)
raw18March21_7<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-18_Picarro/raw18March21_7.dat", header=T)

# merge all the files of each date in one file

raw18March21 <- rbind(raw18March21_1, raw18March21_2,raw18March21_3,raw18March21_4,raw18March21_5,raw18March21_6,raw18March21_7)
str(raw18March21)
head(raw18March21)

# create a column merging date and time

time<-as.POSIXct(paste(raw18March21$DATE, raw18March21$TIME), format="%Y-%m-%d %H:%M:%S")
raw18March21<-cbind(raw18March21,time)
str(raw18March21)

#transform data.frame into data.table

raw18March21 <- as.data.table(raw18March21)

#Select only the columns we are interested in 
# we are lso keeping the column with the date-time in numerical format

raw18March21 <- raw18March21[, c(4,9,10,21,22,23)]
raw18March21 <- setNames(raw18March21, c("frac_hours", "CH4", "CO2", "CH4_dry", "CO2_dry","time"))

#select the time range of each measure
# YOU MUST QUIT 1H TO THE TIMES OF THE EXCEL FILE

#FLOWING
M1_BR01<-raw18March21[time >= "2021-03-18 8:05:00" & time <= "2021-03-18 8:10:00"]
M2_BR01<-raw18March21[time >= "2021-03-18 8:26:00" & time <= "2021-03-18 8:31:00"]
M3_BR01<-raw18March21[time >= "2021-03-18 8:33:00" & time <= "2021-03-18 8:37:00"]
M4_BR01<-raw18March21[time >= "2021-03-18 8:49:00" & time <= "2021-03-18 8:54:00"]
M5_BR01<-raw18March21[time >= "2021-03-18 9:01:00" & time <= "2021-03-18 9:06:00"]
M6_BR01<-raw18March21[time >= "2021-03-18 9:15:00" & time <= "2021-03-18 9:20:00"]

M1_BR02<-raw18March21[time >= "2021-03-18 10:14:00" & time <= "2021-03-18 10:19:00"]
M2_BR02<-raw18March21[time >= "2021-03-18 10:21:00" & time <= "2021-03-18 10:26:00"]
M3_BR02<-raw18March21[time >= "2021-03-18 10:43:00" & time <= "2021-03-18 10:47:00"]
M4_BR02<-raw18March21[time >= "2021-03-18 10:52:00" & time <= "2021-03-18 10:57:00"]
M5_BR02<-raw18March21[time >= "2021-03-18 11:10:00" & time <= "2021-03-18 11:15:00"]

M1_VI01<-raw18March21[time >= "2021-03-18 14:09:00" & time <= "2021-03-18 14:14:00"]
M2_VI01<-raw18March21[time >= "2021-03-18 14:39:00" & time <= "2021-03-18 14:44:00"]
M3_VI01<-raw18March21[time >= "2021-03-18 14:49:00" & time <= "2021-03-18 14:54:00"]
M4_VI01<-raw18March21[time >= "2021-03-18 15:24:00" & time <= "2021-03-18 15:29:00"]
M5_VI01<-raw18March21[time >= "2021-03-18 15:33:00" & time <= "2021-03-18 15:38:00"]

#RIPARIAN
R1_BR01<-raw18March21[time >= "2021-03-18 8:012:00" & time <= "2021-03-18 8:18:00"]
R2_BR01<-raw18March21[time >= "2021-03-18 8:19:00" & time <= "2021-03-18 8:25:00"]
R3_BR01<-raw18March21[time >= "2021-03-18 8:38:00" & time <= "2021-03-18 8:43:00"]
R4_BR01<-raw18March21[time >= "2021-03-18 8:43:00" & time <= "2021-03-18 8:48:00"]
R5_BR01<-raw18March21[time >= "2021-03-18 8:54:00" & time <= "2021-03-18 8:59:00"]
R6_BR01<-raw18March21[time >= "2021-03-18 9:06:00" & time <= "2021-03-18 10:11:00"]

R1_BR02<-raw18March21[time >= "2021-03-18 10:08:00" & time <= "2021-03-18 10:13:00"]
R2_BR02<-raw18March21[time >= "2021-03-18 10:27:00" & time <= "2021-03-18 10:32:00"]
R3_BR02<-raw18March21[time >= "2021-03-18 10:34:00" & time <= "2021-03-18 10:39:00"]
R4_BR02<-raw18March21[time >= "2021-03-18 10:58:00" & time <= "2021-03-18 11:03:00"]
R5_BR02<-raw18March21[time >= "2021-03-18 11:04:00" & time <= "2021-03-18 11:09:00"]

R1_VI01<-raw18March21[time >= "2021-03-18 14:16:00" & time <= "2021-03-18 14:21:00"]
R2_VI01<-raw18March21[time >= "2021-03-18 14:30:00" & time <= "2021-03-18 14:35:00"]
R3_VI01<-raw18March21[time >= "2021-03-18 15:00:00" & time <= "2021-03-18 15:05:00"]
R4_VI01<-raw18March21[time >= "2021-03-18 15:13:00" & time <= "2021-03-18 15:19:00"]
R5_VI01<-raw18March21[time >= "2021-03-18 15:40:00" & time <= "2021-03-18 15:45:00"]


#create an ID column for each measure

M1_BR01$ID <- rep("M1_BR01",nrow(M1_BR01)) 
M2_BR01$ID <- rep("M2_BR01",nrow(M2_BR01)) 
M3_BR01$ID <- rep("M3_BR01",nrow(M3_BR01)) 
M4_BR01$ID <- rep("M4_BR01",nrow(M4_BR01)) 
M5_BR01$ID <- rep("M5_BR01",nrow(M5_BR01)) 
M6_BR01$ID <- rep("M6_BR01",nrow(M6_BR01)) 

M1_BR02$ID <- rep("M1_BR02",nrow(M1_BR02)) 
M2_BR02$ID <- rep("M2_BR02",nrow(M2_BR02)) 
M3_BR02$ID <- rep("M3_BR02",nrow(M3_BR02)) 
M4_BR02$ID <- rep("M4_BR02",nrow(M4_BR02)) 
M5_BR02$ID <- rep("M5_BR02",nrow(M5_BR02))

M1_VI01$ID <- rep("M1_VI01",nrow(M1_VI01)) 
M2_VI01$ID <- rep("M2_VI01",nrow(M2_VI01)) 
M3_VI01$ID <- rep("M3_VI01",nrow(M3_VI01)) 
M4_VI01$ID <- rep("M4_VI01",nrow(M4_VI01)) 
M5_VI01$ID <- rep("M5_VI01",nrow(M5_VI01)) 

R1_BR01$ID <- rep("R1_BR01",nrow(R1_BR01)) 
R2_BR01$ID <- rep("R2_BR01",nrow(R2_BR01)) 
R3_BR01$ID <- rep("R3_BR01",nrow(R3_BR01)) 
R4_BR01$ID <- rep("R4_BR01",nrow(R4_BR01)) 
R5_BR01$ID <- rep("R5_BR01",nrow(R5_BR01)) 
R6_BR01$ID <- rep("R6_BR01",nrow(R6_BR01)) 

R1_BR02$ID <- rep("R1_BR02",nrow(R1_BR02)) 
R2_BR02$ID <- rep("R2_BR02",nrow(R2_BR02)) 
R3_BR02$ID <- rep("R3_BR02",nrow(R3_BR02)) 
R4_BR02$ID <- rep("R4_BR02",nrow(R4_BR02)) 
R5_BR02$ID <- rep("R5_BR02",nrow(R5_BR02))

R1_VI01$ID <- rep("R1_VI01",nrow(R1_VI01)) 
R2_VI01$ID <- rep("R2_VI01",nrow(R2_VI01)) 
R3_VI01$ID <- rep("R3_VI01",nrow(R3_VI01)) 
R4_VI01$ID <- rep("R4_VI01",nrow(R4_VI01)) 
R5_VI01$ID <- rep("R5_VI01",nrow(R5_VI01)) 

# merge all the data in one file

BR01 <- rbind(M1_BR01, M2_BR01, M3_BR01, M4_BR01, M5_BR01, M6_BR01, R1_BR01, R2_BR01,R3_BR01, R4_BR01,R5_BR01,R6_BR01)
BR02 <- rbind(M1_BR02, M2_BR02, M3_BR02, M4_BR02, M5_BR02, R1_BR02, R2_BR02, R3_BR02, R4_BR02, R5_BR02)
VI01 <- rbind(M1_VI01, M2_VI01, M3_VI01, M4_VI01, M5_VI01, R1_VI01, R2_VI01, R3_VI01, R4_VI01, R5_VI01)


##EXPORT DATA FILES

write.csv(BR01, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/BR01.csv")
write.csv(BR02, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/BR02.csv")
write.csv(VI01, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/VI01.csv")

March_18 <- rbind (BR01, BR02, VI01)
write.csv (March_18, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/March_18.csv")


############
## plot

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=M1_BR01)
plot(CO2_dry~ time, data=M2_BR01)
plot(CO2_dry~ time, data=M3_BR01)
plot(CO2_dry~ time, data=M4_BR01)
plot(CO2_dry~ time, data=M5_BR01)
plot(CO2_dry~ time, data=M6_BR01)

plot(CH4_dry~ time, data=M1_BR01)
plot(CH4_dry~ time, data=M2_BR01)
plot(CH4_dry~ time, data=M3_BR01)
plot(CH4_dry~ time, data=M4_BR01)
plot(CH4_dry~ time, data=M5_BR01)
plot(CH4_dry~ time, data=M6_BR01)

par(mfrow=c(2,5))
plot(CO2_dry~ time, data=M1_BR02)
plot(CO2_dry~ time, data=M2_BR02)
plot(CO2_dry~ time, data=M3_BR02)
plot(CO2_dry~ time, data=M4_BR02)
plot(CO2_dry~ time, data=M5_BR02)

plot(CH4_dry~ time, data=M1_BR02)
plot(CH4_dry~ time, data=M2_BR02)
plot(CH4_dry~ time, data=M3_BR02)
plot(CH4_dry~ time, data=M4_BR02)
plot(CH4_dry~ time, data=M5_BR02)

par(mfrow=c(2,5))
plot(CO2_dry~ time, data=M1_VI01)
plot(CO2_dry~ time, data=M2_VI01)
plot(CO2_dry~ time, data=M3_VI01)
plot(CO2_dry~ time, data=M4_VI01)
plot(CO2_dry~ time, data=M5_VI01)

plot(CH4_dry~ time, data=M1_VI01)
plot(CH4_dry~ time, data=M2_VI01)
plot(CH4_dry~ time, data=M3_VI01)
plot(CH4_dry~ time, data=M4_VI01)
plot(CH4_dry~ time, data=M5_VI01)

#########################
#### FLUX CALCULATIONS ##
#########################

gasfluxes(M1_CA02, .id = "ID", .V = "V", .A = "A", .times = "time",
.C = "C", methods = c("linear", "robust linear", "HMR", "NDFE"),
k_HMR = log(1.5), k_NDFE = log(0.01), verbose = TRUE,
plot = TRUE, select, maxiter = 100, ...)




