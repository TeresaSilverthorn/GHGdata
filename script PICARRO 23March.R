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
### Import data  23-March-2021  #  JO01 - CO01 (FAIL, PICARRO TURNED OFF) -AL05
#################################

# RAW DATA is in different files
# load all the files

raw23March21_1<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-23_Picarro/raw23March21_1.dat", header=T)
raw23March21_2<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-23_Picarro/raw23March21_2.dat", header=T)
raw23March21_7<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-23_Picarro/raw23March21_7.dat", header=T)

# these ones have a different time format

raw23March21_3<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-23_Picarro/raw23March21_3.dat", header=T)
raw23March21_4<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-23_Picarro/raw23March21_4.dat", header=T)
raw23March21_5<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-23_Picarro/raw23March21_5.dat", header=T)
raw23March21_6<-read.table("C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/Campaign1_03.2021 - Copy/Picarro/2021-03-23_Picarro/raw23March21_6.dat", header=T)

#merge column of time and PM on column TIME12, and delete the PM column and the TIME column
raw23March21_3$TIME12 <- paste(raw23March21_3$TIME,raw23March21_3$PM)
raw23March21_3$PM<- NULL
raw23March21_3$TIME<- NULL
raw23March21_4$TIME12 <- paste(raw23March21_4$TIME,raw23March21_4$PM)
raw23March21_4$PM<- NULL
raw23March21_4$TIME<- NULL
raw23March21_5$TIME12 <- paste(raw23March21_5$TIME,raw23March21_5$PM)
raw23March21_5$PM<- NULL
raw23March21_5$TIME<- NULL
raw23March21_6$TIME12 <- paste(raw23March21_6$TIME,raw23March21_6$PM)
raw23March21_6$PM<- NULL
raw23March21_6$TIME<- NULL

# put all the times in the same format (24h format, column TIME) and delete the column of 12h format (TIME12)

raw23March21_3$TIME <- format(strptime(raw23March21_3$TIME12, "%I:%M:%S %p"), format="%H:%M:%S")
raw23March21_3$TIME12<- NULL
raw23March21_4$TIME <- format(strptime(raw23March21_4$TIME12, "%I:%M:%S %p"), format="%H:%M:%S")
raw23March21_4$TIME12<- NULL
raw23March21_5$TIME <- format(strptime(raw23March21_5$TIME12, "%I:%M:%S %p"), format="%H:%M:%S")
raw23March21_5$TIME12<- NULL
raw23March21_6$TIME <- format(strptime(raw23March21_6$TIME12, "%I:%M:%S %p"), format="%H:%M:%S")
raw23March21_6$TIME12<- NULL

# merge all the files of each date in one file

raw23March21 <- rbind(raw23March21_1, raw23March21_2,raw23March21_3,raw23March21_4,raw23March21_5,raw23March21_6, raw23March21_7)
str(raw23March21)
head(raw23March21)

# create a column merging date and time

time<-as.POSIXct(paste(raw23March21$DATE, raw23March21$TIME), format="%Y-%m-%d %H:%M:%S")
raw23March21<-cbind(raw23March21,time)
str(raw23March21)

#transform data.frame into data.table

raw23March21 <- as.data.table(raw23March21)

#Select only the columns we are interested in 
# we are lso keeping the column with the date-time in numerical format

raw23March21 <- raw23March21[, c(4,9,10,21,22,23)]
raw23March21 <- setNames(raw23March21, c("frac_hours", "CH4", "CO2", "CH4_dry", "CO2_dry","time"))

#select the time range of each measure
# YOU MUST QUIT 1H TO THE TIMES OF THE EXCEL FILE

#FLOWING
M4_JO01<-raw23March21[time >= "2021-03-23 8:42:00" & time <= "2021-03-23 8:47:00"]
M3_JO01<-raw23March21[time >= "2021-03-23 9:08:00" & time <= "2021-03-23 9:13:00"]
M2_JO01<-raw23March21[time >= "2021-03-23 9:15:00" & time <= "2021-03-23 9:22:00"]
M1_JO01<-raw23March21[time >= "2021-03-23 9:39:00" & time <= "2021-03-23 9:44:00"]
M6_JO01<-raw23March21[time >= "2021-03-23 9:50:00" & time <= "2021-03-23 9:55:00"]
M5_JO01<-raw23March21[time >= "2021-03-23 10:11:00" & time <= "2021-03-23 10:16:00"]

M1_AL05<-raw23March21[time >= "2021-03-23 15:26:00" & time <= "2021-03-23 15:31:00"]
M2_AL05<-raw23March21[time >= "2021-03-23 15:36:00" & time <= "2021-03-23 15:41:00"]
M3_AL05<-raw23March21[time >= "2021-03-23 15:59:00" & time <= "2021-03-23 16:07:00"]
M4_AL05<-raw23March21[time >= "2021-03-23 16:07:00" & time <= "2021-03-23 16:12:00"]
M5_AL05<-raw23March21[time >= "2021-03-23 16:28:00" & time <= "2021-03-23 16:33:00"]
M6_AL05<-raw23March21[time >= "2021-03-23 16:36:00" & time <= "2021-03-23 16:41:00"]

#RIPARIAN
R6_JO01<-raw23March21[time >= "2021-03-23 8:52:00" & time <= "2021-03-23 8:57:00"]
R5_JO01<-raw23March21[time >= "2021-03-23 9:08:00" & time <= "2021-03-23 9:05:00"]
R4_JO01<-raw23March21[time >= "2021-03-23 9:15:00" & time <= "2021-03-23 9:29:00"]
R7_JO01<-raw23March21[time >= "2021-03-23 9:39:00" & time <= "2021-03-23 10:03:00"]

R1_AL05<-raw23March21[time >= "2021-03-23 15:19:00" & time <= "2021-03-23 15:24:00"]
R2_AL05<-raw23March21[time >= "2021-03-23 15:42:00" & time <= "2021-03-23 15:48:00"]
R3_AL05<-raw23March21[time >= "2021-03-23 15:51:00" & time <= "2021-03-23 15:56:00"]
R4_AL05<-raw23March21[time >= "2021-03-23 16:14:00" & time <= "2021-03-23 16:19:00"]
R5_AL05<-raw23March21[time >= "2021-03-23 16:22:00" & time <= "2021-03-23 16:27:00"]
R6_AL05<-raw23March21[time >= "2021-03-23 16:43:00" & time <= "2021-03-23 16:48:00"]

## THERE IS A GAP IN PICARRO FILES FROM 12:16 TO 13:43
## NO DATA FOR CO01, REPETED THE NEXT DAY

#create an ID column for each measure

#FLOWING
M1_JO01$ID <- rep("M1_JO01",nrow(M1_JO01)) 
M2_JO01$ID <- rep("M2_JO01",nrow(M2_JO01)) 
M3_JO01$ID <- rep("M3_JO01",nrow(M3_JO01)) 
M4_JO01$ID <- rep("M4_JO01",nrow(M4_JO01)) 
M5_JO01$ID <- rep("M5_JO01",nrow(M5_JO01)) 
M6_JO01$ID <- rep("M6_JO01",nrow(M6_JO01))

M1_AL05$ID <- rep("M1_AL05",nrow(M1_AL05)) 
M2_AL05$ID <- rep("M2_AL05",nrow(M2_AL05)) 
M3_AL05$ID <- rep("M3_AL05",nrow(M3_AL05)) 
M4_AL05$ID <- rep("M4_AL05",nrow(M4_AL05)) 
M5_AL05$ID <- rep("M5_AL05",nrow(M5_AL05)) 
M6_AL05$ID <- rep("M6_AL05",nrow(M6_AL05))

#RIPARIAN
R6_JO01$ID <- rep("R6_JO01",nrow(R6_JO01)) 
R5_JO01$ID <- rep("R5_JO01",nrow(R5_JO01)) 
R4_JO01$ID <- rep("R4_JO01",nrow(R4_JO01)) 
R7_JO01$ID <- rep("R7_JO01",nrow(R7_JO01)) 

R1_AL05$ID <- rep("R1_AL05",nrow(R1_AL05)) 
R2_AL05$ID <- rep("R2_AL05",nrow(R2_AL05)) 
R3_AL05$ID <- rep("R3_AL05",nrow(R3_AL05)) 
R4_AL05$ID <- rep("R4_AL05",nrow(R4_AL05)) 
R5_AL05$ID <- rep("R5_AL05",nrow(R5_AL05)) 
R6_AL05$ID <- rep("R6_AL05",nrow(R6_AL05))

# merge all the data in one file

JO01 <- rbind(M1_JO01, M2_JO01, M3_JO01, M4_JO01, M5_JO01, M6_JO01, R6_JO01, R5_JO01, R4_JO01, R7_JO01)
AL05 <- rbind(M1_AL05, M2_AL05, M3_AL05, M4_AL05, M5_AL05, M6_AL05, R1_AL05, R2_AL05, R3_AL05, R4_AL05, R5_AL05, R6_AL05)

##EXPORT DATA FILES

write.csv(JO01, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/JO01.csv")
write.csv(AL05, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/AL05.csv")

March_23 <-rbind (JO01, AL05)
write.csv(March_23, "C:/Users/naina/OneDrive/Escritorio/WP3_sampling data/PICARRO_March/March_23.csv")

#########
# PLOTS #
#########

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=M1_JO01)
plot(CO2_dry~ time, data=M2_JO01)
plot(CO2_dry~ time, data=M3_JO01)
plot(CO2_dry~ time, data=M4_JO01)
plot(CO2_dry~ time, data=M5_JO01)
plot(CO2_dry~ time, data=M6_JO01)

plot(CH4_dry~ time, data=M1_JO01)
plot(CH4_dry~ time, data=M2_JO01)
plot(CH4_dry~ time, data=M3_JO01)
plot(CH4_dry~ time, data=M4_JO01)
plot(CH4_dry~ time, data=M5_JO01)
plot(CH4_dry~ time, data=M6_JO01)

par(mfrow=c(2,6))
plot(CO2_dry~ time, data=M1_AL05)
plot(CO2_dry~ time, data=M2_AL05)
plot(CO2_dry~ time, data=M3_AL05)
plot(CO2_dry~ time, data=M4_AL05)
plot(CO2_dry~ time, data=M5_AL05)
plot(CO2_dry~ time, data=M6_AL05)

plot(CH4_dry~ time, data=M1_AL05)
plot(CH4_dry~ time, data=M2_AL05)
plot(CH4_dry~ time, data=M3_AL05)
plot(CH4_dry~ time, data=M4_AL05)
plot(CH4_dry~ time, data=M5_AL05)
plot(CH4_dry~ time, data=M6_AL05)


#########################
#### FLUX CALCULATIONS ##
#########################

gasfluxes(M1_CA02, .id = "ID", .V = "V", .A = "A", .times = "time",
.C = "C", methods = c("linear", "robust linear", "HMR", "NDFE"),
k_HMR = log(1.5), k_NDFE = log(0.01), verbose = TRUE,
plot = TRUE, select, maxiter = 100, ...)




