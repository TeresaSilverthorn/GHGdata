############# Extracting and cleaning temperature data from iButtons ##########


## 1) Load an individual temperature file 

air.temp <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/iButton data/2020.03.15-26_iButtonincar.csv",
                  skip = 14, header=TRUE)
colnames(air.temp) <- c("DateTime", "Unit", "Temp.C")
head(air.temp)
str(air.temp)

# Use the time format of the imported csv to define an "as.POSIXct" time - R's time format.
air.temp$DateTime <- as.POSIXct(air.temp$DateTime, format = "%d/%m/%y %I:%M:%S %p")
#plot to view
plot(air.temp$Temp.C ~ air.temp$DateTime, type="l")

#Cut out the unnecessary data
air.temp <- air.temp[66:294,] #	2021-03-16 07:40:00 to 2021-03-25 19:40:00

#export results
write.csv(air.temp, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/Campaign1_raw_data/iButton data/air.temp.2021.03.csv")

