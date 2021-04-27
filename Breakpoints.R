###   CALCULATING BREAKPOINTS IN GHG DATA   #######
###    for automation of quality control   ###########

################################
## Load the necessary packages

library(tidyverse)
library(mcp)
library(dpseg)
library(tidyr)
library(dplyr)
library(purrr)

########################
# Read in the CO2/CH4 data as well as the N2O data

dat<- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_CH4_dat.csv", header=T)

###############################################

#try the dpseg package 

#example with the same site, but removing 1st and last 30secs of each measure

## BR02_A1

BR02_A1<-subset(dat,ID== "BR02_A1") # subseting one measure
plot(CO2_dry~flux_time, data=BR02_A1) #  just checking how the plot looks like

#here I am using a function to estimate P, and rounded it to 4 decimals
# we can also put a defined value of P (i.e. P= 1e-4, higher number= longer segments
# we can also set the maximum number of points per segment with maxl

BR02_A1_dpseg <- dpseg(x=BR02_A1$flux_time,
                       y=BR02_A1$CO2_dry,
                       jumps = FALSE, #TRUE allows disconnected segments
                       P = round(estimateP(x=BR02_A1$flux_time, y=BR02_A1$CO2_dry,4)), # break-point penalty, higher number gives shorter segments
                       minl=60) # minimum number of points, I have put 60 (1 min)
# Should we change type="r2"? But R2  depends on the slope and will score poorly for segments without slope, therefore the default type="var" variance might be fine
print(BR02_A1_dpseg)

p <- estimateP(x=BR02_A1$flux_time,y=BR02_A1$CO2_dry, plot=TRUE)
plot(dpseg(x=BR02_A1$flux_time,y=BR02_A1$CO2_dry,  jumps=F, P=round(p,))) #p:66.021
estimateP(x=BR02_A1$flux_time,y=BR02_A1$CO2_dry)/10 #6.602061

simple_estimateP <- function (x, y, ...) {
  var(smooth.spline(x, y, ...)$y - y)
}

#          x1         x2 start end intercept      slope        r2          var
#1 0.00000000 0.01429250     1  60 0.4334434 24.8985478 0.9714008 3.253137e-04
#2 0.01429250 0.02874556    60 119 0.6714233  7.3116918 0.9720139 2.812393e-05
#3 0.02874556 0.04330028   119 178 0.8145417  2.0654836 0.9648347 2.888266e-06
#4 0.04330028 0.06687139   178 273 0.9092172 -0.1674492 0.1054703 1.137366e-05

#x1= starting flux_time of the segment
#x2= end flux_time of the segment
#start= starting point (1=1st point of the time series) 
#end= ending point

plot(BR02_A1_dpseg)
lines(predict(BR02_A1_dpseg),lty=2, lwd=3, col="yellow")

# Try another
#CA02_R4
CA02_R4<-subset(dat,ID== "CA02_R4") # subseting one measure
plot(CO2_dry~flux_time, data=CA02_R4) #  just checking how the plot looks like
CA02_R4_dpseg <- dpseg(x=CA02_R4$flux_time,
                       y=CA02_R4$CO2_dry,
                       jumps = F, #TRUE allows disconnected segments
                       P = round(estimateP(x=CA02_R4$flux_time, y=CA02_R4$CO2_dry,4)), 
                       minl=3) # minimum number of points, I have put 60 (1 min)
print(CA02_R4_dpseg)
plot(CA02_R4_dpseg)
lines(predict(CA02_R4_dpseg),lty=2, lwd=3, col="yellow")

# Try another
#GR01_F1
GR01_F1<-subset(dat,ID== "GR01_F1") # subseting one measure
plot(CH4_dry~flux_time, data=GR01_F1) #  just checking how the plot looks like
GR01_F1_dpseg <- dpseg(x=GR01_F1$flux_time,
                       y=GR01_F1$CH4_dry,
                       jumps = F, #TRUE allows disconnected segments
                       P = round(estimateP(x=GR01_F1$flux_time, y=GR01_F1$CH4_dry,4)), 
                       minl=3) # minimum number of points, I have put 60 (1 min)
print(GR01_F1_dpseg)
plot(GR01_F1_dpseg)
lines(predict(GR01_F1_dpseg),lty=2, lwd=3, col="yellow")


######
# NEXT STEP

#Nest data by ID

#Nest the data
dat_nested <- dat %>% 
  group_by(ID) %>% 
  nest()

#define function to obtain breaking segmented fits

CO2_dpseg <- function(x) { dpseg(x=x$flux_time,
                                 y=x$CO2_dry,
                                 jumps = FALSE, 
                                 P = 1e-4,
                                 minl=60) }

CH4_dpseg <- function(x) { dpseg(x=x$flux_time,
                                 y=x$CH4_dry,
                                 jumps = FALSE, 
                                 P = 1e-4,
                                 minl=60) }

dat_nested <- dat_nested %>%  
  mutate(CO2_dpseg  = map(data, CO2_dpseg)) %>% 
  mutate(CH4_dpseg = map(data, CH4_dpseg))



##############################################

###############################################

CO2model <- function(x) {
  lm(CO2_dry ~ flux_time, data = x)
}

CH4model <- function(x) {
  lm(CH4_dry ~ flux_time, data = x)
}

dat_nested  <- dat_nested  %>% 
  mutate(modelCO2 = map(data, CO2model)) %>% 
  mutate(modelCH4 = map(data, CH4model))

dat_nested

#####################################

#Now add the breaking points

library(mcp)

# Define the model #How to define with three breaking points
model = list(
  CO2_dry ~ 1,  # plateau (int_1)
  ~ 0 + flux_time,    # joined slope (time_2) at cp_1
  ~ 1 + flux_time  # disjoined slope (int_3, time_3) at cp_2
)

# Get example data and fit it
CO01_A1<-subset(dat,ID== "CO01_A1")

model1 = mcp(model, data = CO01_A1)

plot(model1)

model1



prior = list(cp_2 = "dunif(0.06, 0.07)")


model2 = mcp(model, data = CO01_A1, prior = prior)
plot(model2) 

model2

############################################################
#### Finding breakpoints and creating new start/end times ####
############################################################

### EXAMPLE FOR RA01_B1 
RA01_B1<-subset(CO2_CH4_dat,ID== "RA01_B1") #ONE MEASURE OF 1 SITE
fit0<-lm(CO2_dry~flux_time, data=RA01_B1) #SIMPLE LINEAR MODEL
fit1 <- segmented(fit0, seg.Z = ~ flux_time, psi = list(flux_time = c(0.02, 0.08))) # PSI??? HERE IS THE ERROR WHEN RUNNING THE LOOP
# When you make psi NA it makes a bad fit #if you set psi to just 0.08 it creates a different break point
summary(fit1) #SHOWS YOU THE BREAKING POINT(S)
plot(CO2_dry ~flux_time, data=RA01_B1) #BASIC PLOT
lines(RA01_B1$flux_time, predict(fit1), col = "red", lwd=5) # ADD LINES FROM SEGMENTED FIT
# get the breakpoints
fit1$psi
# get the slopes
slope(fit1)



#breakpoints() function in the strucchange
breakpoints(CO2_dry~flux_time, data=RA01_B1)
#Breakpoints at observation number:  98 163 261 
#Corresponding to breakdates:  0.2925373 0.4865672 0.7791045 
breakpoints(CO2_dry~flux_time, data=RA01_B1, h=100)



## Loop that almost works
#### try to create loop to include slope-breaking points

#Try sorting the flux time within each ID
CO2_CH4_dat <- data.table(CO2_CH4_dat, key = c("ID", "flux_time"))

ID <- unique(CO2_CH4_dat$ID)


for (i in ID) {
  fit0<-lm(CO2_dry~flux_time, data=subset(CO2_CH4_dat,ID== i))
  fit1 <- segmented(fit0, seg.Z = ~ flux_time, psi = list(flux_time = c(0.02, 0.07)))
  CO2_plot = ggplot(data=subset(CO2_CH4_dat, ID==i)) +
    geom_point(size=1, aes(x=flux_time, y=CO2_dry)) +
    ggtitle(i) +
    geom_line(data=subset(CO2_CH4_dat,ID== i), aes(x=flux_time, y=predict(fit1)), col = "red", lwd=1.5) } 

#May be helpful: http://rstudio-pubs-static.s3.amazonaws.com/12164_ce6e9b8542e8494b8d225b61b632efd9.html psi are estimated of when you think the breakpoints will be
#When psi=NA it says 14 cases when no breakpoint estimated and Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 0 (non-NA) cases

#Error starting psi out of the admissible range
#Error: starting psi out of the range.. see 'alpha' in seg.control. #optional numerical value. The breakpoint is estimated within the quantiles alpha and 1-alpha of the relevant covariate.

### From the package: check if psi is eligible...
#c1 <- apply((Z <= PSI), 2, all) #should all be FALSE (before it was only <)
#c2 <- apply((Z >= PSI), 2, all) #should all be FALSE (before it was only >)
#if(sum(c1 + c2) != 0 || is.na(sum(c1 + c2)) ) stop("starting psi out of the admissible range")

ggsave(CO2_plot, path="C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Flux_figures", 
       file=paste0("plot_", i,".png"), width = 7, height = 5, units = "cm") 
}	

## Romain's loop


ID <- unique(CO2_CH4_dat$ID)
for (i in ID) {
  i=1
  ###RA01_B1
  IDd<-ID[i]
  x<-subset(CO2_CH4_dat,ID== IDd)
  fit0<-lm(CO2_dry ~flux_time, data=x)
  fit1 <- segmented(fit0, seg.Z = ~ time, psi = 1)
  summary(fit1)
  plot(CO2_dry~flux_time, data=RA01_B1) }

##### Can try another package

median(Picarro_dat$CO2_dry)
median(Picarro_dat$CH4_dry)
