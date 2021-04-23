
# Load in Picarro flux results 

CO2_CH4 <-read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_CH4_flux.results.csv")
str(CO2_CH4)

# Add a column for "riparian" and "aquatic"

CO2_CH4 <- CO2_CH4 %>% mutate (ecosystem=ifelse(flow_state=="flowing", "aquatic", "riparian"))
CO2_CH4$ecosystem[is.na(CO2_CH4$ecosystem)] <- "riparian"

######### Clean ########################

#Subset only positive CO2 values # Look into whether or not these values are valid...

CO2_CH4 <- CO2_CH4 %>% filter(CO2_mg_m2_h > 0)
str(CO2_CH4)

################ Exploratory figures #############################

#### AQUATIC ####
#Subset the aquatic data
CO2_CH4_aq <- CO2_CH4[which(CO2_CH4$ecosystem=='aquatic'),]
str(CO2_CH4_aq)

# Compare fluxes between drying regime 

plot(CO2_mg_m2_h ~ drying_regime, data=CO2_CH4_aq)

ggplot(CO2_CH4_aq, aes(x=drying_regime , y=CO2_mg_m2_h,  colour=drying_regime)) + 
  geom_jitter(position=position_jitter(0.2)) + theme_pubr()  + labs(title="Aquatic - flowing") + theme(plot.title = element_text(hjust = 0.5)) +theme(legend.position="none")

boxplot(CH4_ug_m2_h ~ drying_regime, data=CO2_CH4_aq)

ggplot(CO2_CH4_aq, aes(x=drying_regime , y=CH4_ug_m2_h,  colour=drying_regime)) + 
  geom_jitter(position=position_jitter(0.2)) + theme_pubr() +theme(legend.position="none") + labs(title="Aquatic - flowing") + theme(plot.title = element_text(hjust = 0.5)) 

# Compare fluxes between habitat type (aquatic)

ggplot(data=subset(CO2_CH4_aq, !is.na(habitat_type)), aes(x=habitat_type , y=CO2_mg_m2_h)) + geom_jitter(position=position_jitter(0.15)) + theme_pubr() +theme(legend.position="none") + labs(title="Aquatic - flowing") + theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data=subset(CO2_CH4_aq, !is.na(habitat_type)), aes(x=habitat_type , y=CH4_ug_m2_h)) + geom_jitter(position=position_jitter(0.15)) + theme_pubr() +theme(legend.position="none") + labs(title="Aquatic - flowing") + theme(plot.title = element_text(hjust = 0.5)) 

# Regress (aquatic fluxes) with water depth

# Compare fluxes between stream order 


#### RIPARIAN ####
#Subset the riparian data
CO2_CH4_ri <- CO2_CH4[which(CO2_CH4$ecosystem=='riparian'),]
str(CO2_CH4_ri)


# Compare fluxes between drying regime 
boxplot(CO2_mg_m2_h ~ drying_regime, data=CO2_CH4_ri)

ggplot(CO2_CH4_ri, aes(x=drying_regime , y=CO2_mg_m2_h, color=drying_regime)) + 
  geom_jitter(position=position_jitter(0.2)) + theme_pubr()  + labs(title="Riparian") + theme(plot.title = element_text(hjust = 0.5)) +theme(legend.position="none")


ggplot(CO2_CH4_ri, aes(x=drying_regime , y=CH4_ug_m2_h, color=drying_regime)) + 
  geom_jitter(position=position_jitter(0.2)) + theme_pubr()  + labs(title="Riparian") + theme(plot.title = element_text(hjust = 0.5)) +theme(legend.position="none")

# Regress soil temperature + soil moisture with riparian fluxes

plot(CO2_CH4_ri$soil_temp, CO2_CH4_ri$CO2_mg_m2_h)
abline(lm(CO2_mg_m2_h ~ soil_temp, data = CO2_CH4_ri), col = "red")

plot(CO2_CH4_ri$VWC, CO2_CH4_ri$CO2_mg_m2_h) 
abline(lm(CO2_mg_m2_h ~ VWC, data = CO2_CH4_ri), col = "red")

#try excluding the 0.8 value
ggplot(CO2_CH4_ri[which(CO2_CH4_ri$VWC < 0.8),], aes(x=VWC, y=CO2_mg_m2_h)) + geom_point()  + theme_pubr() +  geom_smooth(method='lm')

ggplot(CO2_CH4_ri[which(CO2_CH4_ri$VWC < 0.8),], aes(x=VWC, y=CH4_ug_m2_h)) + geom_point()  + theme_pubr() +  geom_smooth(method='lm')

# Compare fluxes between stream order 






