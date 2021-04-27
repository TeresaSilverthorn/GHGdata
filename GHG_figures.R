#Load necessary packages
library(ggpmisc)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(wesanderson)
library(forcats)
library(cowplot)


# Load in Picarro flux results 

CO2_CH4 <-read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_CH4_flux.results.csv")
str(CO2_CH4)

# Add a column for "riparian" and "aquatic"

CO2_CH4 <- CO2_CH4 %>% mutate (ecosystem=ifelse(flow_state=="flowing", "aquatic", "riparian"))
CO2_CH4$ecosystem[is.na(CO2_CH4$ecosystem)] <- "riparian"

######### Change units ################
# CO2 fluxes are typically reported as g CO2-C / m2 / d 
CO2_CH4$CO2_g_m2_d <- (CO2_CH4$CO2_mg_m2_h * 24)/ 1000

# CH4 fluxes are typically reported as mg CH4-C / m2 / d
CO2_CH4$CH4_mg_m2_d <- (CO2_CH4$CH4_ug_m2_h * 24) /1000

######### Clean ########################

#Should we subset only positive CO2 values # Look into whether or not these values are valid... #No, studies only have positive CO2

CO2_CH4 <- CO2_CH4 %>% filter(CO2_mg_m2_h > 0)
str(CO2_CH4)

#Most study values below around 10/20 CO2, so look into the two outliers...

################ Exploratory figures #############################

########################################
# Compare riparian and aquatic fluxes

CO2_comp <- ggplot(CO2_CH4, aes(x=ecosystem, y=CO2_g_m2_d)) + 
   geom_boxplot(aes(fill=drying_regime), outlier.shape = NA) + geom_point(aes(colour=drying_regime, fill=drying_regime), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))  +
  theme_pubr()  + theme(axis.title.x = element_blank())  + theme(legend.title = element_blank()) + scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) +  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ ylab(expression(g~CO[2]*`-C`~m^-2*~d^-1)) + 
  scale_y_continuous(limits = c(0,20.5)) +
  annotate("text", x = 1.16, y = 20.2, label=sprintf('\u2191')) +
  annotate("text", x = 1.28, y = 20, label="37, 43") +
  theme(plot.title = element_text(hjust = 0.5))
CO2_comp 

CH4_comp <- ggplot(CO2_CH4, aes(x=ecosystem, y=CH4_mg_m2_d)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  geom_boxplot(aes(fill=drying_regime), outlier.shape = NA) + geom_point(aes(colour=drying_regime, fill=drying_regime), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))  +
 theme_pubr()  + theme(axis.title.x = element_blank())  + theme(legend.title = element_blank()) + scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) +  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ ylab(expression(g~CO[2]*`-C`~m^-2*~hr^-1)) +  theme(plot.title = element_text(hjust = 0.5)) + ylab(expression(mg~CH[4]*`-C`~m^-2~d^-1)) +  theme(plot.title = element_text(hjust = 0.5))
CH4_comp 


#### Now combine the CO2 and CH4 files

jpeg(file = "CO2_CH4_comp.jpeg", width=6, height=8, units="in", res=1500) 
plot <- ggarrange(CO2_comp , CH4_comp,  nrow=2, common.legend = TRUE, legend="top", labels="AUTO")
plot
dev.off()

########################################

#### AQUATIC ####
#Subset the aquatic data
CO2_CH4_aq <- CO2_CH4[which(CO2_CH4$ecosystem=='aquatic'),]
str(CO2_CH4_aq)

# Compare fluxes between drying regime 

plot(CO2_CH4_aq$CO2_mg_m2_h ~ CO2_CH4_aq$drying_regime)

CO2_aq_dry <- ggplot(CO2_CH4_aq, aes(x=drying_regime , y=CO2_mg_m2_h,  colour=drying_regime)) + 
  geom_jitter(position=position_jitter(0.2)) + theme_pubr() +theme(legend.position="none") + labs(title="Aquatic - flowing") + 
  theme(plot.title = element_text(hjust = 0.5))
CO2_aq_dry

boxplot(CH4_ug_m2_h ~ drying_regime, data=CO2_CH4_aq)

CH4_aq_dry <- ggplot(CO2_CH4_aq, aes(x=drying_regime , y=CH4_ug_m2_h,  colour=drying_regime)) + 
  geom_jitter(position=position_jitter(0.2)) + theme_pubr() +theme(legend.position="none") + #labs(title="Aquatic - flowing") + 
  theme(plot.title = element_text(hjust = 0.5))
CH4_aq_dry

#Combine plots together
ggarrange(CO2_aq_dry + rremove("x.title") + rremove("x.text") + rremove("legend"), CH4_aq_dry , 
          legend="none",
          labels= c("CO2", "CH4"),
          nrow = 2)



# Compare fluxes between habitat type (aquatic)

CO2_htype <- ggplot(data=subset(CO2_CH4_aq, !is.na(habitat_type)), aes(x=habitat_type , y=CO2_mg_m2_h)) + geom_jitter(position=position_jitter(0.15)) + theme_pubr() +theme(legend.position="none") + labs(title="Aquatic - flowing") + theme(plot.title = element_text(hjust = 0.5)) 
CO2_htype

CH4_htype <- ggplot(data=subset(CO2_CH4_aq, !is.na(habitat_type)), aes(x=habitat_type , y=CH4_ug_m2_h)) + geom_jitter(position=position_jitter(0.15)) + theme_pubr() +theme(legend.position="none") + labs(title="Aquatic - flowing") + theme(plot.title = element_text(hjust = 0.5)) 
CH4_htype

# Regress (aquatic fluxes) with water depth
plot(CO2_CH4_aq$pool_riffle_depth_cm, CO2_CH4_aq$CO2_mg_m2_h)
abline(lm(CO2_mg_m2_h ~ pool_riffle_depth_cm, data = CO2_CH4_ri), col = "red")

ggplot(CO2_CH4_aq, aes(x=pool_riffle_depth_cm, y=CO2_mg_m2_h)) + geom_point()  +  geom_smooth(method='lm')  + theme_pubr()


# Compare fluxes between stream order 




#### RIPARIAN ####
#Subset the riparian data
CO2_CH4_ri <- CO2_CH4[which(CO2_CH4$ecosystem=='riparian'),]
str(CO2_CH4_ri)


# Compare fluxes between drying regime 
boxplot(CO2_mg_m2_h ~ drying_regime, data=CO2_CH4_ri)

CO2_ri_dry <- ggplot(CO2_CH4_ri, aes(x=drying_regime , y=CO2_mg_m2_h, color=drying_regime)) +   geom_jitter(position=position_jitter(0.2)) + ggplot2::theme_pubr()  + labs(title="Riparian") + theme(plot.title = element_text(hjust = 0.5)) +theme(legend.position="none")
CO2_ri_dry

CH4_ri_dry <- ggplot(CO2_CH4_ri, aes(x=drying_regime , y=CH4_ug_m2_h, color=drying_regime)) + 
  geom_jitter(position=position_jitter(0.2)) + theme_pubr()  + labs(title="Riparian") + theme(plot.title = element_text(hjust = 0.5)) +theme(legend.position="none")
CH4_ri_dry

# Regress soil temperature with riparian fluxes

plot(CO2_CH4_ri$soil_temp, CO2_CH4_ri$CO2_mg_m2_h)
abline(lm(CO2_mg_m2_h ~ soil_temp, data = CO2_CH4_ri), col = "red")

my.formula <-CO2_CH4_ri$CO2_mg_m2_h ~ CO2_CH4_ri$soil_temp

ggplot(CO2_CH4_ri, aes(x=soil_temp, y=CO2_mg_m2_h)) + geom_point()  +  geom_smooth(method='lm')  + theme_pubr() + stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(hat(y))~`=`~",aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), parse = TRUE) 

plot(CO2_CH4_ri$soil_temp, CO2_CH4_ri$CH4_ug_m2_h)
abline(lm(CH4_ug_m2_h ~ soil_temp, data = CO2_CH4_ri), col = "red")

ggplot(CO2_CH4_ri, aes(x=soil_temp, y=CH4_ug_m2_h)) + geom_point()  +  geom_smooth(method='lm')  + theme_pubr() 


# Regress soil  moisture with riparian fluxes

plot(CO2_CH4_ri$VWC, CO2_CH4_ri$CO2_mg_m2_h) 
abline(lm(CO2_mg_m2_h ~ VWC, data = CO2_CH4_ri), col = "red")

plot(CO2_CH4_ri$VWC, CO2_CH4_ri$CH4_ug_m2_h) 
abline(lm(CH4_ug_m2_h ~ VWC, data = CO2_CH4_ri), col = "red")

#try excluding the 0.8 value
ggplot(CO2_CH4_ri[which(CO2_CH4_ri$VWC < 0.8),], aes(x=VWC, y=CO2_mg_m2_h)) + geom_point()  + theme_pubr() +  geom_smooth(method='lm')

ggplot(CO2_CH4_ri[which(CO2_CH4_ri$VWC < 0.8),], aes(x=VWC, y=CH4_ug_m2_h)) + geom_point()  + theme_pubr() +  geom_smooth(method='lm') 

# Compare fluxes between stream order 






