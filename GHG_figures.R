#Load necessary packages
library(ggpmisc)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(wesanderson)
library(forcats)
library(cowplot)
########################

# Load in Picarro flux results 
CO2_CH4 <-read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_CH4_flux.results.csv")
str(CO2_CH4) #203 obs.

#load in stream order
stream_order <-read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/Campaign1_raw_data/Ancillary_data/stream_order_prelim.csv")
#Change ID to site
names(stream_order)[names(stream_order) == "Code_final"] <- "site"

#merge with fluxes
CO2_CH4 <- merge (CO2_CH4, stream_order , by="site")

#can load uncleaned data files just to see 
CO2_CH4_untidy <-read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Data/R/GHGdata/CO2_CH4_flux.results_untidy.csv")
str(CO2_CH4_untidy) #207 obs.

# Add a column for "riparian" and "aquatic"

CO2_CH4 <- CO2_CH4 %>% mutate (ecosystem=ifelse(flow_state=="flowing", "aquatic", "riparian"))
CO2_CH4$ecosystem[is.na(CO2_CH4$ecosystem)] <- "riparian"

CO2_CH4_untidy <- CO2_CH4_untidy %>% mutate (ecosystem=ifelse(flow_state=="flowing", "aquatic", "riparian"))
CO2_CH4_untidy$ecosystem[is.na(CO2_CH4_untidy$ecosystem)] <- "riparian"

######### Change units ################
# CO2 fluxes are typically reported as g CO2-C / m2 / d 
CO2_CH4$CO2_g_m2_d <- (CO2_CH4$CO2_mg_m2_h * 24)/ 1000
CO2_CH4_untidy$CO2_g_m2_d <- (CO2_CH4_untidy$CO2_mg_m2_h * 24)/ 1000

# CH4 fluxes are typically reported as mg CH4-C / m2 / d
CO2_CH4$CH4_mg_m2_d <- (CO2_CH4$CH4_ug_m2_h * 24) /1000
CO2_CH4_untidy$CO2_g_m2_d <- (CO2_CH4_untidy$CO2_mg_m2_h * 24)/ 1000

######### Clean ########################

#Should we subset only positive CO2 values # Look into whether or not these values are valid... #No, studies only have positive CO2 but for aquatic some don't .... 

CO2_CH4 <- CO2_CH4 %>% filter(CO2_mg_m2_h > 0)
str(CO2_CH4)

#Also most study values below around 10/20 CO2, so look into the two outliers...

################# PICK A NICE COLOUR PALETTE  ############################
#this one is nice
wes <- wes_palette("FantasticFox1", 5, type = c("discrete"))
print(paste(wes))
#"#DD8D29" "#E2D200" "#46ACC8" "#E58601" "#B40F20"
#Orange, green-y yellow, blue, orange, red

wes <- wes_palette("Royal2", 5, type = c("discrete"))
print(paste(wes))
# "#9A8822" "#F5CDB4" "#F8AFA8" "#FDDDA0" "#74A089"
# pastel green, peach, pink, yellow, mint

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
 theme_pubr()  + theme(axis.title.x = element_blank())  + theme(legend.title = element_blank()) + scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) +  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ ylab(expression(mg~CH[4]*`-C`~m^-2~d^-1)) +  theme(plot.title = element_text(hjust = 0.5))
CH4_comp 


#### Now combine the CO2 and CH4 plots
jpeg(file = "CO2_CH4_comp.jpeg", width=6, height=8, units="in", res=1500) 
plot <- ggarrange(CO2_comp , CH4_comp,  nrow=2, common.legend = TRUE, legend="top", align="v",  labels="AUTO")
plot
dev.off()

########################################
# compare STREAM ORDER across HABITATS (aquatic/riparian)

CO2_order <- ggplot(CO2_CH4, aes(x=ecosystem, y=CO2_g_m2_d)) + 
  geom_boxplot(aes(fill=stream_order), outlier.shape = NA)  + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))  + theme_pubr()  + theme(axis.title.x = element_blank())  + theme(legend.title = element_blank()) + scale_fill_manual(values = c( "#F5CDB4", "#74A089") ) +  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ 
  scale_y_continuous(limits = c(0,17)) +
  annotate("text", x = 0.79, y = 16.2, label=sprintf('\u2191')) +
  annotate("text", x = 0.84, y = 16, label="43") +
    annotate("text", x = 1.10, y = 16.2, label=sprintf('\u2191')) +
  annotate("text", x = 1.20, y = 16, label="18, 37") +
    ylab(expression(g~CO[2]*`-C`~m^-2*~d^-1)) 
CO2_order 

CH4_order <- ggplot(CO2_CH4, aes(x=ecosystem, y=CH4_mg_m2_d)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  geom_boxplot(aes(fill=stream_order), outlier.shape = NA)  + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))+
  theme_pubr()  + theme(axis.title.x = element_blank())  + theme(legend.title = element_blank()) +  scale_fill_manual(values = c( "#F5CDB4", "#74A089") ) +  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ ylab(expression(mg~CH[4]*`-C`~m^-2~d^-1)) +  theme(plot.title = element_text(hjust = 0.5))
CH4_order 

#### Now combine the CO2 and CH4 plots
jpeg(file = "CO2_CH4_orderbyhabitat.jpeg", width=6, height=8, units="in", res=1500) 
plot <- ggarrange(CO2_order, CH4_order, nrow=2, common.legend = TRUE, legend="top", align="v",  labels="AUTO")
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
  geom_jitter(aes(fill=drying_regime), position=position_jitter(0.2),  cex=2) + theme_pubr() + scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) + theme(legend.position="none") + labs(title="Aquatic - flowing") + 
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

CO2_htype <- ggplot(data=subset(CO2_CH4_aq, !is.na(habitat_type)), aes(x=habitat_type , y=CO2_g_m2_d)) + 
    geom_boxplot(aes(fill=habitat_type), outlier.shape = NA) + geom_point(aes(colour=habitat_type, fill=habitat_type), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))  + scale_fill_manual(values = c( "#E2D200", "#46ACC8")) +
   theme_pubr() +theme(legend.position="none") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  ylab(expression(g~CO[2]*`-C`~m^-2*~d^-1)) + theme(axis.title.x = element_blank())  +
  scale_y_continuous(limits = c(0,15)) +
  annotate("text", x = 2.16, y = 14.5, label=sprintf('\u2191')) +
  annotate("text", x = 2.28, y = 14.5, label="43") +
  theme(plot.title = element_text(hjust = 0.5)) 
CO2_htype


CH4_htype <- ggplot(data=subset(CO2_CH4_aq, !is.na(habitat_type)), aes(x=habitat_type , y=CH4_mg_m2_d)) + 
  geom_boxplot(aes(fill=habitat_type), outlier.shape = NA) + geom_point(aes(colour=habitat_type, fill=habitat_type), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))  + scale_fill_manual(values = c( "#E2D200", "#46ACC8")) +
  ylab(expression(mg~CH[4]*`-C`~m^-2~d^-1)) + 
  theme_pubr() +theme(legend.position="none") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + theme(axis.title.x = element_blank())  + theme(plot.title = element_text(hjust = 0.5)) 
CH4_htype

#### Now combine the CO2 and CH4 files for flowing habitat
jpeg(file = "CO2_CH4_Flowing_by_habitat.jpeg", width=8, height=4, units="in", res=1500) 
plot <- ggarrange(CO2_htype, CH4_htype,  ncol=2, legend = "none", labels="AUTO", align="h")
plot
dev.off()



# Regress (aquatic fluxes) with water DEPTH
plot(CO2_CH4_aq$pool_riffle_depth_cm, CO2_CH4_aq$CO2_mg_m2_h)
abline(lm(CO2_mg_m2_h ~ pool_riffle_depth_cm, data = CO2_CH4_ri), col = "red")

depthCO2 <- ggplot(CO2_CH4_aq, aes(x=pool_riffle_depth_cm, y=CO2_g_m2_d)) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, size=2, colour="black" ) +    scale_fill_manual(values = wes_palette("GrandBudapest2", n = 2)) + ylab(expression(g~CO[2]*`-C`~m^-2*~d^-1)) + xlab("Depth (cm)") +  theme_pubr() +  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
depthCO2

depthCH4 <- ggplot(CO2_CH4_aq, aes(x=pool_riffle_depth_cm, y=CH4_mg_m2_d)) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, size=2, colour="black" ) +    scale_fill_manual(values = wes_palette("GrandBudapest2", n = 2))+ ylab(expression(mg~CH[4]*`-C`~m^-2~d^-1))  + xlab("Depth (cm)") + theme_pubr() +  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
depthCH4

## Combined plot for water depth
#### Now combine the CO2 and CH4 files for flowing habitat
jpeg(file = "Water_depth_CO2_CH4.jpeg", width=8, height=4, units="in", res=1500) 
plot <- ggarrange(depthCO2, depthCH4,  ncol=2, legend = "top", common.legend = TRUE, labels="AUTO", align="h")
plot
dev.off()


# Compare fluxes between stream order 
# e.g. boxplot by site, ordered in increasing stream order
CO2_CH4$stream_order <- as.factor(CO2_CH4$stream_order)

CO2_stream_order_bysite <- ggplot(data=CO2_CH4_aq, aes(x=reorder(site, CO2_g_m2_d, median), y=CO2_g_m2_d)) + 
  geom_boxplot(aes(fill=stream_order), outlier.shape = NA) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))+   scale_fill_manual(values = wes_palette("GrandBudapest2", n = 2))+
  theme_pubr()  +  theme(axis.text.x = element_text(angle = 90)) + 
  ylab(expression(g~CO[2]*`-C`~m^-2*~d^-1)) + theme(axis.title.x = element_blank())  
CO2_stream_order_bysite

CO2_stream_order <- ggplot(data=CO2_CH4_aq, aes(x=stream_order, y=CO2_g_m2_d)) + 
  geom_boxplot(aes(fill=stream_order), outlier.shape = NA) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))+   scale_fill_manual(values = wes_palette("GrandBudapest2", n = 2))+
  theme_pubr()  +  ylab(expression(g~CO[2]*`-C`~m^-2*~d^-1)) + 
  scale_y_continuous(limits = c(0,20)) +
  annotate("text", x = 2.16, y = 18, label=sprintf('\u2191')) +
  annotate("text", x = 2.28, y = 18, label="37") +
  annotate("text", x = 1.16, y = 18, label=sprintf('\u2191')) +
  annotate("text", x = 1.28, y = 18, label="43") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  theme(axis.title.x = element_blank())  
CO2_stream_order

CH4_stream_order <- ggplot(data=CO2_CH4_aq, aes(x=stream_order, y=CH4_mg_m2_d)) + 
  geom_boxplot(aes(fill=stream_order), outlier.shape = NA) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))+   scale_fill_manual(values = wes_palette("GrandBudapest2", n = 2))+
  theme_pubr()   +  ylab(expression(mg~CH[4]*`-C`~m^-2~d^-1)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ theme(axis.title.x = element_blank())  
CH4_stream_order

#### Now combine the CO2 and CH4 files for stream order
jpeg(file = "CO2_CH4_stream_order_aq.jpeg", width=8, height=4, units="in", res=1500) 
plot <- ggarrange(CO2_stream_order, CH4_stream_order,  ncol=2, legend = "none", labels="AUTO", align="h")
plot
annotate_figure(plot, top = text_grob("Aquatic", color = "black", face = "bold", size = 14))
dev.off()

##############################################
#Interaction of stream order and drying regime

CO2_stream_order_drying <- ggplot(data=CO2_CH4_aq, aes(x=drying_regime, y=CO2_g_m2_d, group=interaction(stream_order, drying_regime))) + 
  geom_boxplot(aes(fill=stream_order), outlier.shape = NA) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))+   scale_fill_manual(values = wes_palette("GrandBudapest2", n = 3))+
  theme_pubr()  +  ylab(expression(g~CO[2]*`-C`~m^-2*~d^-1)) + 
  scale_y_continuous(limits = c(0,21)) +
  annotate("text", x = 2.75, y = 20.3, label=sprintf('\u2191')) +
  annotate("text", x = 2.85, y = 20, label="37") +
  annotate("text", x = 3.12, y = 20.3, label=sprintf('\u2191')) +
  annotate("text", x = 3.22, y = 20, label="43") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  theme(axis.title.x = element_blank())  
CO2_stream_order_drying

CH4_stream_order_drying <- ggplot(data=CO2_CH4_aq, aes(x=drying_regime, y=CH4_mg_m2_d, group=interaction(stream_order, drying_regime))) + 
  geom_boxplot(aes(fill=stream_order), outlier.shape = NA) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))+   scale_fill_manual(values = wes_palette("GrandBudapest2", n = 2))+
  theme_pubr()   +  ylab(expression(mg~CH[4]*`-C`~m^-2~d^-1)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ theme(axis.title.x = element_blank())  
CH4_stream_order_drying

#### Now combine the CO2 and CH4 files for stream order
jpeg(file = "CO2_CH4_stream_order_drying_aq.jpeg", width=6, height=8, units="in", res=1500) 
plot <- ggarrange(CO2_stream_order_drying + rremove("x.text"), CH4_stream_order_drying,  nrow=2, legend = "bottom", common.legend = TRUE, labels="AUTO", align="hv")
plot
annotate_figure(plot, top = text_grob("Aquatic", color = "black", face = "bold", size = 14))
dev.off()


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

my.formula <-CO2_CH4_ri$CO2_g_m2_d ~ CO2_CH4_ri$soil_temp

CO2temp <- ggplot(CO2_CH4_ri, aes(x=soil_temp, y=CO2_g_m2_d)) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, size =2, colour="black")  +  geom_smooth(method='lm', colour="black")   + scale_fill_manual(values = c( "#F5CDB4", "#74A089") )+
  ylab(expression(g~CO[2]*`-C`~m^-2*~d^-1)) + xlab("Soil temperature (\u00B0C)") + theme_pubr() + stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(hat(y))~`=`~",aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), parse = TRUE)  + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
CO2temp

plot(CO2_CH4_ri$soil_temp, CO2_CH4_ri$CH4_ug_m2_h)
abline(lm(CH4_ug_m2_h ~ soil_temp, data = CO2_CH4_ri), col = "red")

my.formula <-CO2_CH4_ri$CH4_mg_m2_d ~ CO2_CH4_ri$soil_temp

CH4temp <- ggplot(CO2_CH4_ri, aes(x=soil_temp, y=CH4_mg_m2_d)) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, size =2, colour="black")  +  geom_smooth(method='lm', colour="black")  + xlab("Soil temperature (\u00B0C)") + scale_fill_manual(values = c( "#F5CDB4", "#74A089") )+ theme_pubr() +  ylab(expression(mg~CH[4]*`-C`~m^-2~d^-1)) + stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(hat(y))~`=`~",aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), parse = TRUE)  + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
CH4temp

#### Now combine the CO2 and CH4 files for SOIL TEMPERATURE
jpeg(file = "Soil_temperature_CO2_CH4.jpeg", width=8, height=4, units="in", res=1500) 
plot <- ggarrange(CO2temp, CH4temp,  ncol=2, legend = "top", common.legend=TRUE, labels="AUTO", align="hv")
plot
dev.off()


# Regress soil  moisture with riparian fluxes

#multiply by 100 to get percentage
CO2_CH4_ri$VWC <- CO2_CH4_ri$VWC * 100

plot(CO2_CH4_ri$VWC, CO2_CH4_ri$CO2_mg_m2_h) 
abline(lm(CO2_mg_m2_h ~ VWC, data = CO2_CH4_ri), col = "red")

plot(CO2_CH4_ri$VWC, CO2_CH4_ri$CH4_ug_m2_h) 
abline(lm(CH4_ug_m2_h ~ VWC, data = CO2_CH4_ri), col = "red")

#try excluding the 0.8 value
ggplot(CO2_CH4_ri[which(CO2_CH4_ri$VWC < 0.8),], aes(x=VWC, y=CO2_mg_m2_h)) + geom_point()  + theme_pubr() +  geom_smooth(method='lm')

my.formula <-CO2_CH4_ri$CO2_g_m2_d ~ CO2_CH4_ri$VWC

CO2moist <- ggplot(CO2_CH4_ri, aes(x=VWC, y=CO2_g_m2_d)) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, size =2, colour="black")  +  geom_smooth(method='lm', colour="black")   + scale_fill_manual(values = c( "#F5CDB4", "#74A089") )+
  ylab(expression(g~CO[2]*`-C`~m^-2*~d^-1)) + xlab("Soil moisture (%)") + theme_pubr() + stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(hat(y))~`=`~",aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), parse = TRUE)  + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
CO2moist


ggplot(CO2_CH4_ri[which(CO2_CH4_ri$VWC < 0.8),], aes(x=VWC, y=CH4_ug_m2_h)) + geom_point()  + theme_pubr() +  geom_smooth(method='lm') 

my.formula <-CO2_CH4_ri$CH4_mg_m2_d ~ CO2_CH4_ri$VWC

CH4moist <- ggplot(CO2_CH4_ri, aes(x=VWC, y=CH4_mg_m2_d)) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, size =2, colour="black")  +  geom_smooth(method='lm', colour="black")  + xlab("Soil moisture (%)") + scale_fill_manual(values = c( "#F5CDB4", "#74A089") )+ theme_pubr() +  ylab(expression(mg~CH[4]*`-C`~m^-2~d^-1)) + stat_poly_eq(formula = my.formula,eq.with.lhs = "italic(hat(y))~`=`~",aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), parse = TRUE)  + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
CH4moist

#### Now combine the CO2 and CH4 files for SOIL MOISTURE
jpeg(file = "Soil_moisture_CO2_CH4.jpeg", width=8, height=4, units="in", res=1500) 
plot <- ggarrange(CO2moist, CH4moist,  ncol=2, legend = "top", common.legend=TRUE, labels="AUTO", align="hv")
plot
dev.off()

# Compare RIPARIAN fluxes between stream order 

CO2_stream_orderr <- ggplot(data=CO2_CH4_ri, aes(x=stream_order, y=CO2_g_m2_d)) + 
  geom_boxplot(aes(fill=stream_order), outlier.shape = NA) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1)) +   scale_fill_manual(values = c( "#F5CDB4", "#74A089")) + 
  theme_pubr()  +  ylab(expression(g~CO[2]*`-C`~m^-2*~d^-1)) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  theme(axis.title.x = element_blank())  
CO2_stream_orderr


CH4_stream_orderr <- ggplot(data=CO2_CH4_ri, aes(x=stream_order, y=CH4_mg_m2_d)) + 
  geom_boxplot(aes(fill=stream_order), outlier.shape = NA) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1)) +   scale_fill_manual(values = c( "#F5CDB4", "#74A089")) + 
  theme_pubr()   +  ylab(expression(mg~CH[4]*`-C`~m^-2~d^-1)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ theme(axis.title.x = element_blank())  
CH4_stream_orderr

#### Now combine the RIPARIAN CO2 and CH4 plots for stream order
jpeg(file = "CO2_CH4_stream_order_ri.jpeg", width=8, height=4, units="in", res=1500) 
plot <- ggarrange(CO2_stream_orderr, CH4_stream_orderr,  ncol=2, legend = "none", labels="AUTO", align="h")
plot
annotate_figure(plot, top = text_grob("Riparian", color = "black", face = "bold", size = 14))
dev.off()

########################################################
#Interaction of stream order and drying regime for RIPARIAN

CO2_stream_order_drying_r <- ggplot(data=CO2_CH4_ri, aes(x=drying_regime, y=CO2_g_m2_d, group=interaction(stream_order, drying_regime))) + 
  geom_boxplot(aes(fill=stream_order), outlier.shape = NA) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))+  scale_fill_manual(values = c( "#F5CDB4", "#74A089")) + 
  theme_pubr()  +  ylab(expression(g~CO[2]*`-C`~m^-2*~d^-1)) + 
  #scale_y_continuous(limits = c(0,21)) +
  #annotate("text", x = 2.75, y = 20.3, label=sprintf('\u2191')) +
  #annotate("text", x = 2.85, y = 20, label="37") +
  #annotate("text", x = 3.12, y = 20.3, label=sprintf('\u2191')) +
  #annotate("text", x = 3.22, y = 20, label="43") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  theme(axis.title.x = element_blank())  
CO2_stream_order_drying_r

CH4_stream_order_drying_r <- ggplot(data=CO2_CH4_ri, aes(x=drying_regime, y=CH4_mg_m2_d, group=interaction(stream_order, drying_regime))) +  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
  geom_boxplot(aes(fill=stream_order), outlier.shape = NA) + geom_point(aes(colour=stream_order, fill=stream_order), shape=21, colour="black", position = position_jitterdodge(jitter.width=0.1))+   scale_fill_manual(values = c( "#F5CDB4", "#74A089")) + 
  theme_pubr()   +  ylab(expression(mg~CH[4]*`-C`~m^-2~d^-1)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ theme(axis.title.x = element_blank())  
CH4_stream_order_drying_r

#### Now combine the CO2 and CH4 files for stream order 
jpeg(file = "CO2_CH4_stream_order_drying_ri.jpeg", width=6, height=8, units="in", res=1500) 
plot <- ggarrange(CO2_stream_order_drying_r + rremove("x.text"), CH4_stream_order_drying_r,  nrow=2, legend = "bottom", common.legend = TRUE, labels="AUTO", align="hv")
plot
annotate_figure(plot, top = text_grob("Riparian", color = "black", face = "bold", size = 14))
dev.off()

