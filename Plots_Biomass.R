library(ggplot2)
library(readxl)
library(reshape2)
library(plyr)
library(tidyverse)
# Start the code
rm(list = ls(all = TRUE))

MainDir = setwd("~/00DeakinUni/R/BCL_R/StonyCreek")
setwd(MainDir)

# Get the data=====
Data = read_excel("MangroveVego_12Feb2020.xlsx", sheet = "Mangrove_Plots")
str(Data)
Data$Year = as.factor(Data$Year)
Data$Elevation2 = factor(Data$Elevation, levels = c("High", "Low","Lowest"))


# Plot Mangrove density===========
Data <- filter(Data, Elevation != "Lowest")
ggplot(data = subset(Data, !is.na(Elevation2)), aes(x = Elevation2, y = Mangrove_density_25m2,color=Year)) +
geom_point( alpha = 0.1) +
facet_grid(.~Year) +
stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1,mapping = aes(group = Year)) +
stat_summary(fun.y = "mean", size = 3, geom = "point")+
  labs(y = bquote('Mangrove Density  ' (25*~m^-2)), x="Mangrove Arrival Time") +
  theme_bw() +
  ggtitle("Stony Creek (HSBC)")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

ggsave("StonyCreek_MangroveDensity.jpg",  height = 6.3, width = 8)


######## Mangrove Biomass (kg)========
Data_LowHigh <- filter(Data, Elevation2 != "Lowest")#Lowest is cut as it was an extra done by Peter.

Data_LowHigh$Biomass = 0.0509* 							# Constant
			   0.62 *  							# wood density (g/cm3)
			((Data_LowHigh$Circumf_cm_at_base_1/pi)^2* 	# diameter at breast height^2 (cm)
			   Data_LowHigh$Canopy_Height_m)				# height (m)

ggplot(data = subset(Data_LowHigh, !is.na(Elevation2)), aes(x = Elevation2, y = Biomass,color=Year)) +
#geom_point(size = 2, alpha = 0.2) +
facet_grid(~Year) + ylab("Log Biomass per Tree (kg)")+
stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
stat_summary(fun.y = "mean", size = 3, geom = "point") +
  labs(y = bquote('Mangrove Biomass  ' (kg~25*~m^-2)), x="Mangrove Arrival Time") +
  theme_bw() +
  ggtitle("Stony Creek (HSBC)")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))


ggsave("Biomass2.jpg", height = 6.3, width = 8)








