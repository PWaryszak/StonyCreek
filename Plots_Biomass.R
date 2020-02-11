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
Data = read_excel("MangroveVego_17Jan2020.xlsx", sheet = "Mangrove_Plots")
str(Data)
Data$Year = as.factor(Data$Year)
Data$Elevation2 = factor(Data$Elevation, levels = c("High", "Low","Lowest"))


# Plot Mangrove density===========
ggplot(data = subset(Data, !is.na(Elevation2)), aes(x = Year, y = Mangrove_density_25m2)) +
geom_point(size = 2, alpha = 0.4) +
facet_grid(~Elevation2) +
stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
stat_summary(fun.y = "mean", size = 3, geom = "point")
#ggsave("Mangrove density.jpg", height = 6.3, width = 8)


ggplot(data = subset(Data, !is.na(Elevation2)), aes(x = Elevation2, y = Mangrove_density_25m2),color=Year) +
geom_point(size = 2, alpha = 0.2, aes(color= Year)) +
facet_grid(~Year) +
stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1,mapping = aes(group = Year)) +
stat_summary(fun.y = "mean", size = 3, geom = "point",mapping = aes(group = Year))
#ggsave("Mangrove density2.jpg",  height = 6.3, width = 8)


######## Equation for biomass (kg)========
Data_LowHigh <- filter(Data, Elevation2 != "Lowest")#Lowest is cut as it was an extra done by Peter.

Data_LowHigh$Biomass = 0.0509* 							# Constant
			   0.62 *  							# wood density (g/cm3)
			((Data_LowHigh$Circumf_cm_at_base_1/pi)^2* 	# diameter at breast height^2 (cm)
			   Data_LowHigh$Canopy_Height_m)				# height (m)

hist(Data_LowHigh$Biomass)


ggplot(data = subset(Data_LowHigh, !is.na(Elevation2)), aes(x = Year, y = Biomass)) +
geom_point(size = 2, alpha = 0.2) +
facet_grid(~Elevation2) +
stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
stat_summary(fun.y = "mean", size = 3, geom = "point") +
scale_y_log10()
ggsave("Biomass.jpg", height = 6.3, width = 8)


ggplot(data = subset(Data_LowHigh, !is.na(Elevation2)), aes(x = Elevation2, y = Biomass,color=Year)) +
#geom_point(size = 2, alpha = 0.2) +
facet_grid(~Year) + ylab("Log Biomass per Tree (kg)")+
stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
stat_summary(fun.y = "mean", size = 3, geom = "point") +scale_y_log10()
ggsave("Biomass2.jpg", height = 6.3, width = 8)

Data2<- group_by (Data_LowHigh, plot_ID)%>%
  summarise(TreeMeanBiomass = mean (Biomass, na.rm=T),
            meanDensity = mean(Mangrove_density_25m2),
            plotBiomass = TreeMeanBiomass * meanDensity) %>%
  separate(plot_ID, into = c("Year","T","Elevation2"), sep = "_")
Data2
Data3 <- filter(Data2, Elevation2 != "SM")

ggplot(data = subset(Data3, !is.na(Elevation2)), aes(x = Elevation2, y = plotBiomass,color=Year)) +
  geom_point(size = 2, alpha = 0.2) +
  facet_grid(~Year) + ylab("Biomass per plot (kg)")+
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
  stat_summary(fun.y = "mean", size = 3, geom = "point") +scale_y_log10()
ggsave("BiomassPerPlot.jpg", height = 6.3, width = 8)








