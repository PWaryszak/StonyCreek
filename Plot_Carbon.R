#StonyCreek:
library(readxl)
library(tidyverse)
# Start the code
rm(list = ls(all = TRUE))

MainDir = setwd("~/00DeakinUni/R/BCL_R/StonyCreek")
setwd(MainDir)

# Get the data=====
Data = read_excel("MangroveVego_12Feb2020.xlsx", sheet = "Organic_Carbon")
str(Data)
Data$Year = as.factor(Data$Year)
Data$Elevation2 = factor(Data$Elevation, levels = c("High", "Low","Lowest"))

names(Data)
unique(Data$Site)

#Plot by Year and DepthRange.cm============
a <- ggplot(Data[Data$Elevation2 != "Lowest",] ,
            aes(x = Elevation2, y = C_percent, color= Elevation2))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha = 0.6) +
  ylab("Organic Carbon (%)") + xlab("") +
  facet_grid(Depth_Range_hc~Year)+
  theme_bw() +
  ggtitle("Stony Creek")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))
a
ggsave("CarbonByYearDepth.jpg", height = 6.3, width = 8)

#Plot by Year and Elevation ============
b <- ggplot(Data[Data$Elevation2 != "Lowest",] ,
            aes(x = Elevation2, y = C_percent, color= Elevation2))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha = 0.6) +
  ylab("Organic Carbon (%)") + xlab("") +
  facet_grid(.~Year)+
  theme_bw() +
  ggtitle("Stony Creek")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))
b
ggsave("CarbonByYearElevation.jpg", height = 6.3, width = 8)


##Compute Mg/ha using OC(%)==========
NewDATA <- Data
NewDATA$C_percent <- ifelse(NewDATA$C_percent == 0, 0.001, NewDATA$C_percent)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$DepthTo.cm - NewDATA$DepthFrom.cm) #round % to full numbers to run Poisson
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLength.cm  #slice volume
NewDATA$dry_bulk_density.gcm3 <- NewDATA$DryWeight.g / NewDATA$SampleVolume.cm3

NewDATA$Core_in.mm <- ((NewDATA$PipeLength.cm *10)  - NewDATA$CompactionIn.mm) # Compaction in mm!
NewDATA$Pipe_in.mm <- (NewDATA$PipeLength.cm *10)  - NewDATA$CompactionOut.mm  #Compaction in mm!
NewDATA$Compaction_Correction_Value<- NewDATA$Core_in.mm/NewDATA$Pipe_in.mm

NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$dry_bulk_density.gcm3 * NewDATA$Compaction_Correction_Value

NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C_percent/100
NewDATA$CarbonStock.Mgha <- (((NewDATA$CarbonDensity.gcm3  / 1000000 ) *100000000) * NewDATA$SliceLength.cm )

range(NewDATA$Compaction_Correction_Value, na.rm = T)# Check if all values are  below 1 = If value = 1 this core had no records of compaction
range(NewDATA$CarbonStock.Mgha, na.rm = T )# 0.6328892 239.2190928
range(NewDATA$CarbonDensity.gcm3, na.rm = T )#0.0009671682 0.0956876371


#Plot Carbon by elevation========
aa <- ggplot(NewDATA[NewDATA$Elevation2 != "Lowest",],
             aes(x = Year, y = CarbonStock.Mgha, color = t)) +
  geom_point( size = 2)+
  #geom_boxplot(outlier.shape = NA) +
  facet_grid(DepthRange.cm~ Elevation2)+ #geom_jitter( size = 2,alpha = 0.4, aes(color = t))+
  labs(y = bquote('Carbon Stock  ' (Mg*~ha^-1 ~y^-1)), x="Mangrove Arrival Time",
       color = "Transect: ") +
  theme_bw() +
  coord_flip()+
  ggtitle("Stony Creek (HSBC)")+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=14),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))
aa

ggsave("CarbonByYearElevationDepth.jpg", height = 9.3, width = 8)

