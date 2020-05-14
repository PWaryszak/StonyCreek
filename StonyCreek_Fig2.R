#LOAD data Off Stony Creek =====
library(readxl)
library(tidyverse)
library(gridExtra)

Soil_Data = read_excel("MangroveVego_12Feb2020.xlsx", sheet = "Organic_Carbon")
Soil_Data$Year = as.factor(Soil_Data$Year)
Soil_Data$Elevation2 = factor(Soil_Data$Elevation, levels = c("High", "Low","Lowest"))

##Compute Mg/ha using OC(%)==========
NewDATA <- Soil_Data
NewDATA$C_percent <- ifelse(NewDATA$C_percent == 0, 0.001, NewDATA$C_percent)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$DepthTo.cm - NewDATA$DepthFrom.cm) #round % to full numbers to run Poisson
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLength.cm  #slice volume
NewDATA$dry_bulk_density.gcm3 <- NewDATA$DryWeight.g / NewDATA$SampleVolume.cm3
NewDATA$Core_in.mm <- ((NewDATA$PipeLength.cm *10)  - NewDATA$CompactionIn.mm) # Compaction in mm!
NewDATA$Pipe_in.mm <- (NewDATA$PipeLength.cm *10)  - NewDATA$CompactionOut.mm  #Compaction in mm!
NewDATA$Compaction_Correction_Value<- NewDATA$Core_in.mm/NewDATA$Pipe_in.mm
NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$dry_bulk_density.gcm3 * NewDATA$Compaction_Correction_Value
NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C_percent/100
NewDATA$CarbonStock.Mgha <- ((NewDATA$CarbonDensity.gcm3  *100 * NewDATA$SliceLength.cm ))

range(NewDATA$Compaction_Correction_Value, na.rm = T)# Check if all values are  below 1 = If value = 1 this core had no records of compaction
range(NewDATA$CarbonStock.Mgha, na.rm = T )# 0.6328892 239.2190928
range(NewDATA$CarbonDensity.gcm3, na.rm = T )#0.0009671682 0.0956876371

#PLOT SOIL========
#Soil Carbon to horizon (DepthAtRehab_cm:)
#1986 core: ~9 mm a year. This means 1986 is about 30 cm.
#1996 core: ~14cm is 1996.
#2006 core: ~8cm is 2006

#Cut core till DepthAtRehab_cm and estimate CarbonStockTillRehab_Mgha:
CarbonTillRehab_cores <- NewDATA %>% filter(Elevation != "Lowest") %>% #This one was weird plot, not in design
  mutate(DepthTo_SinceRehabilitated_cm = ifelse(SiteYear ==  "Stony1986",30,
                                                ifelse(SiteYear ==  "Stony1996", 14, 8)))%>%
  mutate(KeepThrow = ifelse(DepthTo_SinceRehabilitated_cm >= DepthFrom.cm, "keep", "throw")) %>% #keep slices data are > Depth_from
  filter(KeepThrow=="keep") %>% #keep the "keep"
  transform (DepthAtRehab_cm = ifelse(DepthTo.cm <= DepthTo_SinceRehabilitated_cm,  #Cut to the length acc to DepthTo_SinceRehabilitated_cm
                                      DepthTo.cm, DepthTo_SinceRehabilitated_cm)) %>%
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - DepthFrom.cm) %>% #lenght of slice at cores up to DepthTo_SinceRehabilitated_cm
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm,  #Soilc C stock in cores till DepthTo_SinceRehabilitated_cm
          Stock = "Soil")

#Compute Mean Soil Carbon Stock across sites:
CarbonTillRehab_sites <- CarbonTillRehab_cores %>%
  group_by(Plot_ID) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStockTillRehab_Mgha))%>% #Add-up all slices 
  separate(Plot_ID, into = c("Site","Transect" ,"CoreNumber"), sep = "_") %>%
  group_by(Site) %>% #to sum total Carbon Stock per Site
  summarise(AV = mean(TotalCarbonStockPerCore, na.rm = T),
            SD = sd(TotalCarbonStockPerCore, na.rm = T),
            N = length(TotalCarbonStockPerCore),
            SE = SD / sqrt(N)) %>%
  mutate(SiteYear = ifelse(Site=="Stony1986", 1986, 
                           ifelse(Site=="Stony1996", 2002, 2006)))

#write.csv(CarbonTillRehab_sites, file = "SoilCarbonTillRehab_Stony.csv", row.names = F)

#Plot:
soil_stock_plot<- ggplot(CarbonTillRehab_sites, 
                         aes(x= 2019-as.numeric(as.character(SiteYear)), y= AV )) +
  geom_point(aes(shape = Site, color = Site),size = 5) +
  geom_errorbar( aes(ymin = AV + SE,
                     ymax = AV - SE), width=.2)+
  scale_y_continuous(limits = c(0, 100))+
  scale_x_continuous(limits = c(0, 40))+
  labs(x= "Mangrove age (years)", shape = "Site Type: ",
       y = bquote('Soil C-stock since rehabiliatation'~~(Mg*~ha^-1)))+
  guides(color = FALSE) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 10, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

  #In case we need legend add below to edit it:
  theme(axis.text.x = element_text(size = 14, color = "black",angle = 90),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = c(0.1,0.85),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        strip.text=element_text(size=14),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))


#PLOT PLANT=======
Plant_Data = read_excel("MangroveVego_12Feb2020.xlsx", sheet = "Mangrove_Plots")
Plant_Data <- filter(Plant_Data, Elevation != "Lowest" & Elevation != "SM")  #Lowest $ SM (Saltmarhs) is cut-out as it was an extra done by Peter/Maria.
Plant_Data$Elevation2 = factor(Plant_Data$Elevation, levels = c("High", "Low"))
names(Plant_Data)

#Allometric equation from Saintilan (1997)
#Can be applied to smaller trees as opposed to other equations:
Plant_Data$PlantBiomass <-  Plant_Data$Canopy_Height_m * (((0.214 * (Plant_Data$Circumf_cm_at_base_1*pi) - 0.113)) ^2) /10

#Sum up MangroveBiomass for fully survedy plots and extrapolate for not-fully surveyd 
Plant_Carbon <- Plant_Data %>%
  mutate(SiteYear = ifelse(Site=="Stony1986", 1986,ifelse(Site=="Stony1996", 2002, 2006))) %>%
  #Some plots were not fully surveydP All_Ind_Surveyd = "no", hence we need to  extrapolote
  #If TotalMangroveDensity is > length(PlantBiomass)
  group_by(Plot_ID) %>% #Sum up biomass by plot (observational unit)
  summarise(TotalMangroveDensity = mean(Mangrove_density_25m2),
            SurveyedMangroveDensity = length(PlantBiomass),
            SurveyedBiomass = sum(PlantBiomass, na.rm=T),
            TotalBiomass = TotalMangroveDensity * SurveyedBiomass /SurveyedMangroveDensity)


#Get Plant Carbon Averages:
AV_Plant_Carbon <- Plant_Carbon %>%
  #Split by year:
  separate(Plot_ID, into = c("Site","transect", "location"), sep = "_" , remove = F) %>%
  
  #change Years as per Pauls' sattelites images
  mutate(SiteYear = ifelse(Site=="Stony1986", 1986,ifelse(Site=="Stony1996", 2002, 2006))) %>% 
  mutate(Plant_Stand_Age = 2019 - SiteYear) %>%
  
  #get means +- SE by Site
  group_by(Site,SiteYear,Plant_Stand_Age) %>%
  summarise(AV_plant = round(mean(TotalBiomass, na.rm = T),1),
            N_plant = length(TotalBiomass),
            SD_plant = round(sd(TotalBiomass),1),
            SE_plant = round(SD_plant/sqrt(N_plant),1))

AV_Plant_Carbon 
#write.csv(AV_Plant_Carbon, file = "AV_Plant_Carbon_Stony.csv", row.names = F)

#PLOT:
plant_stock_plot <- ggplot(AV_Plant_Carbon, aes(x= 2019-SiteYear, y= AV_plant)) +
  geom_point(aes(shape = Site, color = Site),size = 5) +
  geom_errorbar( aes(ymin = AV_plant + SE_plant,
                     ymax = AV_plant - SE_plant), width=.2)+
  #scale_y_continuous(limits = c(0, 100))+
  scale_x_continuous(limits = c(0, 40))+
  labs(x= "Mangrove age (years)", 
       y = bquote('Plant C-stock since rehabiliatation'~~(Mg*~ha^-1)))+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 10, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))



#PLOT CAR======
StonyCarbon = read_excel("MangroveVego_12Feb2020.xlsx", sheet = "Organic_Carbon")
StonyCarbon$Year = as.factor(StonyCarbon$Year)
StonyCarbon$Elevation2 = factor(StonyCarbon$Elevation, levels = c("High", "Low","Lowest"))
unique(StonyCarbon$Site)#"Stony06" "Stony96" "Stony86"

#Get MAR data off Pere's file above. MAR's units = g/cm2/y
MAR <- data.frame( Site = c( "Stony86" ,"Stony96" ,"Stony06"),
                   MAR = c(0.22, 0.14, 0.096),
                   MAR_SE = c(0, 0.02, 0.007)) 

StonyCarbonCAR <- left_join(StonyCarbon,MAR, by = "Site") %>%
  filter(DepthTo.cm <= 30 & Elevation2 == "High" )#that' deep's where age was estimated

#CAR till 30cm:
StonyCarbonCAR_30cm <- StonyCarbonCAR %>%
  #group_by(Plot_ID) %>% 
  mutate(CAR_gcm2y =  C_percent/100 * MAR)%>%
  group_by(SiteYear) %>% 
  #Averages after Converting Grams Per Square Centimeter to Tonnes Per Hectare = 100
  summarise( AV         = weighted.mean   (CAR_gcm2y*100, na.rm = T), 
             SD         = sd    (CAR_gcm2y*100, na.rm = T),
             N          = length(CAR_gcm2y),
             SE         = SD / sqrt(N)) %>%
  mutate (Depth        = "00to30") %>%
  mutate(SiteYearNum = ifelse(SiteYear=="Stony1986", 1986, 
                           ifelse(SiteYear=="Stony1996", 2002, 2006)))


#write.csv(StonyCarbonCAR_30cm, file = "StonyCarbonCAR_30cm.csv", row.names = F)

#CAR PLOT:
car_plot <- ggplot(StonyCarbonCAR_30cm, aes(x=2019-SiteYearNum, AV))  +
  geom_point(aes(shape = SiteYear, color =SiteYear, size = 3)) +
  geom_errorbar( aes(ymin = AV + SE,
                     ymax = AV - SE), width=.2)+
  scale_x_continuous(limits = c(0, 40))+
  labs(x= "Mangrove age (years)", y = bquote('Carbon Accretion Rate ' (Mg*~ha^-1 ~y^-1)))+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 10, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))


#PLOT PLANT SOIL CAR TOGETHER==========
grid.arrange( plant_stock_plot, soil_stock_plot, car_plot, ncol=3)

#Mean Mangrove Tree Biomasss====
TreeBiomass_plot <- ggplot(Plant_Data, aes(x = Elevation2, y = PlantBiomass, color=Year)) +
  #geom_point(size = 2, alpha = 0.2) +
  facet_grid(~Year) + ylab("Biomass per Tree (kg)") +
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
  stat_summary(fun.y = "mean", size = 3, geom = "point") +
  theme_bw() +
  ggtitle("Eq4_long, from Saintilan (1997)")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 10, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

TreeBiomass_plot



