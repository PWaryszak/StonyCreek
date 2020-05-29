#LOAD data Off Stony Creek =====
library(readxl)
library(tidyverse)
library(gridExtra)
library(readxl)
library(sjPlot)
library(sjmisc)


Soil_Data = read_excel("MangroveVego_12Feb2020.xlsx", sheet = "Organic_Carbon")
Soil_Data$Year = as.factor(Soil_Data$Year)
Soil_Data$Elevation2 = factor(Soil_Data$Elevation, levels = c("High", "Low","Lowest"))
Soil_Data <- filter(Soil_Data, Elevation != "Lowest" & Elevation != "SM")  #Lowest $ SM (Saltmarhs) is cut-out as it was an extra done by Peter/Maria.

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
          StockType = "Soil")

#1. Analysis of soil C variation===
#variation in  soil carbon stocks across three rehabilitated/reestablished mangrove sites

soil_stock <- CarbonTillRehab_cores  %>%
  group_by (Site, Elevation2,Plot_ID) %>%
  summarise(core_total = sum (CarbonStockTillRehab_Mgha, na.rm = T)) %>%
  #Convert to updated years:
  mutate(SiteYear = ifelse(Site=="Stony86", "Rehab_1986", 
                           ifelse(Site=="Stony96", "Rehab_2002", "Rehab_2006"))) 

#Check data distribution and run soil_stock model:
hist(soil_stock$core_total , data = soil_stock)
#Relevel factors to start off from the youngest SiteYear= 2006:
soil_model <- lm (core_total ~ SiteYear, data = soil_stock)
summary(soil_model )
tab_model(soil_model)  


#2. Analysis of plant C variation=======
Plant_Data = read_excel("MangroveVego_12Feb2020.xlsx", sheet = "Mangrove_Plots")
Plant_Data <- filter(Plant_Data, Elevation != "Lowest" & Elevation != "SM")  #Lowest $ SM (Saltmarhs) is cut-out as it was an extra done by Peter/Maria.
Plant_Data$Elevation2 = factor(Plant_Data$Elevation, levels = c("High", "Low"))
names(Plant_Data)

#Allometric equation from Saintilan (1997)
#Can be applied to smaller trees as opposed to other equations:
Plant_Data$PlantBiomass <-  Plant_Data$Canopy_Height_m * (((0.214 * (Plant_Data$Circumf_cm_at_base_1*pi) - 0.113)) ^2) /10

#Sum up MangroveBiomass for fully survedy plots and extrapolate for not-fully surveyd 
plant_stock <- Plant_Data %>%
  #Some plots were not fully surveydP All_Ind_Surveyd = "no", hence we need to  extrapolote
  #If TotalMangroveDensity is > length(PlantBiomass)
  group_by(Plot_ID,Year2) %>% #Sum up biomass by Plot_ID (observational unit)
  summarise(TotalMangroveDensity = mean(Mangrove_density_25m2),
            SurveyedMangroveDensity = length(PlantBiomass),
            SurveyedBiomass = sum(PlantBiomass, na.rm=T)/25 *10 *0.464,
            TotalBiomass = TotalMangroveDensity * SurveyedBiomass /SurveyedMangroveDensity) %>%
  mutate(SiteYear = ifelse(Year2=="1986", "Rehab_1986", 
                           ifelse(Year2=="2002", "Rehab_2002", "Rehab_2006"))) 



#Check data distribution and run plant_stock model:
#Relevel factors to start off from the youngest SiteYear= 2006:
plant_stock$SiteYear <- factor(plant_stock$SiteYear,levels = c("Rehab_2006","Rehab_2002", "Rehab_1986" ) )
hist(plant_stock$TotalBiomass, data = plant_stock)

#Relevel factors to start off from the youngest SiteYear= 2006:
plant_stock$SiteYear <- factor(plant_stock$SiteYear,levels = c("Rehab_2006","Rehab_2002", "Rehab_1986" ) )
plant_model <- lm (TotalBiomass ~ SiteYear, data = plant_stock)
summary(plant_model )
tab_model(plant_model )  

