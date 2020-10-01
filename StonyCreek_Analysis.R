#LOAD data Off Stony Creek =====
library(readxl)
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(car)

rm(list = ls(all = TRUE))#Clean R Environment


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
range(NewDATA$CarbonStock.Mgha, na.rm = T ) # 0.6328892 239.2190928
range(NewDATA$CarbonDensity.gcm3, na.rm = T ) #0.0009671682 0.0956876371

#PLOT SOIL========
#Soil Carbon to horizon (DepthAtRehab_cm:)
#1986 core: ~9 mm a year. This means 1986 is about 31.85 cm. BAsd on SAR*AGE (35years)
#1996 core: ~14cm is 1996. Based on PERE's model
#2006 core: ~8cm is 2006. Based on PERE's model

#Cut core till DepthAtRehab_cm and estimate CarbonStockTillRehab_Mgha:
CarbonTillRehab_slices <- NewDATA %>% filter(Elevation != "Lowest") %>% #This one was weird plot, not in design
  mutate(DepthTo_SinceRehabilitated_cm = ifelse(SiteYear ==  "Stony1986",31.85,
                                                ifelse(SiteYear ==  "Stony1996", 14, 8)))%>%
  mutate(KeepThrow = ifelse(DepthTo_SinceRehabilitated_cm >= DepthFrom.cm, "keep", "throw")) %>% #keep slices data are > Depth_from
  filter(KeepThrow=="keep") %>% #keep the "keep"
  transform (DepthAtRehab_cm = ifelse(DepthTo.cm <= DepthTo_SinceRehabilitated_cm,  #Cut to the length acc to DepthTo_SinceRehabilitated_cm
                                      DepthTo.cm, DepthTo_SinceRehabilitated_cm)) %>%
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - DepthFrom.cm) %>% #lenght of slice at cores up to DepthTo_SinceRehabilitated_cm
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm,  #Soilc C stock in cores till DepthTo_SinceRehabilitated_cm
          StockType = "Soil")

#1. Analysis of soil C variation========
#variation in  soil carbon stocks across three rehabilitated/reestablished mangrove sites
#Aug 2020 UPDATE:
CarbonTillRehab_cores <- CarbonTillRehab_slices %>%
  group_by(Plot_ID) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStockTillRehab_Mgha, na.rm = T)) %>% #Sum stock per core till rehab horizon
  
  separate(Plot_ID, into = c("SiteYear","Transect" ,"Elevation"), sep = "_") %>% #split to get more variables to compute CAR
  mutate(NewYear = as.factor( ifelse(SiteYear=="Stony1986", 1984, 
                          ifelse(SiteYear=="Stony1996", 2002, 2006))))

CarbonTillRehab_cores$SiteYear <- factor(CarbonTillRehab_cores$SiteYear,
                                         levels = c("Stony1986", "Stony1996","Stony2006"))
class(CarbonTillRehab_cores$SiteYear)
soil_model <- lm (TotalCarbonStockPerCore ~ SiteYear, data = CarbonTillRehab_cores )
summary(soil_model )
tab_model(soil_model)  

#Check ANOVA assumptions:
leveneTest(core_total ~ SiteYear, data = soil_stock)#PASSED!  


#Some o these libraries caused glitch in lm above (Don't run if no need)
library(apaTables)#See WEB: https://cran.r-project.org/web/packages/apaTables/vignettes/apaTables.html
library(MBESS)
library(car)#provides leveneTest function
library(gridExtra)

#Produce ANOVA-style output table:
options(contrasts = c("contr.sum", "contr.poly"))
apa.aov.table(soil_model, filename="Table1_APA.doc")




#2. Analysis of plant abovegr C =======
library(readxl)
library(tidyverse)
library(sjPlot)
library(sjmisc)

rm(list = ls(all = TRUE))#Clean R Environment

#LOAD DATA:
Plant_Data = read_excel("MangroveVego_12Feb2020.xlsx", sheet = "Mangrove_Plots")
Plant_Data <- filter(Plant_Data, Elevation != "Lowest" & Elevation != "SM")  #Lowest $ SM (Saltmarhs) is cut-out as it was an extra done by Peter/Maria.
Plant_Data$Elevation2 = factor(Plant_Data$Elevation, levels = c("High", "Low"))
names(Plant_Data)

#Allometric equation from Saintilan (1997)
#Can be applied to smaller trees as opposed to other equations:
Plant_Data$PlantBiomass <-  Plant_Data$Canopy_Height_m * (((0.214 * (Plant_Data$Circumf_cm_at_base_1) - 0.113)) ^2) /10

#Sum up MangroveBiomass for fully survedy plots and extrapolate for not-fully surveyd 
plant_stock <- Plant_Data %>%
  #Some plots were not fully surveydP All_Ind_Surveyd = "no", hence we need to  extrapolote
  #If TotalMangroveDensity is > length(PlantBiomass)
  group_by(Plot_ID,Year2) %>% #Sum up biomass by Plot_ID (observational unit)
  summarise(TotalMangroveDensity = mean(Mangrove_density_25m2),
            SurveyedMangroveDensity = length(PlantBiomass),
            PlantCarbon_gm2 = sum(PlantBiomass, na.rm=T)/25 *0.464, #converting biomass to C-stock
            TotalCarbonPerPlot = TotalMangroveDensity * PlantCarbon_gm2 /SurveyedMangroveDensity) %>%
  mutate(SiteYear = ifelse(Year2=="1986", "Rehab_1986", 
                           ifelse(Year2=="2002", "Rehab_2002", "Rehab_2006"))) 


View(plant_stock )
#write.csv(plant_stock, file = "StonyPlantBiomass.csv", row.names = F)

#Check data distribution and run plant_stock model:
#Relevel factors to start off from the youngest SiteYear= 2006:
plant_stock$SiteYear <- factor(plant_stock$SiteYear,levels = c("Rehab_2006","Rehab_2002", "Rehab_1986" ) )
hist(plant_stock$TotalCarbonPerPlot, data = plant_stock)#NOT-NORMAL
hist(log(plant_stock$TotalCarbonPerPlot), data = plant_stock)#ok-ISH

#Relevel factors to start off from the youngest SiteYear= 2006:
plant_stock$SiteYear <- factor(plant_stock$SiteYear,levels = c("Rehab_2006","Rehab_2002", "Rehab_1986" ) )
plant_model <- lm (log(TotalCarbonPerPlot) ~ SiteYear, data = plant_stock)
tab_model(plant_model ) 

#Paul likes it ANOVA style:
summary(aov(log(TotalCarbonPerPlot) ~ SiteYear, data = plant_stock))

#No log model:
plant_stock$SiteYear <- factor(plant_stock$SiteYear,levels = c("Rehab_2006","Rehab_2002", "Rehab_1986" ) )
plant_model <- lm (TotalCarbonPerPlot ~ as.factor(SiteYear), data = plant_stock)
tab_model(plant_model ) 

#Check ANOVA assumptions (web:http://www.sthda.com/english/wiki/one-way-anova-test-in-r)
leveneTest(TotalCarbonPerPlot ~ SiteYear, data = plant_stock)#Failed
leveneTest(log(TotalCarbonPerPlot) ~ SiteYear, data = plant_stock)#passed


#Produce ANOVA-style output table:
summary(aov (TotalCarbonPerPlot ~ as.factor(SiteYear), data = plant_stock))
options(contrasts = c("contr.sum", "contr.poly"))
apa.aov.table(plant_model, filename="PLANT_ANOVA_TABLE2.doc")



#3. Analysis of GAS=======
#LOAD DATA and libraries:
#library(ggplot2); library(plyr); library(grid);library(stringr);library(gdata);library(reshape2);library(splitstackshape)
library(tidyverse)
rm(list = ls(all = TRUE))#Clean R Environment

# Set the SiteYear of the folder with all the data first:
gas_data <- read.csv(file = "AllSlopes_comb_Aug2020.csv")%>%
  mutate(Site = "Stony") %>%
  unite(SiteYear,c("Site","Location"), remove = FALSE, sep="") %>%
  mutate(NewYear = ifelse(SiteYear=="Stony1986", 1984, 
                          ifelse(SiteYear=="Stony1996", 2002, 2006))) #Compute Stock Age (Time_Since_Planting)

#PLOT:
ggplot(data = gas_data, aes(y = Flux_mgm2day, x = as.factor(SiteYear), col = as.factor(SiteYear))) +
  #geom_point(size = 3, alpha = 0.3) +
  facet_grid(Gas~., scale = "free_y") +
  guides(col = F) +
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
  stat_summary(fun.y = "mean", size = 5, geom = "point") +
  labs(x="", y = bquote('Gas flux  ' (mg*~m^-2~day^-1 )))+
  theme_bw()+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

#ggsave("StonyCreekGas.jpg", height = 6.733945, width = 11.577981)

#Get AV+-SE for Stacey:=======
stt <- gas_data %>% 
  
  #select(Gas, SiteYear, Flux_mgm2day)%>%
  group_by(SiteYear, Gas,NewYear) %>%
  summarise(AV = mean(Flux_mgm2day, na.rm = T),
            N = length(Flux_mgm2day),
            SD = sd(Flux_mgm2day, na.rm = T),
            SE = SD/sqrt(N), na.rm = T)
stt
#write.csv(stt, file = "StonyCreekGasData_Aug2020.csv", row.names = F) 


#Look at r2:
gas_data_r2 <- gas_data %>%
  mutate(R_square_value = ifelse(R2  >0.7, "good", "crap"))

ggplot(data = gas_data_r2, aes(y = Flux_mgm2day, x = SiteYear)) +
  geom_boxplot()  +
  geom_jitter(aes(col = R_square_value),size=3)+
  facet_grid(.~Gas) +
  ggtitle("Gas Fluxes at Stony, crap when R2 <0.7")+ theme_bw()

#CO2:####################
CO2 <- filter(gas_data, Gas=="CO2")
#View(CO2)
#Check normality:
hist(CO2$Flux_mmolm2day)#close to NORMAL. Check with Levene's Test:
leveneTest(Flux_mmolm2day ~ SiteYear, data = CO2)#Passed when P>0.05

#Run CO2_model:
CO2$SiteYear <- factor(CO2$SiteYear, levels = c("Stony2006", "Stony1996", "Stony1986"))
CO2_model <- lm (Flux_mmolm2day ~ SiteYear, data = CO2)
tab_model(CO2_model)

summary(aov(Flux_mmolm2day ~ SiteYear, data = CO2))
############Df Sum Sq Mean Sq F value Pr(>F)  
#SiteYear     2   3066  1532.8   6.452 0.0113 *
  
#Produce ANOVA-style output table:
options(contrasts = c("contr.sum", "contr.poly"))
apa.aov.table(CO2_model, filename="CO2_ANOVA_TABLE.doc")



#CH4:#####################
CH4 <- filter(gas_data, Gas=="CH4")

#Check normality:
hist(CH4$Flux_mmolm2day)#close to NORMAL. Check with Levene's Test:
leveneTest(Flux_mmolm2day ~ SiteYear, data = CH4)#Passed when P>0.05

#Run CH4_model:
CH4$SiteYear <- factor(CH4$SiteYear, levels = c("Stony2006", "Stony1996", "Stony1986"))
CH4_model <- lm (Flux_mmolm2day ~ SiteYear, data = CH4)
tab_model(CH4_model) 
summary(aov (Flux_mmolm2day ~ SiteYear, data = CH4))
############Df Sum Sq Mean Sq F value  Pr(>F)   
#SiteYear   2 0.3271 0.16356   8.442 0.00447 **

#Produce ANOVA-style output table:
options(contrasts = c("contr.sum", "contr.poly"))
apa.aov.table(CH4_model, filename="CH4_ANOVA_TABLE.doc")


#4. CAR analysis (mixed SAR/Pere method):========
rm(list = ls(all = TRUE))#Clean R Environment
library(tidyverse)
library(readxl)
library(sjPlot)
#LOAD DATA for CAR:
Soil_Data = read_excel("MangroveVego_12Feb2020.xlsx", sheet = "Organic_Carbon")
Soil_Data$Year = as.factor(Soil_Data$Year)
Soil_Data$Elevation2 = factor(Soil_Data$Elevation, levels = c("High", "Low","Lowest"))
Soil_Data <- filter(Soil_Data, Elevation != "Lowest" & Elevation != "SM")  #Lowest $ SM (Saltmarhs) is cut-out as it was an extra done by Peter/Maria.


##Compute Mg/ha using OC(%):
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

##Soil Carbon to horizon (DepthAtRehab_cm):

NewDATA$NewYear <- ifelse(NewDATA$SiteYear == "Stony1996", 2002, 
                          ifelse(NewDATA$SiteYear == "Stony1986", 1984, 2006))

#Get MAR (Mass Accretion Rates) off PB210 age dating data set (MAR = g/cm2/year):
#Get SAR (Sediment Accretion Rates, mm/year)
MAR <- data.frame( SiteYear = c( "Stony1986" ,"Stony1996" ,"Stony2006"),
                   MAR =    c(0.22, 0.14, 0.096),
                   MAR_SE = c(0,    0.02, 0.007),
                   SAR =    c(9.1,  6.8,  4 ),
                   SAR_SE = c(0,    0.9,  0.3 ) )


StonyData <- left_join(NewDATA,MAR, by = "SiteYear") #Join MAR and NewDATA
StonyData$Time_Since_Planting <- 2019 - StonyData$NewYear #Estimate time since rehab
StonyData$DepthTo_SinceRehabilitated_cm <- StonyData$SAR * StonyData$Time_Since_Planting/10 #estimate depth till rehab
#Pere estimated Depths for two sites but 1986:
#CORRECT YEARS OF REHAB!!!!
#1986 core: ~9 mm a year. Acc to article Maria shared it is 1984
#1996 core: ~14cm is 1996. Ac to images it is more of 2002. 14 cm comes from model by Pere
#2006 core: ~8cm is 2006 . Correct. 8 cm comes from model by Pere

StonyData$DepthTo_SinceRehabilitated_cm_pere <- ifelse(StonyData$SiteYear ==  "Stony1996", 14,
                                                       ifelse(StonyData$SiteYear ==  "Stony2006", 8 ,
                                                              StonyData$SAR * StonyData$Time_Since_Planting/10))#StonyData$SAR * StonyData$Time_Since_Planting/10 #estimate depth till rehab
unique(StonyData$DepthTo_SinceRehabilitated_cm_pere)#31.85 14.00  8.00

#MIXED-CAR-method (CAR based on SAR for 1986, 2002 & 2006 based on Pere's spreadsheet): 
#Cut core slices till DepthAtRehab_cm and estimate CarbonStockTillRehab_Mgha:
CarbonTillRehab_pere <- StonyData %>% 
  filter(Elevation != "Lowest") %>% #This one was weird plot, not in design
  mutate(KeepThrow = ifelse(DepthTo_SinceRehabilitated_cm_pere >= DepthFrom.cm, "keep", "throw")) %>% #keep slices data are > Depth_from
  filter(KeepThrow=="keep") %>% #keep the "keep"
  transform (DepthAtRehab_cm = ifelse(DepthTo.cm <= DepthTo_SinceRehabilitated_cm_pere,  #Cut to the length acc to DepthTo_SinceRehabilitated_cm_pere
                                      DepthTo.cm, DepthTo_SinceRehabilitated_cm_pere)) %>%
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - DepthFrom.cm) %>% #lenght of slice at cores up to DepthTo_SinceRehabilitated_cm_pere
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm,  #Soilc C stock in core slices till DepthTo_SinceRehabilitated_cm_pere
          StockType = "Soil")

#Compute Mean Soil Carbon Stock across sites and then CAR (Stock/Stock age):
CarbonTillRehab_sites_pere <- CarbonTillRehab_pere %>%
  group_by(Plot_ID) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStockTillRehab_Mgha, na.rm = T))%>% #Sum stock per core till rehab horizon
  
  separate(Plot_ID, into = c("SiteYear","Transect" ,"Elevation"), sep = "_") %>% #split to get more variables to compute CAR
  mutate(NewYear = ifelse(SiteYear=="Stony1986", 1984, 
                          ifelse(SiteYear=="Stony1996", 2002, 2006))) %>%  #Compute Stock Age (Time_Since_Planting)
  
  group_by(SiteYear) %>% #to sum total Carbon Stock per Site
  summarise(AV = mean(TotalCarbonStockPerCore, na.rm = T),
            SD = sd(TotalCarbonStockPerCore, na.rm = T),
            N = length(TotalCarbonStockPerCore),
            SE = SD / sqrt(N))

CarbonTillRehab_sites_pere
#SiteYear     AV    SD     N    SE
# Stony1986  92.4  9.35     6  3.82
# Stony1996  48.7  9.84     6  4.02
# Stony2006  30.1 13.1      6  5.36

#SAVE:
#write.csv(CarbonTillRehab_sites_pere, file = "Stock_TillRehab_Stony_Pere.csv", row.names = F)

#LM on CAR:
CAR_TillRehab_lm <- CarbonTillRehab_pere %>%
  group_by(Plot_ID) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStockTillRehab_Mgha, na.rm = T))%>% #Sum stock per core till rehab horizon
  
  separate(Plot_ID, into = c("SiteYear","Transect" ,"Elevation"), sep = "_") %>% #split to get more variables to compute CAR
  mutate(NewYear = ifelse(SiteYear=="Stony1986", 1984, 
                          ifelse(SiteYear=="Stony1996", 2002, 2006))) %>%  #Compute Stock Age (Time_Since_Planting)
  
  mutate(Time_Since_Planting = 2019 - NewYear,
         CAR = TotalCarbonStockPerCore / Time_Since_Planting)

hist(CAR_TillRehab_lm$CAR)
car_model <- lm (CAR ~ SiteYear, data = CAR_TillRehab_lm)
tab_model(car_model ) 
summary(aov(CAR ~ SiteYear, data = CAR_TillRehab_lm))

#Produce ANOVA-style output table:
#Some o these libraries caused glitch in lm above (Don't run if no need)
library(apaTables)#See WEB: https://cran.r-project.org/web/packages/apaTables/vignettes/apaTables.html
library(MBESS)
library(car)
library(gridExtra)

options(contrasts = c("contr.sum", "contr.poly"))
apa.aov.table(car_model, filename="CAR_model_ANOVA_TABLE.doc")
#Check ANOVA assumptions:
leveneTest(CAR ~ SiteYear, data = CAR_TillRehab_lm)#PASSED  ( if p < 0.05).

