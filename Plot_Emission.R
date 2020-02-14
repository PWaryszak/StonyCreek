library(tidyverse)
library(lubridate)#install.packages ("lubridate")
#Start the code
rm(list = ls(all = TRUE))
MainDir = setwd("~/00DeakinUni/R/BCL_R/StonyCreek")
setwd(MainDir)

#COMPUTE EMISSION RATE OFF UGGA txt FILE =====
#Stony Creek UGGA data======
pawel <- read.delim("Pawel_gga_2019-07-03_f0000.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) 

maria0 <- read.delim("Maria_gga_2019-07-03_f0000.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) 
maria1 <- read.delim("Maria_gga_2019-07-03_f0001.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) 
maria2 <- read.delim("Maria_gga_2019-07-03_f0002.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) 
maria3 <- read.delim("Maria_gga_2019-07-03_f0003.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) 
maria4 <- read.delim("Maria_gga_2019-07-03_f0004.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) 
maria5 <- read.delim("Maria_gga_2019-07-03_f0005.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) 

maria <- rbind (maria0,maria1,maria2,maria3,maria4,maria5)
dim(maria)#14360    2

#MARIA======
gas_maria<-  maria %>%
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S")))%>%
  mutate(Site =         
           ifelse(Datetime >='2019-03-07 08:15:20' & Datetime <='2019-03-07 08:16:00', "Stony2006_T3_High",
                  ifelse(Datetime >='2019-03-07 08:42:00' & Datetime <='2019-03-07 08:52:00', "Stony2006_T3_Low", 
                         ifelse(Datetime >='2019-03-07 09:11:00' & Datetime<= '2019-03-07 09:20:59', "Stony2006_T2_High",                        
                                ifelse(Datetime >='2019-03-07 09:35:00' & Datetime <='2019-03-07 09:43:00', "Stony2006_T2_Low",  
                                       ifelse(Datetime >='2019-03-07 10:07:00' & Datetime <='2019-03-07 10:15:00', "Stony2006_T1_High",
                                              ifelse(Datetime >='2019-03-07 12:02:51' & Datetime <='2019-03-07 12:10:00', "Stony1986_T2_High",
                                                     ifelse(Datetime >='2019-03-07 12:34:35' & Datetime <='2019-03-07 12:39:56', "Stony1986_T3_High",
                                                            ifelse(Datetime >='2019-03-07 13:02:00' & Datetime <='2019-03-07 13:09:00', "Stony1986_T3_Low", "BLANK")))))))))%>% 
  
  filter(Site != "BLANK") %>% #remove in-betweens fluxes
  group_by(Site) %>%
  mutate (NumTime = seq_along(Site))%>% #produce numeric values for time to run lm on.
  mutate (Slope = coef(summary(lm (X.CO2._ppm ~ NumTime)))[2,1],
          R_square = summary(lm (X.CO2._ppm ~ NumTime))$r.squared) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.02604, #Before = 0.0353250 This may need to change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/(Chamber_Surface.m2*1000))%>%
  group_by(Site) %>%
  summarise(CO2_flux = mean (Flux),
            CO2_R2 = mean(R_square),
            CO2_slope = mean(Slope),
            MeanTime = mean(Datetime)) %>%
  mutate(R_square_value = ifelse(CO2_R2  >0.6, "good", "crap")) %>%
  separate(Site, into = c("Year","Transect","Elevation"), sep = "_", remove = FALSE)

View(gas_maria)



#PAWEL=====

gas_pawel<-  pawel %>%
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S")))%>%
  
  mutate(Site =         ifelse(Datetime >='2019-07-03 08:08:08' & Datetime <='2019-07-03 08:14:49',"Stony1996_T3_High",
                               ifelse(Datetime >='2019-07-03 10:14:19' & Datetime <='2019-07-03 10:25:06',"Stony1996_T2_High",
                                      ifelse(Datetime >='2019-07-03 09:08:22' & Datetime <='2019-07-03 09:15:10',"Stony1996_T2_Low",
                                             ifelse(Datetime >='2019-07-03 11:27:00' & Datetime <='2019-07-03 11:33:01',"Stony1986_T2_Low",
                                                    ifelse(Datetime >='2019-07-03 09:36:44' & Datetime <='2019-07-03 09:44:57',"Stony1996_T1_Low",
                                                           ifelse(Datetime >='2019-07-03 09:59:25' & Datetime <='2019-07-03 10:05:00',"Stony1996_T1_High",
                                                                  ifelse(Datetime >='2019-07-03 10:46:42' & Datetime <='2019-07-03 10:54:17',"Stony1986_T1_High",
                                                                         ifelse(Datetime >='2019-07-03 11:10:29' & Datetime <='2019-07-03 11:16:39',"Stony1986_T1_Low","BLANK"))))))))) %>%
  
  filter(Site != "BLANK") %>% #remove in-betweens fluxes
  group_by(Site) %>%
  mutate (NumTime = seq_along(Site))%>% #produce numeric values for time to run lm on.
  mutate (Slope = coef(summary(lm (X.CO2._ppm ~ NumTime)))[2,1],
          R_square = summary(lm (X.CO2._ppm ~ NumTime))$r.squared) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.02604, #Before = 0.0353250 This may need to change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/(Chamber_Surface.m2*1000))%>%
  group_by(Site) %>%
  summarise(CO2_flux = mean (Flux),
            CO2_R2 = mean(R_square),
            CO2_slope = mean(Slope),
            MeanTime = mean(Datetime)) %>%
  mutate(R_square_value = ifelse(CO2_R2  >0.6, "good", "crap")) %>%
  separate(Site, into = c("Year","Transect","Elevation"), sep = "_", remove = FALSE)

stony_fluxes <- rbind(gas_maria,gas_pawel)
View(stony_fluxes)


 ggplot(stony_fluxes, aes(x=Site, y=CO2_flux,fill=Year,color=Year, shape =R_square_value))+
  geom_segment( aes(x=Site, xend= Site, y=0, yend=CO2_flux), color = "black")+
  geom_point(size=4) +
  #scale_x_discrete(label=abbreviate,1)+
  #scale_y_continuous(limits = c(0,6))+
  theme_bw() +
  labs(x= "", y =  bquote("CO"[2] ~ "flux " ~(mg*~m^-2 ~day^-1)))+ 
  facet_wrap(~Elevation)+
  scale_shape_manual(values = c(4,21))+
  theme(axis.text.y=element_text(size =10),
        axis.title.x=element_text(size =14),
        axis.text.x=element_text(size =8,angle=90),
        axis.title.y=element_text(size =14),
        strip.text = element_text(size=16),
        legend.position = "right")

ggsave("GasFluxesStonyCreek.jpg",  height = 6.3, width = 8)
 

#Loop function to collate all ugga files=======
#To work on. 
#Specify path to folder where you store youre CN spreadsheets and list them all:
files <- list.files(path = "C:/Users/BlueCarbon/Documents/00DeakinUni/R/BCL_R/StonyCreek/GasRawData",
                    pattern = "*.txt", full.names = T)
files
#Create function to export data from sheet = 1 (Sample Table)
Read_Flux <- function(x) read.delim (file = x,
                                     sep =  ",",
                                     skip = 1)

#Export "Sample Table" data from all files in your folder:
gas <- sapply(files, Read_Flux, simplify=FALSE) %>%
  bind_rows(.id = "File_Path")

#Create a list to loop over for future data management:

Output <- NULL #we need to set an empty shelf for data called Output

for ( i in 1:length(MyList) ){
  #create a subset data
  Start<- gas_maria[TimeStart>=MyList(i)$TimeSTart(1)]
  End <- gas_maria[TimeEnd<=MyList(i)$TimeSTart(1)]
  
  Output <- rbind(Output, saveoutput)
}

Output

