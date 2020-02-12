library(readxl)
library(tidyverse)
library(lubridate)#install.packages ("lubridate")

# Start the code
rm(list = ls(all = TRUE))

MainDir = setwd("~/00DeakinUni/R/BCL_R/StonyCreek")
setwd(MainDir)

# Get the data=====

#COMPUTE EMISSION RATE OFF UGGA txt FILE =====
#Subset only pre-defined start and stop time (off our field notes).
ugga_data <- read.delim("gga_2019-07-03_f0000.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) 

#Specify path to folder where you store youre CN spreadsheets and list them all:
files <- list.files(path = "C:/Users/BlueCarbon/Documents/00DeakinUni/R/BCL_R/StonyCreek/GasRawData",
                    pattern = "*.txt", full.names = T)

#Create function to export data from sheet = 1 (Sample Table)
Read_Flux <- function(x) read.delim (file = x,
                                     sep =   ",",
                                     skip = 1)

#Export "Sample Table" data from all files in your folder:
gas <- sapply(files, Read_Flux, simplify=FALSE) %>%
  bind_rows(.id = "File_Path")

names(gas)
dim(gas)#28237    29

gas_data<-  gas %>%
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S")))%>%
  mutate(Site = ifelse(Datetime>='2019-07-03 08:47:40' & Datetime <='2019-07-03 08:54:32',"Stony1996_T3_Low",
                       ifelse(Datetime >='2019-07-03 08:08:08' & Datetime <='2019-07-03 08:14:49',"Stony1996_T3_High",
                              "BLANK"))) %>%
  filter (Site != "BLANK") %>% #remove all records in between chambers.  I called them BLANK
  group_by(Site) %>%
  mutate (NumTime = seq_along(Site))%>% #produce numeric values for time to run lm on.
  mutate (Slope = coef(summary(lm (X.CO2._ppm ~ NumTime)))[2,1],
          R_square = summary(lm (X.CO2._ppm ~ NumTime))$r.squared) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need to change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000))

unique(gas_data$Flux)

#Draw the plot for SITE1:
ggplot(ugga_data[ugga_data$Site=="SITE1",] , aes(x = NumTime, y = X.CO2._ppm)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

#Look at CO2 emission against two time intervals ============
library(grid)
library(gridExtra)

#Points around NumTime = 75 fluctute. Let's cut them out:
ugga_data_old <- filter(ugga_data, Site =="SITE1")
ugga_data_new <- filter(ugga_data, NumTime >= 75 & Site =="SITE1")

#Compare different plots using this function:
ggplotRegression <- function (fit) {
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                          "Intercept =",signif(fit$coef[[1]],5 ),
                          " Slope =",signif(fit$coef[[2]], 5),
                          " P =",signif(summary(fit)$coef[2,4], 5)))
}

#Compare plots:
u1<- ggplotRegression(lm(X.CO2._ppm ~ NumTime, data = ugga_data_old)) +labs(title = "SITE1_OLD")
u2<- ggplotRegression(lm(X.CO2._ppm ~ NumTime, data = ugga_data_new))+labs(title = "SITE1_NEW")
grid.arrange(u1,u2) #Very small difference in R2
