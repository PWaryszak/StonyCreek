library(ggplot2)
library(readxl)
library(reshape2)
library(plyr)

# Start the code
rm(list = ls(all = TRUE))

#Set the main directory of the folder with all the data:
MainDir = setwd("~/00DeakinUni/R/BCL_R/StonyCreek")
setwd(MainDir)

#Get the data:=====
Data = read_excel("MangroveVego_12Feb2020.xlsx", sheet = "Mangrove_Quadrat")
str(Data)
Data$Year = as.factor(Data$Year)
Data$Elevation2 = factor(Data$Elevation, levels = c("Low", "High"))


#Plot pneumatophores========
#By Elevation Heading:
ggplot(data = subset(Data, !is.na(Elevation2)), aes(x = Year, y = Pneumatophore_density_root_count)) +
geom_point(size = 2, alpha = 0.2) +
facet_grid(~Elevation2) +
stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
stat_summary(fun.y = "mean", size = 3, geom = "point")

ggsave("Pneumatophores1.jpg",  height = 6.3, width = 8)

#By Year in Heading:
ggplot(data = subset(Data, !is.na(Elevation2)), aes(x = Elevation2, y = Pneumatophore_density_root_count)) +
geom_point(size = 2, alpha = 0.2) +
facet_grid(~Year) +
stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
stat_summary(fun.y = "mean", size = 3, geom = "point")
ggsave("Pneumatophores2.jpg", height = 6.3, width = 8)


#Plot seedlings height=======
Data2 = Data[, 1:47]
Data3 = na.omit(melt(Data2, id = names(Data2)[1:8]))

ggplot(data = Data3, aes(x = Elevation, y = value)) +
geom_point(size = 2, alpha = 0.2) +
facet_grid(~Year) +
stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1) +
stat_summary(fun.y = "mean", size = 3, geom = "point") +
ylab("Seedling height (cm)")
ggsave("Seedling height.jpg", height = 6.3, width = 8)














