library(shiny)
library(rgdal)
library(rgeos)
library(tmap)
library(leaflet)
library(vioplot)


CamdenData <- readOGR(".","Camden")

vioplot(CamdenData$Unmplyd, CamdenData$Qulfctn, 
        CamdenData$Wht_Brt, CamdenData$Lw_Occp, 
        ylim=c(0,100), col = "dodgerblue", rectCol="dodgerblue3", 
        colMed="dodgerblue4", names=c("Unemployed", "Qualifications", 
                                      "White British", "Occupancy"))


summary(CamdenData@data[,2:5])
