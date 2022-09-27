## code to pull ESRI data from FGP and convert to sf objects

#install 'github.com/yonghah/esri2sf'
library(remotes)
install_github("yonghah/esri2sf")

#load libraries
library(esri2sf)

#example FGP esri URL that can be used 
esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/DFO_MPO_Regions/MapServer/"

#pull the file down and convert to sf - here the 0 refers to the layer you need to grab, these should be listed on FGP.
regions_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"))