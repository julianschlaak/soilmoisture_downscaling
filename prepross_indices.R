#ndvi croppen
library(rgdal)
library(raster)
if (!require(terra)) install.packages('terra')
if (!require(tmap)) install.packages('tmap')
if (!require(lubridate)) install.packages('lubridate')
library(delayedMatrixStats)
library(ggplot2)
library(ncdf4)
library(rnaturalearth)
library(rnaturalearthdata)
library(lubridate)
library(PCICt)
library(delayed)
if (!require(terra)) install.packages('rgeos')
library(rgeos)
# prepare working directory
rm(list = ls())
setwd("D:\\Schlaak\\Soilmoisutre_downscaling\\")

aoi <- readOGR("extent\\extent_wgs.shp")
#ug <- readOGR("Extent_FC/Extent_FC.shp")
ndvi_data <- data.frame(file = list.files("NDVI\\", pattern = "*.tif$", recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
ndwi_data <- data.frame(file = list.files("NDWI\\", pattern = "*.tif$", recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)

ug_rast <- aoi
ug_ext <- extent(ug_rast)
meta_file_sent<-raster("crop_swi_10cm\\SWI_10_20190110_swi.tif")
#load real extent
real_ext <- readOGR("extent\\Messnetz_extent\\testius.shp")
richtiges_crs<- crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

real_extent = spTransform(real_ext,richtiges_crs )


for(i in 1:nrow(ndvi_data)){

  temp_ndvi <- raster(ndvi_data[i,1])
  temp_ndwi <- raster(ndwi_data[i,1])
  
  ndvi_name <- paste(substring(ndvi_data[i,1],6,9),substring(ndvi_data[i,1],11,12),substring(ndvi_data[i,1],14,15),sep="")
  ndwi_name <- paste(substring(ndwi_data[i,1],6,9),substring(ndwi_data[i,1],11,12),substring(ndwi_data[i,1],14,15),sep="")
  #ndwi_name <- paste(substring(ndwi_data[i,1],19,22),substring(ndwi_data[i,1],24,25),substring(ndwi_data[i,1],27,28),sep="")

  print("crop")
  temp_ndvi_crop <- crop(temp_ndvi, extent(ug_ext))
  temp_ndwi_crop <- crop(temp_ndwi, extent(ug_ext))
  
  print("project.")
  temp_ndvi_crop <-  raster::projectRaster(temp_ndvi_crop,res = 100 ,crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  temp_ndwi_crop <-  raster::projectRaster(temp_ndwi_crop,res = 100 ,crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  print("crop real extent")
  temp_ndvi_crop <- crop(temp_ndvi_crop , extent(real_extent))
  temp_ndwi_crop <- crop(temp_ndwi_crop , extent(real_extent))
  
  print("resampl")
  temp_ndvi_crop <- resample(temp_ndvi_crop, meta_file_sent)
  temp_ndwi_crop <- resample(temp_ndwi_crop, meta_file_sent)
  

  writeRaster(temp_ndvi_crop, paste("crop_ndvi\\",  ndvi_name, "_ndvi.tif", sep=""),  filetype = "GTiff", overwrite=TRUE)
  writeRaster(temp_ndwi_crop, paste("crop_ndwi\\",  ndwi_name, "_ndwi.tif", sep=""),  filetype = "GTiff", overwrite=TRUE)
  print(paste("exportiert", toString(i)))
}

