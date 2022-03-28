#This script preprocesses all necessary data sets (Sentinel1, SWI, NDVI, NDWI, TWI, all derivatives of the digital terrain model).
#Pre-processing includes: crop, resample and reproject.

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

#preprocessing Sentinel1 and SWI-Data
rm(list = ls())
setwd("F:\\loew\\")
#load area of investigation
aoi <- readOGR("extent\\extent_wgs.shp")
#list all available swi files and extract date information (from file name)
swi_data_pre <- data.frame(file = list.files("soil_mois\\2019\\Soilmoisture-Tiffs", pattern = "*.tif$", recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
swi_data_pre$date <-substr(swi_data_pre$file, nchar(swi_data_pre$file)-24, nchar(swi_data_pre$file)-17) #23/16 zeichenstellen
#list all available s1k0 files and extract date information (from file name)
s1ko_data_pre <- data.frame(file = list.files("sentinel1\\scenes", pattern = glob2rx("*.tif$"), recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
s1ko_data_pre$date <- substr(s1ko_data_pre$file, nchar(s1ko_data_pre$file)-44, nchar(s1ko_data_pre$file)-37)  # 44 /37 zeichennummern
#merge file lists of swi and s1k0 data --> find dates for which both data is available
merged_data_pre <- merge(swi_data_pre, s1ko_data_pre, by = "date")
# get extent from area of investigation 
ug_rast <- aoi
ug_ext <- extent(ug_rast)

#crop, resample and reproj. swi and sent (for all files in merged data) 
for( i in 1:nrow(merged_data_pre)){
  #create temp. raster for each row from merged_data_pre
  temp_swi_rast <- raster(merged_data_pre[i,2])
  temp_sent_rast <- raster(merged_data_pre[i,3])
  #store names
  name_sent = merged_data_pre[i,3]
  name_swi =merged_data_pre[i,2]
  #crop temp_raster_data with extent
  temp_swi_crop <- crop(temp_swi_rast, extent(ug_ext))
  temp_sent_crop <- crop(temp_sent_rast, extent(ug_ext), snap="in") ## obacht, weil pixel
  #save Extent from first file => metaextent for all files 
  if(i==1){
    res_extent <- temp_sent_crop
  }
  #resample on metaextent
  temp_swi_crop <- raster::resample(temp_swi_crop, res_extent)
  temp_sent_crop <- raster::resample(temp_sent_crop, res_extent)
  #change to utm 33 proj. and set resolution to 500m (depends on computing power)
  temp_swi_crop <-  raster::projectRaster(temp_swi_crop ,  res = 500 ,crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  temp_sent_crop <-  raster::projectRaster(temp_sent_crop, res = 500,crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  #check if raster has error values
  if(is.numeric(minValue(temp_swi_crop))){
    print("Raster besitzt Werte")
    #define area from first file with values as meta_area
    if(i==1){
      meta_area =  gArea(rasterToPolygons(temp_swi_crop))
      print(meta_area)
    }
    #get area form temp swi file
    swi_area <- gArea(rasterToPolygons(temp_swi_crop))
    #export if swi_area has the same size like the meta_area
    if(swi_area == meta_area){
      print("gleiche Flaechen")
      #ignore the swi-duplicate, export only 1 per date (cause of the 2 sentinel images per date in merged_data)
      if(i%%2 == 1){
        writeRaster(temp_swi_crop, paste("crop_swi_2\\", toString(i), "_", substr(name_swi, nchar(name_swi)-24, nchar(name_swi)-17), "_swi.tif", sep=""),  filetype = "GTiff", overwrite=TRUE)   
      }
      writeRaster(temp_sent_crop, paste("crop_sent_2\\",toString(i), "_", substr(name_sent, nchar(name_sent)-44, nchar(name_sent)-37), "_sent.tif", sep=""),  filetype = "GTiff", overwrite=TRUE)
      print("exportiert")
    }
    else {print("Flächeninhalt nicht gleich groß, nächstes File")}
  }
  else{
    print("Fehlerhafte Daten, nächtes File")
  }
}

#####Pre-processing of the other data sets

rm(list = ls()) 
#load all data
aspect <- brick("neuesdgm\\aspect_ext.tif")
convergence <- brick("neuesdgm\\konvergenz_2.tiff")
elevation <- brick("neuesdgm\\DGM_EXTENT.tif")
slope <- brick("neuesdgm\\slope_ext.tif")
twi <- brick("neuesdgm\\TWI.tif")
ndvi_mean <- brick("GEE_INDEXE\\Anual_Mean_Sentinel_NDVI_2019.tif")
ndvi_std <- brick("GEE_INDEXE\\Anual_StdDev_Sentinel_NDVI_2019.tif")
ndwi_mean <- brick("GEE_INDEXE\\Anual_Mean_Sentinel_NDWI_2019.tif")
ndwi_std <- brick("GEE_INDEXE\\Anual_StdDev_Sentinel_NDWI_2019.tif")

#get extent and change proj to wgs84
sent_resampl <- brick("crop_sent_2\\1_20190110_sent.tif") #load metafile for extent 
sent_resampl<-  raster::projectRaster(sent_resampl  ,crs =("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) #change proj,
ext_resampl <- extent(sent_resampl) #get extent
#crop to extent 
convergence <- raster::crop(convergence, ext_resampl)
aspect <- raster::crop(aspect, ext_resampl)
slope  <- raster::crop(slope, ext_resampl)
elevation  <- raster::crop(elevation, ext_resampl)
ndvi_mean <- raster::crop(ndvi_mean, ext_resampl)
ndvi_std<- raster::crop(ndvi_std, ext_resampl)
ndwi_mean <- raster::crop(ndwi_mean, ext_resampl)
ndwi_std  <- raster::crop(ndwi_std, ext_resampl)
twi<- raster::crop(twi, ext_resampl)
#resample
elevation <- raster::resample(elevation, sent_resampl)
convergence <- raster::resample(convergence, sent_resampl)
aspect <- raster::resample(aspect, sent_resampl)
slope  <- raster::resample(slope , sent_resampl)
ndvi_mean <- raster::resample(ndvi_mean, sent_resampl)
ndvi_std<- raster::resample(ndvi_std, sent_resampl)
ndwi_mean <- raster::resample(ndwi_mean, sent_resampl)
ndwi_std  <- raster::resample(ndwi_std, sent_resampl)
twi<- resample(twi, sent_resampl)
#stack all and change proj to utm 33n 
constant_stack <- stack(aspect, convergence, elevation, slope,twi,ndvi_mean ,ndwi_mean ,ndvi_std, ndwi_std)
constant_stack <- raster::projectRaster(constant_stack ,crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
utm_resampl <- brick("crop_sent_2\\1_20190110_sent.tif")
constant_stack <- resample(constant_stack, utm_resampl)
##export data
out_dir <- "output_preprocessing"
query_dir <- dir.exists(out_dir)
if(query_dir==F) dir.create(out_dir)
#vector of all datanames
data_name <- c("aspect", "convergence", "elevation", "slope","twi","ndvi_mean" ,"ndwi_mean" ,"ndvi_std", "ndwi_std")
#export all files as tif 
for(i in 1:nlayers(constant_stack)){
  #temp_name= names(constant_stack[[i]]) #names will be like the inputfiles at load all
  temp_name <- (data_name[i])
  writeRaster(constant_stack[[i]],paste("output_preprocessing\\", temp_name,".tif", sep=""),filetype = "GTiff", overwrite=TRUE) 
  print(paste(temp_name, "exportiert", sep =" "))
}


