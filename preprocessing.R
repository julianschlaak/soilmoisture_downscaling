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
library(rgeos)
#preprocessing Sentinel1 and SWI-Data
rm(list = ls())
setwd("D:\\Schlaak\\Soilmoisutre_downscaling\\")
#create output dir
out_dir_swi <- "crop_swi_10cm"
query_dir <- dir.exists(out_dir_swi)
if(query_dir==F) dir.create(out_dir_swi)

out_dir_swi<- "crop_swi_40cm"
query_dir <- dir.exists(out_dir_swi)
if(query_dir==F) dir.create(out_dir_swi)

out_dir_sent <- "crop_sent"
query_dir <- dir.exists(out_dir_sent)
if(query_dir==F) dir.create(out_dir_sent)


#load area of investigation
aoi <- readOGR("extent\\extent_wgs.shp")
#list all available swi files and extract date information (from file name)
swi_data_pre <- data.frame(file = list.files("SWI_1km_nc", pattern = "*.nc", recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
swi_data_pre$date <- substr(swi_data_pre$file, nchar(swi_data_pre$file)-35, nchar(swi_data_pre$file)-28)


#old globals swi
#swi_data_pre$date <-substr(swi_data_pre$file, nchar(swi_data_pre$file)-24, nchar(swi_data_pre$file)-17) #23/16 zeichenstellen

#list all available s1k0 files and extract date information (from file name)
s1ko_data_pre <- data.frame(file = list.files("sentinel1\\scenes", pattern = glob2rx("*.tif$"), recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
s1ko_data_pre$date

s1ko_data_pre$date <- substr(s1ko_data_pre$file, nchar(s1ko_data_pre$file)-44, nchar(s1ko_data_pre$file)-37)  # 44 /37 zeichennummern
#merge file lists of swi and s1k0 data --> find dates for which both data is available
merged_data_pre <- merge(swi_data_pre, s1ko_data_pre, by = "date")
# get extent from area of investigation 
#get the ndvi 
ug_rast <- aoi
ug_ext <- extent(ug_rast)
#load real extent
real_ext <- readOGR("extent\\Messnetz_extent\\testius.shp")
richtiges_crs<- crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

real_extent = spTransform(real_ext,richtiges_crs )


#crop, resample and reproj. swi and sent (for all files in merged data) 
for( i in 1:nrow(merged_data_pre)){

  print(paste("File ",toString(i)))
  #create temp. raster for each row from merged_data_pre
  #temp_swi_rast <- raster(merged_data_pre[i,2])
  temp_sent_rast <- raster(merged_data_pre[i,3])
  swi_10cm_temp <- stack(merged_data_pre[i,2], varname = "SWI_010")
  swi_40cm_temp <- stack(merged_data_pre[i,2], varname = "SWI_040")
    
  values(swi_10cm_temp)<- values(swi_10cm_temp)*0.5
  values(swi_40cm_temp)<- values(swi_40cm_temp)*0.5
  #store names
  name_sent = merged_data_pre[i,3]
  name_swi =merged_data_pre[i,2]
  #crop temp_raster_data with extent
  #temp_swi_crop <- crop(temp_swi_rast, extent(ug_ext))
  temp_sw1_10_crop <- crop(swi_10cm_temp, extent(ug_ext))
  temp_sw1_40_crop <- crop(swi_40cm_temp, extent(ug_ext))
  temp_sent_crop <- crop(temp_sent_rast, extent(ug_ext)) ## obacht, weil pixel
  #save Extent from first file => metaextent for all files 
  if(i==1){
    res_extent <- temp_sent_crop
    
  }
  #resample on metaextent
  print("resampl on metaextent")
  #temp_swi_crop <- raster::resample(temp_swi_crop, res_extent, method='bilinear')
  temp_swi_10_crop <- raster::resample(temp_sw1_10_crop , res_extent, method='bilinear')
  temp_swi_40_crop <- raster::resample(temp_sw1_40_crop , res_extent, method='bilinear')
  temp_sent_crop <- raster::resample(temp_sent_crop, res_extent, method='bilinear')
  #change to utm 33 proj. and set resolution to 500m (depends on computing power)
  print("reproj.")
  #temp_swi_crop <-  raster::projectRaster(temp_swi_crop, res = 100, crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  temp_swi_10_crop <-  raster::projectRaster(temp_swi_10_crop, res = 100, crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  temp_swi_40_crop <-  raster::projectRaster(temp_swi_40_crop, res = 100, crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  temp_sent_crop <-  raster::projectRaster(temp_sent_crop, res =100,  crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  ###crop to the real extent
  print(" crop to real extent")
  #temp_swi_crop <- crop(temp_swi_crop, extent(real_extent))
  
  temp_swi_10_crop <- crop(temp_swi_10_crop, extent(real_extent))
  temp_swi_40_crop <- crop(temp_swi_40_crop, extent(real_extent))
  temp_sent_crop <- crop(temp_sent_crop, extent(real_extent)) ## obacht, weil pixel
  
  temp_sent_crop <- raster::resample(temp_sent_crop, temp_swi_10_crop , method='bilinear')
  #check if raster has error values
  if(is.numeric(minValue(temp_swi_10_crop))){
    print("Raster besitzt Werte")
    #store number of na-pixels from meta_layer
    if(i==1){
      print("erstelle meta_na_count")
      temP_df <- as.data.frame(temp_swi_10_crop)
      summe_na <- sum(is.na(temP_df))
      meta_na = summe_na
    }
    #check na pixels
    temP_df <- as.data.frame(temp_swi_10_crop)
    summe_na <- sum(is.na(temP_df))
    print(paste("NA_pixel: ", toString(summe_na)))
   
    
     if(summe_na == meta_na){
        print("gleicher Anzahl NA-Pixel")
        #ignore the swi-duplicate, export only 1 per date (cause of the 2 sentinel images per date in merged_data)
        if(i%%2 == 1){
          writeRaster(temp_swi_10_crop, paste("crop_swi_10cm\\", "SWI_10", "_", substr(name_swi, nchar(name_swi)-35, nchar(name_swi)-28), "_swi.tif", sep=""),  filetype = "GTiff", overwrite=TRUE)
          writeRaster(temp_swi_40_crop, paste("crop_swi_40cm\\", "SWI_40", "_", substr(name_swi, nchar(name_swi)-35, nchar(name_swi)-28), "_swi.tif", sep=""),  filetype = "GTiff", overwrite=TRUE)
        }
        writeRaster(temp_sent_crop, paste("crop_sent\\", substr(name_sent, nchar(name_sent)-44, nchar(name_sent)-37),"_",toString(i), "_sent.tif", sep=""),  filetype = "GTiff", overwrite=TRUE)
        print(paste("File: ",toString(i), "exportiert"))
      }
    else {print("Zu viele NAs, nächstes File")}
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
ndvi_mean <- brick("indices_mean\\Anual_Mean_Sentinel_NDVI_2019.tif")
ndvi_std <- brick("indices_mean\\Anual_StdDev_Sentinel_NDVI_2019.tif")
ndwi_mean <- brick("indices_mean\\Anual_Mean_Sentinel_NDWI_2019.tif")
ndwi_std <- brick("indices_mean\\Anual_StdDev_Sentinel_NDWI_2019.tif")

#get extent and change proj to wgs84
sent_resampl <- brick("sent_crop\\1_20190110_sent.tif") #load metafile for extent 
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
utm_resampl <- brick("sent_crop\\1_20190110_sent.tif")
constant_stack <- resample(constant_stack, utm_resampl)
##export data
out_dir <- "output_preprocessing"
query_dir <- dir.exists(out_dir)
if(query_dir==F) dir.create(out_dir)
#vector of all datanames
data_name <- c("aspect", "convergence", "elevation", "slope","twi","ndvi_mean" ,"ndwi_mean" ,"ndvi_std", "ndwi_std")#
#cropen not nes. cause metafile (ext_resampl)
constant_stack <- crop(constant_stack, extent(real_extent))

#export all files as tif 
for(i in 1:nlayers(constant_stack)){
  #temp_name= names(constant_stack[[i]]) #names will be like the inputfiles at load all
  temp_name <- (data_name[i])
  writeRaster(constant_stack[[i]],paste("output_preprocessing\\", temp_name,".tif", sep=""),filetype = "GTiff", overwrite=TRUE) 
  print(paste(temp_name, "exportiert", sep =" "))
}
####pre lulc
out_dir <- "lulc_out"
query_dir <- dir.exists(out_dir)
if(query_dir==F) dir.create(out_dir)


#load landcover and classify  
lulc <- raster("Landcover\\landcover_neu_clipped.tif")
my_quality_layer <- lulc
my_quality_layer[my_quality_layer %in% c(50)] <- 4000 ## impervious surface+greenhouse near-zero prediction q
my_quality_layer[my_quality_layer == 60] <- 1000 ## open soil good prediction q
my_quality_layer[my_quality_layer %in% c(30,40)] <- 2000 ##changing vegetation cover moderate prediction q
my_quality_layer[my_quality_layer %in% c(10,20)] <- 3000 ## dense vegetation cover bad prediction q
my_quality_layer[my_quality_layer %in% c(80) ] <- 8000 ## water bodies no prediction possible
#my_quality_layer[my_quality_layer %in% c(90,100)] <- -1 ## water bodies no prediction possible
#NA values

# get extent from sentinel_data to prepare lulc
sent_resampl <- brick("sent_crop\\1_20190110_sent.tif")
sent_resampl<-  raster::projectRaster(sent_resampl  ,crs =("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ext_resampl <- extent(sent_resampl)
utm_resampl <- brick("sent_crop\\1_20190110_sent.tif")
#prepare lulc (crop)
lulc <- crop(lulc, ext_resampl)
lulc <- projectRaster(lulc ,crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lulc <- resample(lulc, utm_resampl)
#same with lulc layers
my_quality_layer <- crop(my_quality_layer, ext_resampl)
my_quality_layer <- projectRaster(my_quality_layer ,crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
my_quality_layer <- resample(my_quality_layer, utm_resampl)
extent(my_quality_layer)

writeRaster(lulc ,paste(out_dir, "\\", "lulc",".tif", sep=""),filetype = "GTiff", overwrite=TRUE)
writeRaster(my_quality_layer ,paste(out_dir, "\\", "my_quality_layer",".tif", sep=""),filetype = "GTiff", overwrite=TRUE) 
