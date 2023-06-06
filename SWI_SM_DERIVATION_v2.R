#Derivation of the soil moisture from the input data using Random Forest as classification/regression method
#first landcover data has to be prepared
library(rgdal)
library(raster)
library(RStoolbox)
library(sp)
install.packages("Rtools")

Sys.which("C:\\rtools40\\usr\\bin\\make.exe")
urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
install.packages(urlPackage, repos=NULL, type="source") 
library("randomForest")

rm(list = ls())
setwd("D:\\Schlaak\\Soilmoisutre_downscaling\\")
#create output dir
out_dir <- "SWI_Predictions_10cm"
query_dir <- dir.exists(out_dir)
if(query_dir==F) dir.create(out_dir)

# get extent from shapefile 
aoi <- readOGR("extent\\extent_wgs.shp")
aoi_buffer <- buffer(aoi, 0.1)
aoi_utm <- readOGR("extent\\extent_utm.shp")

#load landcover 
lulc <- raster("lulc_out\\lulc.tif")
my_quality_layer <- raster("lulc_out\\my_quality_layer.tif")
#list all available swi files and extract date information (from file name)
swi_data <- data.frame(file = list.files("crop_swi_10cm_kor", pattern = "*.tif$", recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
swi_data$date <-substr(swi_data$file, nchar(swi_data$file)-15, nchar(swi_data$file)-8) 
#filter swi files: only SWI data from 2019
swi_data <- swi_data[grepl("2019", swi_data$file),]
#list all available s1k0 files and extract date information (from file name)
s1k0_data_a <- data.frame(file = list.files("crop_sent\\Sent1_band_a\\", pattern = glob2rx("*.tif$"), recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
s1k0_data_a$date <- substr(s1k0_data_a$file, nchar(s1k0_data_a$file)-19, nchar(s1k0_data_a$file)-12)  
####sent_band b
s1k0_data_b <- data.frame(file = list.files("crop_sent\\Sent1_band_b\\", pattern = glob2rx("*.tif$"), recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
s1k0_data_b$date <- substr(s1k0_data_b$file, nchar(s1k0_data_b$file)-19, nchar(s1k0_data_b$file)-12)  
#get ndvi
ndvi_data <- data.frame(file = list.files("crop_ndvi\\", pattern = glob2rx("*.tif$"), recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
ndvi_data$date <- substr(ndvi_data$file, nchar(ndvi_data$file)-16, nchar(ndvi_data$file)-9)
#get ndwi
ndwi_data <- data.frame(file = list.files("crop_ndwi\\", pattern = glob2rx("*.tif$"), recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
ndwi_data$date <- substr(ndwi_data$file, nchar(ndwi_data$file)-16, nchar(ndwi_data$file)-9)

#merge the file information --> only dates left for which both data is available
merged_data <- merge(swi_data, s1k0_data_a, by = "date")
#merged_indices <- merge(ndvi_data, ndwi_data, by= "date")
merged_ndvi<- merge(ndvi_data, swi_data, by = "date")
merged_ndwi<- merge(ndwi_data, swi_data, by = "date")
merged_sent<- merge(s1k0_data_a, swi_data, by="date")
#stack data
s1_stack <- stack(merged_data$file.y)
#load all data sets from preprocessing script
aspect <- brick("output_preprocessing\\aspect.tif")
convergence <- brick("output_preprocessing\\convergence.tif")
elevation <- brick("output_preprocessing\\elevation.tif")
slope <- brick("output_preprocessing\\slope.tif")
#ndvi_mean <- brick("output_preprocessing\\ndvi_mean.tif")
#ndvi_std <- brick("output_preprocessing\\ndvi_std.tif")
#ndwi_mean <- brick("output_preprocessing\\ndwi_mean.tif")
#ndwi_std <- brick("output_preprocessing\\ndwi_std.tif")
twi <- brick("output_preprocessing\\twi.tif")
#stack all
constant_stack <- stack(aspect, convergence, elevation, slope,twi)#,ndvi_mean ,ndwi_mean ,ndvi_std, ndwi_std)
#was machen sachen lulc

sent_resampl <- brick("crop_sent\\Sent1_band_a\\20190110_01_sent.tif")
sent_resampl<-  raster::projectRaster(sent_resampl  ,crs =("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ext_resampl <- extent(sent_resampl)
utm_resampl <- brick("crop_sent\\Sent1_band_a\\20190110_01_sent.tif")

lulc <- raster("Landcover\\landcover_neu_clipped.tif")
lulc <- crop(lulc, ext_resampl)
lulc <- projectRaster(lulc ,crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lulc <- resample(lulc, utm_resampl)

#load regression output and create mask with lulc => export
resi_stack <- stack(list.files(path = "Output_10cm", pattern =  "*.tif$", full.names = T))


resi_stack_masked <- mask(resi_stack, lulc)
writeRaster(resi_stack_masked, "temp_stack_masked_real.tif", format="GTiff", overwrite= T)

varnames <- c("SWI_010")#"SWI_001", "SWI_005","SWI_010","SWI_015","SWI_020","SWI_040", "SWI_060", "SWI_100")
k=1

for (i in 1:nrow(merged_data)) {
  

  j = ceiling(i/2)
  
  #my_swi <- raster(merged_data$file.x[i], varname= varnames[k])

  my_ndvi<- raster(merged_ndvi$file.x[j], varname= varnames[k])
  my_resi <- brick("temp_stack_masked_real.tif")[[j]]
  my_ndwi<- raster(merged_ndwi$file.x[j], varnames = varnames[k])
  sent_b<- raster(merged_sent$file.x[j], varnames = varnames[k])
  my_k0 <- s1_stack[[i]]
  swi<- raster(merged_data$file.x[i], varname= varnames[k])
  #stack and cropen 
  
  temp_stack <- stack(my_k0, my_ndvi, my_ndwi, sent_b)
  #temp_crop <-  crop(temp_stack, extent(constant_stack, 500, 1000, 500, 1000))
  #const_crop <- crop(constant_stack, extent(constant_stack, 500, 1000, 500, 1000))
  #my_resi <- crop(my_resi, extent(constant_stack, 500, 1000, 500, 1000))
  #swi <- crop(swi, extent(constant_stack,500, 1000, 500, 1000))
  #my_quality_layer_crop <- crop(my_quality_layer, extent(my_quality_layer,500, 1000, 500, 1000))
  
  ####
  print("set_values")
  my_resi_rounded <- setValues(my_resi, round(getValues(my_resi), digits = 0))
  rm(my_resi)
  ######
  print("Which")
  my_cellNR <- Which(my_resi_rounded %in% c(-2:2), cells=T) #annahme lin 
  #####
  print("xyFromcell")
  cellCoord <- xyFromCell(my_resi_rounded, my_cellNR)
  #####
  print("seq")
  pos <- seq(1, nrow(cellCoord), 5) #eigentlich 10
  #####
  print("cellcord")
  cellCoord_sub <- cellCoord[pos,]
  ######
  print("as DF")
  traincoords <- as.data.frame(cellCoord_sub)
  coordinates(traincoords) <- ~x+y
  crs(traincoords) <- crs(my_resi_rounded)
  print("extract")
  traindata <- extract(swi, traincoords, sp=T)
  rm(traincoords)
  names(traindata) <- "swi"
  print("superclass")
  #superclass ohne temp ndvi/ndwi
  my_pred <- superClass(stack(temp_stack, constant_stack), trainData = traindata, responseCol = "swi", trainPartition = 0.7,
                        model = "rf", mode = "regression", predict = TRUE, verbose = TRUE, kfold = 3, tuneLength = 1)
  
  
  my_out <- stack(my_pred$map, my_quality_layer)
  my_out_name <- paste0(out_dir, "/", s1k0_data_a$date[i], "_", varnames[k], "_lulc_adjusted_prediction.tif")
  
  writeRaster(my_out, filename = my_out_name, format="GTiff", overwrite=T)
  
  print(paste( "exportiert ", my_out_name))
}

file.remove("temp_stack_masked_real.tif")
