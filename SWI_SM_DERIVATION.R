#Derivation of the soil moisture from the input data using Random Forest as classification/regression method
#first landcover data has to be prepared
library(rgdal)
library(raster)
library(RStoolbox)
library(sp)

rm(list = ls())
setwd("F:\\loew\\")
#create output dir
out_dir <- "SWI_Predictions"
query_dir <- dir.exists(out_dir)
if(query_dir==F) dir.create(out_dir)

# get extent from shapefile 
aoi <- readOGR("extent\\extent_wgs.shp")
aoi_buffer <- buffer(aoi, 0.1)
aoi_utm <- readOGR("extent\\extent_utm.shp")

#load landcover and classify  
lulc <- raster("landcover_neu_clipped.tif")
my_quality_layer <- lulc
my_quality_layer[my_quality_layer %in% c(50)] <- 4000 ## impervious surface+greenhouse near-zero prediction q
my_quality_layer[my_quality_layer == 60] <- 1000 ## open soil good prediction q
my_quality_layer[my_quality_layer %in% c(30,40)] <- 2000 ##changing vegetation cover moderate prediction q
my_quality_layer[my_quality_layer %in% c(10,20)] <- 3000 ## dense vegetation cover bad prediction q
my_quality_layer[my_quality_layer %in% c(80) ] <- 8000 ## water bodies no prediction possible
#my_quality_layer[my_quality_layer %in% c(90,100)] <- -1 ## water bodies no prediction possible
#NA values
#lulc[lulc %in% c(2, 7,8)] <- NA #lulc[lulc %in% c(2, 7,8)]
#list all available swi files and extract date information (from file name)
swi_data <- data.frame(file = list.files("crop_swi", pattern = "*.tif$", recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
swi_data$date <-substr(swi_data$file, nchar(swi_data$file)-15, nchar(swi_data$file)-8) 
#filter swi files: only SWI data from 2019
swi_data <- swi_data[grepl("2019", swi_data$file),]
#list all available s1k0 files and extract date information (from file name)
s1k0_data <- data.frame(file = list.files("crop_sent\\", pattern = glob2rx("*.tif$"), recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
#s1k0_data$date <- lapply(s1k0_data$file, FUN = function(x){substr(strsplit(x, "/")[[1]][2],1,8)})
s1k0_data$date <- substr(s1k0_data$file, nchar(s1k0_data$file)-16, nchar(s1k0_data$file)-9)  # 44 /37 zeichennummern
#merge sentinel and swi
merged_data <- merge(swi_data, s1k0_data, by = "date")
#stack data
s1_stack <- stack(merged_data$file.y)
#load all data sets from preprocessing script
aspect <- brick("output_preprocessing\\aspect.tif")
convergence <- brick("output_preprocessing\\convergence.tif")
elevation <- brick("output_preprocessing\\elevation.tif")
slope <- brick("output_preprocessing\\slope.tif")
ndvi_mean <- brick("output_preprocessing\\ndvi_mean.tif")
ndvi_std <- brick("output_preprocessing\\ndvi_std.tif")
ndwi_mean <- brick("output_preprocessing\\ndwi_mean.tif")
ndwi_std <- brick("output_preprocessing\\ndwi_std.tif")
twi <- brick("output_preprocessing\\twi.tif")
#stack all
constant_stack <- stack(aspect, convergence, elevation, slope,twi,ndvi_mean ,ndwi_mean ,ndvi_std, ndwi_std)
# get extent from sentinel_data to prepare lulc
sent_resampl <- brick(merged_data[[1,3]])
sent_resampl<-  raster::projectRaster(sent_resampl  ,crs =("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ext_resampl <- extent(sent_resampl)
utm_resampl <- brick(merged_data[[1,3]])
#prepare lulc (crop)
lulc <- crop(lulc, ext_resampl)
lulc <- projectRaster(lulc ,crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lulc <- resample(lulc, utm_resampl)
#same with lulc layers
my_quality_layer <- crop(my_quality_layer, ext_resampl)
my_quality_layer <- projectRaster(my_quality_layer ,crs =("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
my_quality_layer <- resample(my_quality_layer, utm_resampl)
#load regression output and create mask with lulc => export
resi_stack <- stack(list.files(path = "Output", pattern =  "*.tif$", full.names = T))
resi_stack_masked <- mask(resi_stack, lulc)
writeRaster(resi_stack_masked, "temp_stack_masked.tif", format="GTiff", overwrite= T)

varnames <- c("SWI_001", "SWI_005","SWI_010")#,"SWI_015","SWI_020","SWI_040", "SWI_060", "SWI_100")
for (i in 1:nrow(merged_data)) {
  my_swi <- raster(merged_data$file.x[i], varname= varnames[k])
  my_resi <- brick("temp_stack_masked.tif")[[i]]
  my_k0 <- s1_stack[[i]]

  swi<- raster(merged_data$file.x[i], varname= varnames[k])
  my_resi_rounded <- setValues(my_resi, round(getValues(my_resi), digits = 0))
  rm(my_resi)
  my_cellNR <- Which(my_resi_rounded %in% c(-1:0.2), cells=T) #annahme lin 
  
  cellCoord <- xyFromCell(my_resi_rounded, my_cellNR)
  pos <- seq(1, nrow(cellCoord), 10)
  
  cellCoord_sub <- cellCoord[pos,]
  
  traincoords <- as.data.frame(cellCoord_sub)
  coordinates(traincoords) <- ~x+y
  crs(traincoords) <- crs(my_resi_rounded)
  
  traindata <- extract(swi, traincoords, sp=T)
  rm(traincoords)
  names(traindata) <- "swi"
  
  my_pred <- superClass(stack(my_k0, constant_stack), trainData = traindata, responseCol = "swi", trainPartition = 0.7,
                        model = "rf", mode = "regression", predict = TRUE, verbose = TRUE, kfold = 3, tuneLength = 1)
  
  my_out <- stack(my_pred$map, my_quality_layer)
  my_out_name <- paste0(out_dir, "/", s1k0_data$date[i], "_", varnames[k], "_lulc_adjusted_prediction.tif")
  
  writeRaster(my_out, filename = my_out_name, format="GTiff", overwrite=T)
}

file.remove("temp_stack_masked.tif")