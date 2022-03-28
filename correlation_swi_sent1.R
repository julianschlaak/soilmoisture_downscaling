#Correlation between Sentinel and SWI data
#This script is only intended as an overview
#The output will not be used further!

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

rm(list = ls())
setwd("F:\\loew\\")
#get aoi(extent)
aoi_utm <- readOGR("extent_neu_utm.shp")
aoi_buffer <- buffer(aoi_utm, 0.1)
#list all available swi files and extract date information (from file name)
swi_data <- data.frame(file = list.files("crop_swi_2\\", pattern = "*.tif$", recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
swi_data$date <-substr(swi_data$file, nchar(swi_data$file)-15, nchar(swi_data$file)-8) 
#list all available s1k0 files and extract date information (from file name)
s1k0_data <- data.frame(file = list.files("crop_sent_2\\", pattern = glob2rx("*.tif$"), recursive = TRUE, full.names = TRUE), stringsAsFactors = FALSE)
s1k0_data$date <- substr(s1k0_data$file, nchar(s1k0_data$file)-16, nchar(s1k0_data$file)-9) 
#merge by date 
merged_data <- merge(swi_data, s1k0_data, by = "date")

#create a layer stack with all s1k0 data of the previous filtered dates
for(i in c(1:nrow(merged_data))){
  s1 <- brick(merged_data$file.y[i]) # multilayer rasterobject => method for character
  if(i == 1){
    s1_stack <- s1  
  }
  else{
    s1_stack <- stack(s1_stack, s1) 
  }
  #layernames: date information
  names(s1_stack)[i] <- paste0("s1_k0_", merged_data$date[i])
}
#function to calculate correlation and linear regression
corr_lm_function <- function(x){
  x <- as.vector(x)
  #pearson correlation coefficient
  c <- cor(x[1:8], x[9:16], method = "pearson") #care HARDCODE adapt nlayers
  #approach does not work 
  #c <- cor(x[1:(nlayers(x))/2], x[((nlayers(x))/2)+1:nlayers(x)], method = "pearson") 
  
  #create data frame so that one column = swi, another column = s1k0 values
  x_df <- data.frame(swi = x[1:8], s1 = x[9:16]) #care HARDCODE adapt nlayers
  
  #approach does not work 
  #x_df <- data.frame(swi = x[1:(nlayers(x))/2], s1 = x[((nlayers(x))/2)+1:nlayers(x)])
  
  #if there is no data for both of them --> no linear model can be calculated
  if(nrow(x_df[complete.cases(x_df),]) == 0){
    r2_mod <- NA
    p_mod <- NA
  }else{
    #calculate linear model (y = swi, x = s1k0)
    mod <- lm(swi ~ s1, data = x_df)
    #extract p value and R² of the model and return those
    p_mod <- as.numeric(pf(summary(mod)$fstatistic[1], summary(mod)$fstatistic[2], summary(mod)$fstatistic[3], lower.tail = FALSE))
    r2_mod <- summary(mod)$adj.r.squared
  }
  vals <- c(c, r2_mod, p_mod)
  return(vals)
}

#vector of all soil depths
varnames <- c("SWI_001", "SWI_005","SWI_010","SWI_015","SWI_020","SWI_040", "SWI_060", "SWI_100")

#do the following for all soil depths
for (v in varnames){
  print(paste("Processing", v))
  #create a layer stack of the swi data (of the specific soil depth) and mask it to the aoi
  for(i in c(1:nrow(merged_data))){
    #get swi
    swi <- raster::brick(merged_data$file.x[i], varname = v)
    swi_aoi <- swi 
    if(i == 1){
      swi_stack <- swi_aoi
    }else{
      swi_stack <- stack(swi_stack, swi_aoi)
    }
    #layer names: date information
    names(swi_stack)[i] <- paste0("swi_", merged_data$date[i])
  }
  #stack swi and s1
  data_stack <- stack(swi_stack, s1_stack)
  #execute corr_lm_function
  corr_raster <- calc(data_stack, fun= corr_lm_function)
  #plot the see results
  print(paste("Plot", v))
  plot(corr_raster)
  #export the corr_raster
  writeRaster(corr_raster, paste0("Output_reg_2\\corr_lm_raster_", v, ".tif"), overwrite = TRUE, filetype = "GTiff")
  print("exportiert" )
}


