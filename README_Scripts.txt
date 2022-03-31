following R scripts:

Google Earth Engine
yearly_ndvi_ndwi_gee: calculates annual average and standard deviation NDWI/NDVI for selected extent
--> output: rasterdata (geotiff)

R
preprocessing: crop, reproject, resample all data sets
--> output: raster with same extents, pixelsizes and projections

R
correlation with SWI: pixelwise correlation and linear regression (SWI ~ S1K0)
--> output: Korrelationsraster

R
regression: time-wise linear regression (SWI ~ S1K0 + additional data)
--> output: Regression: csv table with parameters
--> output: Residuals_raster: residuals per time and soil depth

R
random forest: time-wise random forest (SWI ~ S1K0 + additional data
--> output: RandomForest: csv tables with parameters + prediction maps

R
SWI_SM_DERIVATION: pixelwise derivation of soilmoistute from inputdata using random forest
--> output predictionraster (soilmoisture)

Python
The following two scripts can only be used with the demmin tereno data:
filter_and_extract_insitu-data: extracts the needed soilmoisture data per timestamp
--> output table with soilmoisture data

Python
combine_filtered_insitu-data: combines soilmoisture data with coordinates of the measuring stations
--> output table with soilmoisture data and coordinates

R
correlation_prediction_insitu: 
--> extracts point values, at measuring stations, from prediction_raster
--> correlation (insitu ~ extracted values)
--> output: table with correlation coefficients



Attention: Scripts may not run from start till end. A lot of memory is needed for the calculations. 
Thus, the script may stop in the middle of the for loops. Just restart R and continue with the current soil depth.
