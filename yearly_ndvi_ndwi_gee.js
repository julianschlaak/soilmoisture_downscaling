Map.centerObject(geometry);

// get sentinel2 data (as collection) which intersects the geometry for the specified time frame 
var S2_SR_2019 = ee.ImageCollection('COPERNICUS/S2_SR').filterBounds(geometry).filterDate('2019-01-01', '2019-12-31');
var S2_SR_2020 = ee.ImageCollection('COPERNICUS/S2_SR').filterBounds(geometry).filterDate('2020-01-01', '2020-12-31');

// function which adds a NDVI band to an image
var addNDVI = function(image) {
  var ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI');
  return image.addBands(ndvi);
};

// function which adds a NDWI band to an image
var addNDWI = function(image) {
  var ndwi = image.normalizedDifference(['B8', 'B3']).rename('NDWI');
  return image.addBands(ndwi);
};

// use the functions to add the bands to each image of the collections
var S2_newBands_2019 = S2_SR_2019.map(addNDVI);
var S2_newBands_2020 = S2_SR_2020.map(addNDVI);
var S2_newBands_2019 = S2_newBands_2019.map(addNDWI);
var S2_newBands_2020 = S2_newBands_2020.map(addNDWI);
//print(S2_newBands_2019, S2_newBands_2020)

// calculate the mean and standard deviation of each collection, returning a single image for each collection
var stabw_2019 = S2_newBands_2019.reduce(ee.Reducer.stdDev());
var stabw_2020 = S2_newBands_2020.reduce(ee.Reducer.stdDev());
var mittelw_2019 = S2_newBands_2019.reduce(ee.Reducer.mean());
var mittelw_2020 = S2_newBands_2020.reduce(ee.Reducer.mean());
//print(stabw_2019, stabw_2020, mittelw_2019, mittelw_2020)

// export the the images to the google drive
Export.image.toDrive({
  image: stabw_2019.select('NDVI_stdDev'),
  description: 'Anual_StdDev_Sentinel_NDVI_2019',
  scale: 10,
  region: geometry,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});

Export.image.toDrive({
  image: stabw_2020.select('NDVI_stdDev'),
  description: 'Anual_StdDev_Sentinel_NDVI_2020',
  scale: 10,
  region: geometry,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});

Export.image.toDrive({
  image: mittelw_2019.select('NDVI_mean'),
  description: 'Anual_Mean_Sentinel_NDVI_2019',
  scale: 10,
  region: geometry,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});

Export.image.toDrive({
  image: mittelw_2020.select('NDVI_mean'),
  description: 'Anual_Mean_Sentinel_NDVI_2020',
  scale: 10,
  region: geometry,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});

Export.image.toDrive({
  image: stabw_2019.select('NDWI_stdDev'),
  description: 'Anual_StdDev_Sentinel_NDWI_2019',
  scale: 10,
  region: geometry,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});

Export.image.toDrive({
  image: stabw_2020.select('NDWI_stdDev'),
  description: 'Anual_StdDev_Sentinel_NDWI_2020',
  scale: 10,
  region: geometry,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});

Export.image.toDrive({
  image: mittelw_2019.select('NDWI_mean'),
  description: 'Anual_Mean_Sentinel_NDWI_2019',
  scale: 10,
  region: geometry,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});

Export.image.toDrive({
  image: mittelw_2020.select('NDWI_mean'),
  description: 'Anual_Mean_Sentinel_NDWI_2020',
  scale: 10,
  region: geometry,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});