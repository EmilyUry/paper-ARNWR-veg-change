// Split panel image 1984/2017


//behind the scenes 

Map.setCenter(-76.2, 35.7, 9); //These are the coordinates used to center the view window and the zoom level 


// 1985

var table = ee.FeatureCollection("users/uryemily/all_unit_trim"); 
//Mapping Burn Unit by Last Burn Year
//Map.addLayer(table, {}, 'all units');


// Landsat 5 collection, surface reflectance
var ic = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR");
// clip collection to the area of interest
var c = ic.filterBounds(table);
// filters the collection to images from row 14 path 35 (not necessary for small areas)
var d =   c.filter(ee.Filter.eq('WRS_PATH', 14))
  .filter(ee.Filter.eq('WRS_ROW', 35));
// filter the clipped collection to those with less than 0.5 percent cloud cover
var filteredCollection = d.filterMetadata('CLOUD_COVER_LAND', 'less_than', 10);
// print collection, there are 18 images :)
//print(filteredCollection, 'landsat5 cloud filtered');

var visParams = {  //Landsat 5
  bands: ['B3', 'B2', 'B1'],
  min: 100,
  max: 1000,
  gamma:[1,1, 1.0],
};

var start = ee.Date('1984-01-01');
var finish = ee.Date('1986-12-31');
var composite = filteredCollection    
.filterDate(start,finish)
.map(function(i) {
  return i.updateMask(i.select('pixel_qa').bitwiseAnd(22).neq(0)); //exclude cloud and shadow
})
.median()
.clip(table);
//Map.addLayer(composite, visParams, 'image');

//var scene3 =ee.Image('LANDSAT/LC08/C01/T1_SR/LC08_014035_20171221');
//Map.addLayer(scene3.clip(table), visParams, 'April 2017', false);
// Make the training dataset.
var training = composite.sample({
  region: table,
  scale: 30,
  numPixels: 5000
});
// Instantiate the clusterer and train it.
var clusterer = ee.Clusterer.wekaKMeans(5).train(training);
//var clip = input.clip(shoreline_marsh);
var clip = composite.clip(table);
// Cluster the input using the trained clusterer.
var result85 = clip.cluster(clusterer);
var igbpPalette = [
  '18562c', // pine
    'fbf56c', // marsh
  'a98a5a',//shrub
  '18562c', // pine
  '72d692', //mixed
    ];
//Map.addLayer(result85,  {min: 0, max: 4, palette: igbpPalette}, 'classes 1985', false);



//////////////////////////////////

//2017

var table = ee.FeatureCollection("users/uryemily/all_unit_trim"); 
//Mapping Burn Unit by Last Burn Year
//Map.addLayer(table, {}, 'all units');


// Landsat 8 collection, surface reflectance
var ic = ee.ImageCollection("LANDSAT/LC08/C01/T1_SR");
// clip collection to the area of interest
var c = ic.filterBounds(table);
// filters the collection to images from row 14 path 35 (not necessary for small areas)
var d =   c.filter(ee.Filter.eq('WRS_PATH', 14))
  .filter(ee.Filter.eq('WRS_ROW', 35));
// filter the clipped collection to those with less than 0.5 percent cloud cover
var filteredCollection = d.filterMetadata('CLOUD_COVER_LAND', 'less_than', 20);
// print collection, there are 18 images :)
//print(filteredCollection, 'landsat8 cloud filtered');

var visParams = {  //Landsat 8
  bands: ['B4', 'B3', 'B2'],
  min: 100,
  max: 1000,
  gamma:[1,1, 1.0],
};

var start = ee.Date('2016-01-01');
var finish = ee.Date('2018-12-31');
var composite = filteredCollection    
.filterDate(start,finish)
.map(function(i) {
  return i.updateMask(i.select('pixel_qa').bitwiseAnd(22).neq(0)); //exclude cloud and shadow
})
.median()
.clip(table);
//Map.addLayer(composite, visParams, 'image');

//var scene3 =ee.Image('LANDSAT/LC08/C01/T1_SR/LC08_014035_20171221');
//Map.addLayer(scene3.clip(table), visParams, 'April 2017', false);
// Make the training dataset.
var training = composite.sample({
  region: table,
  scale: 30,
  numPixels: 5000
});
// Instantiate the clusterer and train it.
var clusterer = ee.Clusterer.wekaKMeans(5).train(training);
//var clip = input.clip(shoreline_marsh);
var clip = composite.clip(table);
// Cluster the input using the trained clusterer.
var result17 = clip.cluster(clusterer);
var igbpPalette1 = [
  'a98a5a', // shrub
    'fbf56c', // marsh
  'fbf56c',//marsh2
  '72d692', // mixed
  '18562c', //pine
    ];
//Map.addLayer(result17,  {min: 0, max: 4, palette: igbpPalette1}, 'classes 2017', false);


var vis85 = result85.visualize({min: 0, max: 4, palette: igbpPalette});
var vis17 = result17.visualize({min: 0, max: 4, palette: igbpPalette1});

var images = {'1985': vis85, '2017': vis17};


/*
 * Set up the maps and control widgets
 */

// Create the left map, and have it display layer 0.
var leftMap = ui.Map();
leftMap.setControlVisibility(false);
var leftSelector = addLayerSelector(leftMap, 0, 'top-left');

// Create the right map, and have it display layer 1.
var rightMap = ui.Map();
rightMap.setControlVisibility(false);
var rightSelector = addLayerSelector(rightMap, 1, 'top-right');

// Adds a layer selection widget to the given map, to allow users to change
// which image is displayed in the associated map.
function addLayerSelector(mapToChange, defaultValue, position) {
  var label = ui.Label('Choose an image to visualize');

  // This function changes the given map to show the selected image.
  function updateMap(selection) {
    mapToChange.layers().set(0, ui.Map.Layer(images[selection]));
  }

  // Configure a selection dropdown to allow the user to choose between images,
  // and set the map to update when a user makes a selection.
  var select = ui.Select({items: Object.keys(images), onChange: updateMap});
  select.setValue(Object.keys(images)[defaultValue], true);

  var controlPanel =
      ui.Panel({widgets: [label, select], style: {position: position}});

  mapToChange.add(controlPanel);
}


/*
 * Tie everything together
 */

// Create a SplitPanel to hold the adjacent, linked maps.
var splitPanel = ui.SplitPanel({
  firstPanel: leftMap,
  secondPanel: rightMap,
  wipe: true,
  style: {stretch: 'both'}
});

// Set the SplitPanel as the only thing in the UI root.
ui.root.widgets().reset([splitPanel]);
var linker = ui.Map.Linker([leftMap, rightMap]);
leftMap.setCenter(-76.2, 35.7, 10);





