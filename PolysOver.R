# PolysOver function
# *********************************************************************************************
# function takes SpatialPointsDataFrame (SpPts) and SpatialPolygonsDataFrame (SpPol) as inputs
# and checks if any of the Polygons overlap with the bounding box of SpatialPointsDataFrame.
# In case overlapping Polys exist function returns these Polys
#
# Note: function requires overlap function (overlap.R)
# *********************************************************************************************

PolysOver = function(SpPts, SpPol){
require(sp)
  # check if overlap function is loaded
  if(sum(lsf.str(envir = .GlobalEnv) == "overlap") == 0){
    warning("Error: overlap function not loaded")
    }
  
  # create bounding box for SpatialPointsDF
  # for now we set a buffer of 0.02 deg around the SpatialPointsDF
  base = bbox(SpPts) + c(-0.02,-0.02,0.02,0.02)
  
  # initialize output
  SpPolysOver = SpatialPolygons(list())
  # initialize empty list for Polys
  PolysOverList = list()
  
  # now iterate through SpatialPolygonsDataFrame
  # iterate through Polygons objects
  for(i in 1:length(SpPol@polygons)){
    # initilaze empty list for Poly objects
    PolyOverList = list()
    # for every Polygons object, iterate through all associated Polygon objects
    for(j in 1:length(SpPol@polygons[[i]]@Polygons)){
      
      if(overlap(base, bbox(SpPol@polygons[[i]]@Polygons[[j]]@coords))){
        # create Polygon object here
        PolyOver = SpPol@polygons[[i]]@Polygons[[j]]
        PolyOverList = c(PolyOverList, PolyOver)
      }
    }
    
    # if PolyOverList is not empty create Polygons object
    if(length(PolyOverList) != 0){
      # create Polygons object here
      PolysOver = Polygons(PolyOverList, ID = SpPol@polygons[[i]]@ID)
      PolysOverList = c(PolysOverList, PolysOver)
    }
    
  }
  # if PolysOverList is not empty create SpatialPolygons object
  if(length(PolysOverList) != 0){
    # create SpatialPolygons object here
    SpPolysOver = SpatialPolygons(PolysOverList)
  }
  return(SpPolysOver)
}

