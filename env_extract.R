library("rgdal")
library("raster")
library("sp")

# import environmental data as raster
bath = raster(file.choose()) # bathymetry data

# slope and aspect ---------------------------------------------
# calculate slope and aspect from bathymetry data

# import dry reef shapefiles
GBR.dry = readOGR(dsn = "/Users/tadzio/Documents/UQProject/data/Bathymetry\ data/3dgbr_geomorph/shape/", 
                  layer = "gbr_dryreef")

CS.dry = readOGR(dsn = "/Users/tadzio/Documents/UQProject/data/Bathymetry\ data/3dgbr_geomorph/shape/", 
                 layer = "coralsea_dryreef")

# plot bathymetry data and transects; check if they line-up
# first specify colormap
library(RColorBrewer)
cols = rev(colorRampPalette(brewer.pal(11, "Spectral"))(256))
# plot bathymetry map of cropped area
image(crop(bath, extent(147.85, 147.92, -16.5,-16.4)),
      col = cols, 
      breaks = c(seq(-5000, -500, (4500/55)), seq(-75, 0, (75/200))))

grad = lapply(split(cdata[, c(1:4)], as.factor(cdata$trans)), function(x){
  # load functions
  source("/Users/tadzio/Documents/UQProject/scripts/PolysOver.R")
  source("/Users/tadzio/Documents/UQProject/scripts/overlap.R")
  
  coordinates(x) = c("lon", "lat")
  proj4string(x) = CRS("+proj=longlat +datum=WGS84")
  
  # check of SpatialPoints overlap with GBR dry-reef polys
  if (length(PolysOver(x, GBR.dry)) != 0){
    dryPoly = PolysOver(x, GBR.dry)
  } 
  # now check if SpatialPoints overlap with CS dry-reef polys
  # in case of no overlap PolysOver returns an empty SpPolygons object
  else {
    dryPoly = PolysOver(x, CS.dry)
  }
  
  pbox = bbox(x) + c(-0.02,-0.02,0.02,0.02)
  bath.crop = crop(bath, pbox)
  # ************
  # for plotting
  # ************
  #   plot(terrain(bath.crop, opt="slope", unit = "tangent", neighbors = 8), main = x@data$trans[1])
  #   plot(dryPoly, add = T)
  #   plot(x, add = T)
  
  # set dry reef pixels to NaNs
  bath.crop[dryPoly] = NaN
  # plot(terrain(bath.crop, opt="slope", unit = "tangent", neighbors = 8), main = x@data$trans[1])
  
  # calculate aspect and slope
  aspect = extract(terrain(bath.crop, opt = "aspect", unit = "radians", neighbors = 8), x, method = "simple", sp = T)@data
  slope = extract(terrain(bath.crop, opt = "slope", unit = "degrees", neighbors = 8), x, method = "simple", sp = T)@data
  
  return(list(image = aspect$image,
              aspect = aspect$aspect,
              slope = slope$slope))

})

# now bind list by rows
library(data.table)
grad = rbindlist(grad) 
# take sine and cosine of aspect; (see Wilson et al 2007)
grad$aspect.cos = cos(grad$aspect)
grad$aspect.sin = sin(grad$aspect)

# geomorphology -----------------------------------------
# determine geomorphology (e.g. barrier reef or atoll)
# import csv containing transect id - reef name - region
# file: trans_region.csv
mGBR = read.csv(file.choose(), sep=";", head=TRUE)
# remove rows with NA values
mGBR = mGBR[1:131,]


# divide transects in sections according to region:
# Northern GBR = Far Northern
# Central GBR = Central + Cairns
# South GBR = Heron + One Three Island + Wilson Island Reef*
# Coral Sea = Osprey Reef + Flinders Reef + Holmes Reef
# *note: Wilson Island Reef is not Wilson Reef
mGBR$section[mGBR$sub_region %in% "Far Northern"] = "Northern GBR"
mGBR$section[mGBR$sub_region %in% c("Central", "Cairns")] = "Central GBR"
mGBR$section[mGBR$reef_name %in% c("Heron Island Reef","One Tree Island", "Wilson Island Reef")] = "Southern GBR"
mGBR$section[mGBR$reef_name %in% c("Osprey Reef","Flinders Reef", "Holmes Reef")] = "CSCMR"

# create df with image and associated geomorphology
geomorph = cdata[,1:4]
geomorph$morph[geomorph$trans %in% mGBR[mGBR$section == "CSCMR",1]] = "Atoll"
geomorph$morph[geomorph$trans %in% mGBR[mGBR$section != "CSCMR",1]] = "Barrier"

# check if morphology is assigned correctly
image(bath, col = cols, breaks = c(seq(-5000, -500, (4500/55)), seq(-75, 0, (75/200))))
# are transects indeed located on the barrier reef?
# repeat for geomorph$morph = "Atoll
plot(SpatialPoints(geomorph[geomorph$morph == "Atoll", c(4,3)], 
                   proj4string = CRS("+proj=longlat +datum=WGS84")), 
     add = TRUE, 
     col="black")

# if assigned correctly remove trans, lat and lon column from df
geomorph = geomorph[,c(-2,-3,-4)]

# extract environmental variables -----------------------------------------

# set coordinates of sampling locations
samp = data.frame(image = cdata$image, lon = cdata$lon, lat = cdata$lat)
coordinates(samp) = c("lon", "lat")

# get geotiffs
gtiffs = list.files(path = "/Users/tadzio/Documents/UQProject/data/vars_geotiffs", pattern = "\\.tif$",
                    recursive = T, full.names = T)

#init environmental df
env = as.data.frame(matrix(NA, ncol = length(gtiffs)+1, nrow = length(samp)))
env[,1] = samp@data$image
colnames(env)[1] = "image"
# extract values from geotiffs
for(i in 1:length(gtiffs)){
  # get var name
  varname = sub(".*/([a-z].*).tif", "\\1", gtiffs[i])
  # laod raster
  rfile = raster(gtiffs[i])
  # extract values according to coordinates in "samp"
  var = extract(rfile, samp, method = "simple", sp = T)@data
  # sort "var" so it matches the sequence of images in "env"
  # this is probably not necessary; but doing it to make sure so rows are alligned properly
  var = var[match(env$image, var$image),]
  env[,i+1] = var[,2]
  colnames(env)[i+1] = varname
}

# sort "grad" and "geomorph" so it matches the sequence of images in "env"
grad = grad[match(env$image, grad$image),]
geomorph = geomorph[match(env$image, geomorph$image),]
# add gradient and geomorph vars to env df
env$slope = grad$slope
env$aspect.sin = grad$aspect.sin
env$aspect.cos = grad$aspect.cos
env$morph = as.factor(geomorph$morph) 
