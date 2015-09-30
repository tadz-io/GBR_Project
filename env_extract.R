library("rgdal")
library("raster")
library("sp")

# import environmental data as raster
cots = raster(file.choose()) # crown of thorns data
cyclo = raster(file.choose()) # cyclone data
bath = raster(file.choose()) # bathymetry data

# slope and aspect ---------------------------------------------
# calculate slope and aspect from bathymetry data

# import dry reef shapefiles
GBR.dry = readOGR(dsn = "/Users/tadzio/Documents/UQProject/data/Bathymetry\ data/3dgbr_geomorph/shape/", 
                  layer = "gbr_dryreef")

CS.dry = readOGR(dsn = "/Users/tadzio/Documents/UQProject/data/Bathymetry\ data/3dgbr_geomorph/shape/", 
                 layer = "coralsea_dryreef")

# plot bathymetry data and transects
# check if they line-up

# first specify colormap
library(RColorBrewer)
cols = rev(colorRampPalette(brewer.pal(11, "Spectral"))(256))
# plot bathymetry map of cropped area
image(crop(bath, extent(147.85, 147.92, -16.5,-16.4)),
      col = cols, 
      breaks = c(seq(-5000, -500, (4500/55)), seq(-75, 0, (75/200))))

# plot transect points on top of bathymetry plot
plot(SpatialPoints(cdata[, c(4,3)], proj4string = CRS("+proj=longlat +datum=WGS84")), add = TRUE, col="black")

# plot dryreef shape files on top of bathymetry data
plot(GBR.dry, add = TRUE)
plot(CS.dry, add = TRUE)

# TO DO: exclude dry reef pixels from calculation of slope and aspect!
# MANUEL, ANY SUGGESTIONS?

# calculate slope and aspect from bathymetry data
slope = terrain(bath, opt = 'slope', unit = 'tangent', neighbors = 8)
aspect = terrain(bath, opt = 'aspect', unit = 'radians', neighbors = 8)

# set coordinates of sampling locations
samp = data.frame(image = cdata$image, lon = cdata$lon, lat = cdata$lat)
coordinates(samp) = c("lon", "lat")

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
plot(SpatialPoints(geomorph[geomorph$morph == "Barrier", c(4,3)], 
                   proj4string = CRS("+proj=longlat +datum=WGS84")), 
     add = TRUE, 
     col="black")

# if assigned correctly remove trans, lat and lon column from df
geomorph = geomorph[,c(-2,-3,-4)]

# extract environmental variables -----------------------------------------
# extract environmental vars for every sampling location
cyclo.ex = data.frame(extract(cyclo, samp, method="simple", sp = T)@data)
cots.ex = data.frame(extract(cots, samp, method="simple", sp = T)@data)
slope.ex = data.frame(extract(slope, samp, method="simple", sp = T)@data)
aspect.ex = data.frame(extract(aspect, samp, method="simple", sp = T)@data)
# add sine and cosine of aspect to df
aspect.ex$aspect.sin = sin(aspect.ex$aspect)
aspect.ex$aspect.cos = cos(aspect.ex$aspect)

colnames(cyclo.ex) = c("image", "cyclo")
colnames(cots.ex) = c("image", "cots")

# create df with environmental variables
env.raw = data.frame(image=cyclo.ex$image, cyclo = cyclo.ex$cyclo, cots = cots.ex$cots)
