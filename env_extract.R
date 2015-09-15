library("rgdal")
library("raster")
library("sp")

# import environmental data as raster
cots = raster(file.choose())
cyclo = raster(file.choose())

# set coordinates of sampling locations
samp = data.frame(image = cdata$image, lon = cdata$lon, lat = cdata$lat)
coordinates(samp) = c("lon", "lat")

# extract environmental variables for every sampling location
cyclo.ex = data.frame(extract(cyclo, samp, method="simple", sp = T)@data)
cots.ex = data.frame(extract(cots, samp, method="simple", sp = T)@data)

colnames(cyclo.ex) = c("image", "cyclo")
colnames(cots.ex) = c("image", "cots")

# create df with environmental variables
env = data.frame(image=cyclo.ex$image, cyclo = cyclo.ex$cyclo, cots = cots.ex$cots)
