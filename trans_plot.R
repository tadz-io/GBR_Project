#libs
library("sp")

#calculate centroid per transect
trans = unique(cdata$trans)
im.count = data.frame(matrix(0, nrow=length(trans), ncol=5))
colnames(im.count) = c("trans", "no.images","lat","lon","sp.autocorr")


for (i in 1:length(trans))
{
  # transect id
  im.count[i,1] = trans[i]
  # number of images per transect
  im.count[i,2] = length(cdata[cdata$trans == trans[i],2])
  # lat. + lon. of center images in transect
  im.count[i,3] = cdata[cdata$trans == trans[i],3][round(im.count[i,2]/2)]
  im.count[i,4] = cdata[cdata$trans == trans[i],4][round(im.count[i,2]/2)]
   
  # get x-intercept for spline correlogram for every transect
  # needs output from spline_par.R script
  im.count[i,5] = res.tot[res.tot$trans==trans[i],3]
}

map = get_map(location = c(lon = 145.8981, lat = -15), zoom=8, scale = "auto", maptype = "hybrid")
ptrans = ggmap(map) + geom_point(aes(x=lon, y=lat, colour=as.factor(trans), size=sqrt(sp.autocorr)), data=im.count)
ptrans 

# create scaled diagram
xy = im.count[,3:4]
coordinates(xy) = c("lon","lat")
proj4string(xy) = CRS("+proj=longlat +datum=WGS84")
xy = spTransform(xy, CRS("+proj=utm +zone=55 +datum=WGS84"))
xy = as.data.frame(scale(xy@coords, scale=TRUE))

# plot scaled coordinates and adjust point size to sqrt of x.intercept of spline correlogram
p = ggplot(xy, aes(x=lon,y=lat))
p = p + geom_point(aes(colour=as.factor(res.tot$trans), size=sqrt(res.tot$x.intercept)))
p + xlim(.25,.5) + ylim(.5,.75)
