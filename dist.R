# function to calculate distance between coordinates. Estimates should be ok for short distances between coordinates.
# function does not assume curvature of the earth

dist = function(dat)
{
  library("sp")
  library("rgdal")
  
  #declare output as vector
  len = length(dat[,1])
  outl = list()
  d = data.frame(image = numeric(len),
                 dist = numeric(len))
  
  #project coordinates to utm
  coordinates(dat) = c("lon","lat")
  proj4string(dat) = CRS("+proj=longlat +datum=WGS84")
  tdata<-spTransform(dat,CRS("+proj=utm +zone=55 +datum=WGS84"))
  as(tdata,"SpatialPoints")
  
  # iterate over the utm coordinates and calculate distance using pythagoras
  for (i in 1:(len-1))
  {
    d[i,1] = tdata$image[i]
    d[i,2] = sqrt((tdata$lat[i+1]-tdata$lat[i])^2+(tdata$lon[i+1]-tdata$lon[i])^2)
    
    if (d[i,2]>100)
    {
      outl[length(outl)+1] = d[i,1]
    }
    
  }
  
  return(d)
}
