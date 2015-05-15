tab = aggregate(cbind(cdata$lat,cdata$lon), by = list(cdata$image,cdata$trans), FUN = "mean")
colnames(tab ) = c("image","trans","lat","lon")

trans = unique(cdata$trans)

dist = data.frame()
or.trans = data.frame()

for (i in (2:length(tab[,1])))
  {
    dist[i,1] = tab[i,1]
    dist[i,2] = earth.dist(tab$lon[i],tab$lat[i],tab$lon[i-1],tab$lat[i-1])
  }

for (j in 1:length(trans))
  {
  or.trans[j,1] = cdata[cdata$trans==trans[j],2][1] 
  }
