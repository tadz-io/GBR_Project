#calculate centroid per transect
trans = unique(cdata$trans)
im.count = data.frame()


for (i in 1:length(trans))
{
  im.count[i,1] = trans[i]
  im.count[i,2] = length(unique(cdata[cdata$trans == trans[i],2]))
  im.count[i,3] = unique(cdata[cdata$trans == trans[i],1])[round(im.count[i,2]/2)]
  im.count[i,4] = cdata[cdata$id==im.count[i,3],4]
  im.count[i,5] = cdata[cdata$id==im.count[i,3],5]
}



map = get_map(location = c(lon = 145.8981, lat = -15), zoom=5, scale = "auto", maptype = "hybrid")
ptrans = ggmap(map) + geom_point(aes(x=V5, y=V4, colour=factor(V1)), data=im.count)
ptrans 