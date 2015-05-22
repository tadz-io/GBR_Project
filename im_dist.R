#set breaks
breaks = c(0:10,50,100,1000,10000)

#retrieve unique transect numbers
untrans = unique(cdata$trans)
#remove transects with invalid coordinate values
untrans = untrans[!untrans %in% c(10004,10005,10007,10009,14008,15011)]

#declare data frame for frequency per bin per transect
imdist = data.frame(matrix(NA, nrow=length(untrans), ncol=length(breaks)+2))

# subset the dataset per transect and calculate distance between images using dist function
for (i in 1:length(untrans))
{
  # subset the dataset by transect and aggregate subset by unique images; aggregate prevents calculation of distances
  # between coordinates of the same image  
  sdata = data.frame()
  sdata = aggregate(cbind(lat,lon)~image,data = subset(cdata,trans==untrans[i]), mean)
  
  #per transect, calculate distance between images and determine frequency using bins defined by breaks
  imdist[i,] = cbind(untrans[i], min(dist(sdata)$dist), max(dist(sdata)$dist), t(hist(dist(sdata)$dist,breaks=breaks)$counts))
}
colnames(imdist) = c("trans","min","max",breaks[2:length(breaks)])
