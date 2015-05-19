#set number of bins and breaks
bins = 500
breaks = seq(0,bins)

#retrieve unique transect numbers
untrans = unique(cdata$trans)
#remove transects with invalid coordinate values
untrans = untrans[!untrans %in% c(10007,15011)]

#declare data frame for frequency per bin per transect
imdist = data.frame(matrix(NA, nrow=length(untrans), ncol=bins+1))


# subset the dataset per transect and calculate distance between images using dist function
for (i in 1:length(untrans))
{
  # subset the dataset by transect and aggregate subset by unique images; aggregate prevents calculation of distances
  # between coordinates of the same image  
  sdata = aggregate(cbind(lat,lon)~image,data = subset(cdata,trans==untrans[i]), mean)
  
  #per transect, calculate distance between images and determine frequency using bins defined by breaks
  imdist[i,] = cbind(untrans[i], t(hist(dist(sdata)$dist,breaks=breaks)$counts))
}
