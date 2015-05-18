# declare variable for storage histograms per transect. atm not sure if data.frame is appropriate data struct to store histograms
imdist = data.frame()

# subset the dataset per transect and calculate distance between images using dist function
for (i in 1:length(unique(cdata$trans)))
{
  # subset the dataset by transect and aggregate subset by unique images; aggregate prevents calculation of distances
  # between coordinates of the same image
  
  sdata = aggregate(cbind(lat,lon)~image,data = subset(cdata,trans==unique(trans)[i]), mean)
  
  imdist[i] = hist(dist(sdata$lat,sdata$lon)) 
}