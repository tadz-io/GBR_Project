#declare variable for storage histograms per transect. atm not sure if data.frame is appropriate data struct to store histograms
imdist = data.frame()

#subset the dataset per transect and calculate distance between images using dist function
for (i in 1:length(unique(cdata$trans)))
{
  sdata = subset(cdata,trans==unique(trans)[i])
  imdist[i] = hist(dist(sdata$lat,sdata$lon)) 
}