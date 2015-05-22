# plots transects using utm coordinates.
# must supply check (list with transect numbers)

for (j in 1:length(check))
{
  s1data = data.frame()
  s1data = aggregate(cbind(lat,lon)~image,data = subset(cdata,trans==check[j]), mean)
  coordinates(s1data) = c("lon","lat")
  
  plot(s1data)
  axis(side=c(1,2))
      
  title(main = check[j])
}