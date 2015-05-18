# function to calculate distance between coordinates. Estimates should be ok for short distances between coordinates.
# function does not assume curvature of the earth

dist = function(lat,lon)
{
  #declare output as vector
  d = c()
  
  # iterate over the coordinate vectors and calculate distance using pythagoras
  for (i in 1:length(lat)-1)
  {
    d[i] = sqrt((lat[i+1]-lat[i])^2+(lon[i+1]-lon[i])^2)
  }
  
  return(d)
}