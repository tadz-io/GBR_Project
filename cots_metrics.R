# get list of geotifs of COTS data
cfiles = list.files(path = "/Users/tadzio/Documents/UQProject/data/COTS/", pattern = "\\.tif$",
                    recursive = T, full.names = T)
len = length(cfiles)

# # check resolution and extent of all geotifs
# r.df = data.frame(xmin = rep(NA, len),
#                   xmax = rep(NA, len),
#                   ymin = rep(NA, len),
#                   ymax = rep(NA, len),
#                   xres = rep(NA, len),
#                   yres = rep(NA, len))
# 
# for(i in 1:len){
#   rast = raster(cfiles[i])
#   r.df$xmin[i] = extent(rast)[1]
#   r.df$xmax[i] = extent(rast)[2]
#   r.df$ymin[i] = extent(rast)[3]
#   r.df$ymax[i] = extent(rast)[4]
#   r.df$xres = res(rast)[1]
#   r.df$yres = res(rast)[2]
# }

# init raster stack
cots = stack()
# init lookup table with dates
cots.data = data.frame(id = rep(NA, len),
                       date = rep(NA, len))
# iteratively load raster files into a stack
# and create lookup table with dates (years) of cots rasters
for(i in 1:len){
 
  rast = raster(cfiles[i])
  # reproject raster using new datum
  rast = projectRaster(rast, crs = CRS("+proj=longlat +datum=WGS84"))
  # add layer to stack
  cots = addLayer(cots, rast)
  
  cots.data$id[i] = rast@data@names
  cots.data$date[i] = as.numeric(substring(strsplit(rast@data@names, split = "[_]")[[1]][2], 1, 4))
}

# calculate probability of cots event
cots.prob = calc(cots, function(x){
  # set threshold
  th = (x>1)
  # calculate frequency
  freq = sum(th, na.rm = T)/len
  # calculate probability of occurence assuming poisson distr
  prob = 1-exp(-freq)
  return(prob)
})

# calculate time since last cots event
cots.time = calc(cots, function(x){
  
  # set threshold again
  th = (x>=1)

  # set timepoint from which to calculate time interval to last event
  # here we only set year since no exact date for cots data is specified
  tp = 2012
  
  if(any(th, na.rm = T)){
  
    # get id names for which condition "th" is true
    # then match names with lookup table to get row id
    cots.id =  sapply(na.omit(names(x)[th]), FUN = match, table = cots.data$id)
    # create var with end dates of cyclone
    cots.dates = cots.data$date[cots.id]
  
    # calculate time since most recent event
    if(any(cots.dates<tp)){
      
      return(tp-max(cots.dates[cots.dates<tp]))
    }
    else{
      return(NA)
    }
  }
  
  else{
    return(NA)
  }
})

#
cots.mean = calc(cots, function(x){
  
  th = (x>0)
  # invoke browser
  #   if(sum(th, na.rm = T)>1)
  #   {browser()}
  
  if(any(th, na.rm = T)){
    
    # get id of cots
    cots.id =  sapply(na.omit(names(x)[th]), FUN = match, table = cots.data$id)
    # specify time interval over which to calculate mean intensity
    tp = 2012
    lb = 2002
    
    # create logical vector
    # select cots that happenend between lb and tp
    logi = cots.data$date[cots.id] < tp & cots.data$date[cots.id] >= lb
    
    if(any(logi, na.rm = T)){
      
      # get id names of these cots events
      cots.names = cots.data$id[cots.id[logi]]
      # return mean values of these cyclones
      return(mean(x[cots.names]))
    }
    else{
   
      return(NA)
    }
 
  }
  else{
    return(NA)
  }
  
})