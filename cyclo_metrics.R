# cyclone data

# make list of all raster files
rfiles = list.files(path = "/Users/tadzio/Documents/UQProject/data/historical_MP/", pattern = 'hdr.adf', recursive = T, full.names = T)
len = length(rfiles)

# set extent with buffer
# note that extent() is different from bbox()
# extent() takes a vector in the order: xmin, xmax, ymin, ymax; and is of class extent
# bbox() takes a vector in the order: xmin, ymin, xmax, ymax; and is off class matrix
area = extent(SpatialPoints(cdata[,c(4,3)]))+c(-0.5,0.5,-0.5,0.5)
# init stack
cyclStack = stack()
# init projection raster
r.proj = raster(area, crs = CRS("+proj=longlat +datum=WGS84"))
# set resolution
res(r.proj) = c(0.0359, 0.0349)

# iteratively load raster files into a stack
for(i in 1:len){
  #load raster files again
  rast = raster(rfiles[i])
  # reproject
  rast = projectRaster(rast, crs = CRS("+proj=longlat +datum=WGS84"))
   # reproject raster on extended raster file using nearest neigbor interpolation
  rast = resample(rast, r.proj, method = "ngb")
  # get name of cyclone
  # assuming structure of path is ..//w4m_xxx/hdr.adf
  cname = sub(".*w4m_([a-z0-9]+)/hdr.adf", "\\1", rfiles[i])
  rast@data@names = cname
  # add layer to stack
  cyclStack = addLayer(cyclStack, rast)
}

# calculate probability of cyclone event
cycl.prob = calc(cyclStack, function(x){
  # set threshold
  # threshold 1: 1-12
  # threshold 2: >12
  th = (x>12)
  # calculate frequency; time interval: 1985-2015 -> 30 yrs
  freq = sum(th, na.rm = T)/30
  # calculate probability of occurence assuming poisson distr
  prob = 1-exp(-freq)
  return(prob)
})

# import list with dates of cyclones
cycl.data = read.csv(file.choose(), header = T, sep = ";")
cycl.data$Name = as.character(cycl.data$Name)
# change names in df so that they match names in rasterstack
cycl.data$Name[cycl.data$Name == "Nathan" & cycl.data$Year.start == 2015] = "Nathan15"
cycl.data$Name[cycl.data$Name == "Ita" & cycl.data$Year.start == 2014] = "Ita14"
# decapatilize characters to match names in rasterstack
cycl.data$Name =  apply(as.array(cycl.data$Name), MARGIN = 1, function(x){
  return(paste(tolower(substring(x, 1, 1)), substring(x, 2, nchar(x)), sep = ""))
  })

# convert end date columns to date format
cycl.data$Date.end = apply(cycl.data[,5:7], MARGIN = 1, FUN = function(x){
  return(paste(x[1],x[2],x[3], sep = "."))
  })
cycl.data$Date.end = as.Date(cycl.data$Date.end, format = "%Y.%m.%d")

# calculate time since last cyclone event
cycl.time = calc(cyclStack, function(x){
  
  # set threshold again
  th = (x>12)

  # set timepoint from which to calculate time interval to last event
  # c(year, month)
  tp = as.Date("2012.11.1", format = "%Y.%m.%d")
  
  if(any(th, na.rm = T)){
    # get cyclone names for which condition "th" is true
    # then match names with lookup table to get row id
    cycl.id =  sapply(na.omit(names(x)[th]), FUN = match, table = cycl.data$Name)
    # create var with end dates of cyclone
    cycl.dates = cycl.data$Date.end[cycl.id]
    # calculate time since most recent event
    if(any(cycl.dates<tp)){
      # continue here:
      # to calc. mean for last 10 yrs lookup dates within interval
      # then lookup cyclone intensity that match dates that are within interval
     
      return(difftime(tp, max(cycl.dates[cycl.dates<tp]), units = "days")/365)
    }
    else{
      return(NA)
    }
  }
 
 else{
   return(NA)
 }
})

cycl.mean = calc(cyclStack, function(x){
  
  th = (x>=1)
  # invoke browser
#   if(sum(th, na.rm = T)>1)
#   {browser()}
  
  if(any(th, na.rm = T)){
    
    # get id of cyclones
    cycl.id =  sapply(na.omit(names(x)[th]), FUN = match, table = cycl.data$Name)
    tp = as.Date("2012.11.1", format = "%Y.%m.%d")
    lb = as.Date("2002.11.1", format = "%Y.%m.%d")
    
    # create logical vector
    # select cyclones that happenend between lb and tp
    logi = cycl.data$Date.end[cycl.id] < tp & cycl.data$Date.end[cycl.id] >= lb
    
    if(any(logi)){
      # get names of these cyclones
      cycl.names = cycl.data$Name[cycl.id[logi]]
      # return mean values of these cyclones
      return(mean(x[cycl.names]))
    }
    
    else{
      return(NA)
    }
  }
  
  else{
    return(NA)
    }

})



